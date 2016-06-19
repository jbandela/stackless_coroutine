#pragma once

#include <atomic>
#include <memory>
#include <mutex>
#include <thread>
#include <condition_variable>


namespace detail {

  using channel_function_t = void (*)(void* node);
  struct node_base {
	  using function_t = channel_function_t;
    std::atomic<bool> *pdone = nullptr;
    node_base *next = nullptr;
    node_base *previous = nullptr;
    void *data = nullptr;
    function_t func = nullptr;
	bool closed = false;
  };



  // Assumes lock has already been taken
  inline void add(std::atomic<node_base *> &head, node_base *&tail, node_base *node) {
    if (!head.load()) {
      node->next = nullptr;
      node->previous = nullptr;
      tail = node;
      head = node;
    } else {
		assert(head != node);
      assert(tail != nullptr);
      node->next = nullptr;
      node->previous = tail;
	  tail->next = node;
      tail = node;
    }
  }

  // Assumes lock has already been taken
  inline node_base *remove_helper(std::atomic<node_base *> &head, node_base *&tail, node_base *node) {
    if (!node) {
      return nullptr;
    }

    if (node->previous) {
      node->previous->next = node->next;
    }
    if (node->next) {
      node->next->previous = node->previous;
    }

    if (head == node) {
      head = node->next;
    }
    if (tail == node) {
      tail = node->previous;
    }
	node->next = nullptr;
	node->previous = nullptr;
    return node;
  }



  template<class T>
  struct node_t:node_base {
	  T value;
  };



}

struct channel_executor {
	using node_base = detail::node_base;
 std::atomic<node_base *> head{nullptr};
  node_base *tail = nullptr;

  std::atomic<bool> closed{ false };
  std::atomic<unsigned int> running{ 0 };
  std::mutex mut;
  using lock_t = std::unique_lock<std::mutex>;
  std::condition_variable cvar;
  std::condition_variable finished_running;



  bool add(node_base* node) {
	  if (closed) return false;
	  lock_t lock{ mut };
	  detail::add(head, tail, node);
	  lock.unlock();
	  cvar.notify_all();
	  return true;
  }

  void run() {
	  auto set_false = [&](void *) {--running;finished_running.notify_all();};

	  std::unique_ptr<void, std::decay_t<decltype(set_false)>> false_setter{ &running,set_false };

	  ++running;

	  while (true) {

		  lock_t lock{ mut };
		  
		  auto node = detail::remove_helper(head, tail, head.load());
		  while (!node) {
			if (closed.load()) return;
			cvar.wait(lock);
			node = detail::remove_helper(head, tail, head.load());
		  }

		  lock.unlock();
		  node->func(node);
	  }

  }

  void close() {
	  if (closed.load()) return;
	  lock_t lock{ mut };
	  closed = true;
	  cvar.notify_all();

  }

  ~channel_executor()
  {
	  close();

	  lock_t lock{ mut };
	  while (running.load() > 0) {
		  finished_running.wait_for(lock, std::chrono::seconds{ 5 });
	  }


  }




};

namespace detail {
	struct channel_runner {
		channel_executor* executor = nullptr;
		std::thread rt;

		channel_runner(channel_executor* e) :executor{ e }, rt{ [e]() {e->run();} } {

		}

		~channel_runner() {
			executor->close();
			rt.join();
		}





	};
}

channel_executor& get_channel_executor() {
	static channel_executor e;
	static detail::channel_runner cr{ &e };
	return e;
}

template <class T> struct channel {
	using node_t = detail::node_t<T>;
	using node_base = detail::node_base;
	using function_t = detail::channel_function_t;
  std::atomic<node_base *> read_head{nullptr};
  node_base *read_tail = nullptr;
  std::atomic<node_base *> write_head{nullptr};
  node_base *write_tail = nullptr;

  std::atomic<bool> closed{ false };
  std::mutex mut;
  using lock_t = std::unique_lock<std::mutex>;

  channel_executor* executor = nullptr;


  bool write(node_t *writer) {
	  if (closed) return false;
	  lock_t lock{ mut };
	  auto reader = static_cast<node_t*>(detail::remove_helper(read_head, read_tail, read_head.load()));
	  while (reader && reader->pdone && reader->pdone->exchange(true) == true) {
		  reader = static_cast<node_t*>(detail::remove_helper(read_head, read_tail, read_head.load()));
	  }
	  if (!reader) {
		  detail::add(write_head, write_tail, writer);
	  }
	  else {
		  lock.unlock();
		  assert(reader->func);
		  assert(writer->func);
		  reader->value = std::move(writer->value);

		  reader->closed = false;
		  writer->closed = false;

		  executor->add(reader);
		  if (!writer->pdone || writer->pdone->exchange(true) == false) {
			  executor->add(writer);
		  }
	  }
	  return true;
  }
  bool read(node_t *reader) {
	  if (closed) return false;
	  lock_t lock{ mut };
	  auto writer = static_cast<node_t*>(detail::remove_helper(write_head, write_tail, write_head.load()));
	  while (writer && writer->pdone && writer->pdone->exchange(true) == true) {
		  writer = static_cast<node_t*>(detail::remove_helper(write_head, write_tail, write_head.load()));
	  }
	  if (!writer) {
		  detail::add(read_head, read_tail, reader);
	  }
	  else {
		  lock.unlock();
		  assert(writer->func);
		  T value{ std::move(writer->value) };

		  writer->closed = false;
		  reader->closed = false;

		  executor->add(writer);

		  if (!reader->pdone || reader->pdone->exchange(true) == false) {
			  executor->add(reader);
		  }
	  }
	  return true;
  }

  void remove_reader(node_t* reader) {
	  lock_t lock{ mut };
	  detail::remove_helper(read_head, read_tail, reader);
  }

  void remove_writer(node_t* writer) {
	  lock_t lock{ mut };
	  detail::remove_helper(write_head, write_tail, writer);
  }

  void close() {
	  closed = true;
	  lock_t lock{ mut };

	  auto writers = write_head.load();

	  write_head = nullptr;
	  write_tail = nullptr;

	  auto readers = read_head.load();

	  read_head = nullptr;
	  read_tail = nullptr;

	  lock.unlock();

	  // Clear out all the writers
	  for (auto n = static_cast<node_t*>(writers);n != nullptr;) {
		  auto old = n;
		  n = static_cast<node_t*>(n->next);

		  old->closed = true;
		  if (!old->pdone || old->pdone->exchange(true) == false) {
			  executor->add(old);
		  }
	  }

	  // Clear out all the readers
	  for (auto n = static_cast<node_t*>(readers);n != nullptr; ) {
		  auto old = n;
		  n = static_cast<node_t*>(n->next);
		  old->closed = true;
		  if (!old->pdone || old->pdone->exchange(true) == false) {
			  executor->add(old);
		  }
	  }


  }

  channel(channel_executor* e = &get_channel_executor()) :executor{ e } {}


};

namespace detail {
	struct void_holder {
		void* value;
	};
}

template <class T,class PtrType = std::shared_ptr<channel<T>>> struct channel_reader {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	using value_type = T;
	PtrType ptr;

	auto get_node() const {
		return &node;
	}
	template<class Context>
	bool read(Context context) {

		node.func = [](void* n) {

		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->value,node->closed);
		};
		node.data = &context.f().value();
		return ptr->read(&node);
	}
	template<class Select,class Context>
	bool read(Select& s,Context context) {

		node.func = [](void* n) {

		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, detail::void_holder{ &node->value }, node->closed);
		};
		node.data = &context.f().value();
		node.pdone = &s.done;
		return ptr->read(&node);
	}
	void remove() {
		ptr->remove_reader(&node);
	}



	explicit channel_reader(PtrType p) :ptr{ p } {}
	~channel_reader() {
		ptr->remove_reader(&node);
	}



};
template <class T,class PtrType = std::shared_ptr<channel<T>>> struct channel_writer {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	PtrType ptr;

	auto get_node() const {
		return &node;
	}
	template<class Context>
	bool write(T t, Context context) {
		node.value = std::move(t);

	node.func = [](void* n) {
		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->closed);
		};
	
		node.data = &context.f().value();
		return ptr->write(&node);
	}
	template<class Select,class Context>
	bool write(T t,Select& s, Context context) {
		node.value = std::move(t);

	node.func = [](void* n) {
		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->closed);
		};
	
		node.data = &context.f().value();
		node.pdone = &s.done;
		return ptr->write(&node);
	}

	void remove() {
		ptr->remove_writer(&node);
	}

	explicit channel_writer(PtrType p) :ptr{ p } {}

	~channel_writer() {
		ptr->remove_writer(&node);
	}

};


struct channel_selector {
	std::atomic<bool> done{ false };

	struct select_helper_reader {
		channel_selector* sel = nullptr;
		detail::node_base* channel = nullptr;
		detail::void_holder value;
		bool success = false;
		select_helper_reader(channel_selector* s, detail::node_base* channel, detail::void_holder value) :sel{ s }, channel{ channel }, value{ value } {}
		template<class ChannelReader, class F>
		select_helper_reader& select(ChannelReader& c, F&& f) {
			c.remove();
			if (channel == c.get_node()) {
				using value_type = typename ChannelReader::value_type;
				success = true;
				f(*static_cast<value_type*>(value.value));
			}
			return *this;


		}

		template<class Range, class F>
		select_helper_reader& select_range(Range& r, F&& f) {
			for (auto& c : r) {
				c.remove();
				if (channel == c.get_node()) {
					using ChannelReader = std::decay_t<decltype(c)>;
					using value_type = typename ChannelReader::value_type;
					success = true;
					f(*static_cast<value_type*>(value.value));
				}
			}
			return *this;


		}


		explicit operator bool()const { return success; }

		~select_helper_reader(){
			sel->done = false;
		}


	};
	struct select_helper_writer {
		channel_selector* sel = nullptr;
		detail::node_base* channel = nullptr;
		bool success = false;
		select_helper_writer(channel_selector* s, detail::node_base* channel) :sel{ s }, channel{ channel }{}
		template<class ChannelReader, class F>
		select_helper_writer& select(ChannelReader& c, F&& f) {
			c.remove();
			if (channel == c.get_node()) {
				success = true;
				f();
			}
			return *this;


		}
		template<class Range, class F>
		select_helper_writer& select_range(Range& r, F&& f) {
			for (auto& c : r) {
				c.remove();
				if (channel == c.get_node()) {
					success = true;
					f();
				}
			}
			return *this;

		}


		~select_helper_writer() {
			sel->done = false;
		}

		explicit operator bool()const { return success; }

	};

	auto get_selector(detail::node_base* channel) {
		return select_helper_writer{this, channel };
	}
	auto get_selector(detail::node_base* channel, detail::void_holder value) {
		return select_helper_reader{this, channel,value };
	}





};
