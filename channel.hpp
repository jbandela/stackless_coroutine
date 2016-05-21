#pragma once

#include <atomic>
#include <memory>
#include <mutex>


namespace detail {

  using channel_function_t = void (*)(void* node);
  struct node_base {
	  using function_t = channel_function_t;
    std::atomic<bool> *pdone = nullptr;
    node_base *next = nullptr;
    node_base *previous = nullptr;
    void *data = nullptr;
    function_t func = nullptr;
    std::atomic<void *> channel{nullptr};
  };



  // Assumes lock has already been taken
  inline void add(std::atomic<node_base *> &head, node_base *&tail, node_base *node) {
    if (!head.load()) {
      node->next = nullptr;
      node->previous = nullptr;
      tail = node;
      head = node;
    } else {
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
	node->channel = nullptr;
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
  std::atomic<bool> running{ false };
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
	  auto set_false = [&](void *) {running = false;finished_running.notify_all();};

	  std::unique_ptr<void, std::decay_t<decltype(set_false)>> false_setter{ &running,set_false };

	  {
		  lock_t lock{ mut };
		  running = true;
	  }

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
	  while (running) {
		  finished_running.wait(lock);
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
	//static detail::channel_runner cr{ &e };
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
	  if (reader && reader->pdone && reader->pdone->exchange(true) == true) reader = nullptr;
	  if (!reader) {
		  detail::add(write_head, write_tail, writer);
	  }
	  else {
		  lock.unlock();
		  reader->channel = nullptr;
		  assert(reader->func);
		  assert(writer->func);
		  reader->value = std::move(writer->value);

		  executor->add(reader);
		  executor->add(writer);
	  }
	  return true;
  }
  bool read(node_t *reader) {
	  if (closed) return false;
	  lock_t lock{ mut };
	  auto writer = static_cast<node_t*>(detail::remove_helper(write_head, write_tail, write_head.load()));
	  if (writer && writer->pdone && writer->pdone->exchange(true) == true) writer = nullptr;
	  if (!writer) {
		  reader->channel = this;
		  detail::add(read_head, read_tail, reader);
	  }
	  else {
		  lock.unlock();
		  writer->channel = nullptr;
		  assert(writer->func);
		  T value{ std::move(writer->value) };
		  executor->add(writer);
		  executor->add(reader);
	  }
	  return true;
  }

  void remove_reader(node_t* reader) {
	  lock_t lock{ mut };
	  if (reader->channel != this) return;
	  reader->channel = nullptr;
	  detail::remove_helper(read_head, read_tail, reader);
  }

  void remove_writer(node_t* writer) {
	  lock_t lock{ mut };
	  if (writer->channel != this) return;
	  writer->channel = nullptr;
	  detail::remove_helper(write_head, write_tail, writer);
  }

  void close() {
	  closed = true;
	  lock_t lock{ mut };

	  auto writers = write_head;

	  write_head = nullptr;
	  write_tail = nullptr;

	  auto readers = read_head;

	  read_head = nullptr;
	  read_tail = nullptr;

	  lock.unlock();

	  // Clear out all the writers
	  for (auto n = static_cast<node_t*>(writers);n != nullptr; n = static_cast<node_t*>(n->next)) {
		  n->channel = nullptr;
		  executor->add(n);
	  }

	  // Clear out all the readers
	  for (auto n = static_cast<node_t*>(readers);n != nullptr; n = static_cast<node_t*>(n->next)) {
		  n->channel = nullptr;
		  executor->add(n);
	  }


  }

  channel(channel_executor* e = &get_channel_executor()) :executor{ e } {}


};

template <class T> struct channel_reader {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	std::shared_ptr<channel<T>> ptr;

	void* get_channel() {
		return ptr->get();
	}
	template<class Context>
	bool read(Context context) {

		node.func = [](void* n) {

		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node->channel, node->value);
		};
		node.data = &context.f().value();
		return ptr->read(&node);
	}

	explicit channel_reader(std::shared_ptr<channel<T>> p) :ptr{ p } {}
	~channel_reader() {
		ptr->remove_reader(&node);
	}



};
template <class T> struct channel_writer {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	std::shared_ptr<channel<T>> ptr;

	void* get_channel() {
		return ptr->get();
	}


	template<class Context>
	bool write(T t, Context context) {
		node.value = std::move(t);

	node.func = [](void* n) {
		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node->channel, node->value);
		};
	
		node.data = &context.f().value();
		return ptr->write(&node);
	}

	explicit channel_writer(std::shared_ptr<channel<T>> p) :ptr{ p } {}

	~channel_writer() {
		ptr->remove_writer(&node);
	}

};


struct channel_selector {
	std::atomic<bool> done{ false };

};
