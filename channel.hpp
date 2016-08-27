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
    std::atomic<int> *pdone = nullptr;
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
  inline void add_head(std::atomic<node_base *> &head, node_base *&tail, node_base *node) {
	  if (!head.load()) {
		  node->next = nullptr;
		  node->previous = nullptr;
		  tail = node;
		  head = node;
	  }
	  else {
		  assert(head != node);
		  assert(tail != nullptr);
		  node->next = head;
		  node->previous = nullptr;
		  head.load()->previous = node;
		  head = node;
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

	// 0 = not done, 1 = done, -1 = indeterminate
	static bool set_done(std::atomic<int>* pdone, int expected, int value) {
		if (!pdone) return true;

		while (true) {
			int exp = expected;
			if (pdone->compare_exchange_strong(exp, value)) {
				return true;
			}
			if (exp == 1) {
				// somebody else set it to done, we will never succeed
				return false;
			}
			// it is indeterminate - keep looping;
			assert(exp == -1);
		}

	}


  static bool set_intermediate(std::atomic<int>* pdone) {
	  return set_done(pdone, 0, -1);
  }
  static bool clear_intermediate_success(std::atomic<int>* pdone) {
	  return set_done(pdone, -1, 1);
  }
  static bool clear_intermediate_failure(std::atomic<int>* pdone) {
	  return set_done(pdone, -1, 0);
  }
  static bool set_success(std::atomic<int>* pdone) {
	  return set_done(pdone, 0, 1);
  }






  void write(node_t *writer) {
	  if (closed) {
		  if (set_success(writer->pdone)) {
			  writer->closed = true;
			  executor->add(writer);
		  }
	  }

	  lock_t lock{ mut };
	  auto reader = static_cast<node_t*>(detail::remove_helper(read_head, read_tail, read_head.load()));
	  while (reader && !set_intermediate(reader->pdone)) {
		  reader = static_cast<node_t*>(detail::remove_helper(read_head, read_tail, read_head.load()));
	  }
	  if (!reader) {
		  detail::add(write_head, write_tail, writer);
	  }
	  else {
		  if (!set_success(writer->pdone)) {
			  detail::add_head(read_head, read_tail, reader);
			  clear_intermediate_failure(reader->pdone);
			  return ;
		  }
		  clear_intermediate_success(reader->pdone);
		  lock.unlock();
		  assert(reader->func);
		  assert(writer->func);
		  reader->value = std::move(writer->value);

		  reader->closed = false;
		  writer->closed = false;

		  executor->add(reader);
		  executor->add(writer);
	  }
  }
  void read(node_t *reader) {
	  if (closed) {
		  if (set_success(reader->pdone)) {
			  reader->closed = true;
			  executor->add(reader);
		  }
	  }
	  lock_t lock{ mut };
	  auto writer = static_cast<node_t*>(detail::remove_helper(write_head, write_tail, write_head.load()));
	  while (writer && !set_intermediate(writer->pdone)) {
		  writer = static_cast<node_t*>(detail::remove_helper(write_head, write_tail, write_head.load()));
	  }
	  if (!writer) {
		  detail::add(read_head, read_tail, reader);
	  }
	  else {
		  if (!set_success(reader->pdone)) {
			  detail::add_head(write_head, write_tail, writer);
			  clear_intermediate_failure(writer->pdone);
			  return ;
		  }
		  clear_intermediate_success(writer->pdone);
		  lock.unlock();
		  assert(writer->func);
		  reader->value = std::move(writer->value);

		  writer->closed = false;
		  reader->closed = false;

		  executor->add(writer);

		  executor->add(reader);
	  }
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
		  if (set_success(old->pdone)) {
			  executor->add(old);
		  }
	  }

	  // Clear out all the readers
	  for (auto n = static_cast<node_t*>(readers);n != nullptr; ) {
		  auto old = n;
		  n = static_cast<node_t*>(n->next);
		  old->closed = true;
		  if (set_success(old->pdone)) {
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
	void read(Context context) {

		node.func = [](void* n) {

		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->value,node->closed);
		};
		node.data = &context.f().value();
		ptr->read(&node);
	}
	template<class Select,class Context>
	void read(Select& s,Context context) {

		node.func = [](void* n) {

		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, detail::void_holder{ &node->value }, node->closed);
		};
		node.data = &context.f().value();
		node.pdone = &s.done;
		ptr->read(&node);
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
	void write(T t, Context context) {
		node.value = std::move(t);

	node.func = [](void* n) {
		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->closed);
		};
	
		node.data = &context.f().value();
		ptr->write(&node);
	}
	template<class Select,class Context>
	void write(T t,Select& s, Context context) {
		node.value = std::move(t);

	node.func = [](void* n) {
		auto node = static_cast<node_t*>(n);
			auto context = Context::get_context(node->data);
			context(node, node->closed);
		};
	
		node.data = &context.f().value();
		node.pdone = &s.done;
		ptr->write(&node);
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
	std::atomic<int> done{ 0 };

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
			sel->done = 0;
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
			sel->done = 0;
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

#ifdef _MSC_VER
#include <experimental/coroutine>
#include <tuple>
struct goroutine{
struct promise_type
{
	bool initial_suspend() {
		return false;
	}
	bool final_suspend() {
		return false;
	}
	void return_void() {}
	void set_exception(std::exception_ptr) {}
	goroutine get_return_object() {
		return{};
	}
};
};

template <class T, class PtrType = std::shared_ptr<channel<T>>> struct await_channel_reader {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	using value_type = T;
	PtrType ptr;

	auto get_node() const {
		return &node;
	}
	auto read() {
		struct awaiter {
			await_channel_reader* pthis;

			bool await_ready() {
				return false;
			}
			void await_suspend(std::experimental::coroutine_handle<> rh) {
				auto& node = pthis->node;
				node.func = [](void* n) {
					auto node = static_cast<node_t*>(n);
					auto rh = std::experimental::coroutine_handle<>::from_address(node->data);
					rh();
				};
				node.data = rh.to_address();;
				pthis->ptr->read(&pthis->node);

			}
			auto await_resume() {
				if (pthis) {
					return std::make_pair(!pthis->node.closed, std::move(pthis->node.value));
				}
				else {
					return std::make_pair(false, std::move(pthis->node.value));
				}
			}
		};
		return awaiter{ this };

	}
	template<class Select, class Awaiter>
	void read(Select& s, std::experimental::coroutine_handle<> rh, Awaiter& a) {
		a.prh = rh.to_address();
		node.func = [](void* n) {
			auto node = static_cast<node_t*>(n);
			auto pa = static_cast<Awaiter*>(node->data);
			auto rh = std::experimental::coroutine_handle<>::from_address(pa->prh);
			pa->selected_node = node;
			rh();
		};
		node.data = &a;


		node.pdone = &s.done;
		ptr->read(&node);
	}
	void remove() {
		ptr->remove_reader(&node);
	}



	explicit await_channel_reader(PtrType p) :ptr{ p } {}
	~await_channel_reader() {
		ptr->remove_reader(&node);
	}



};

namespace detail {
	// Apply adapted from http://isocpp.org/files/papers/N3915.pdf
		template <typename F, typename Tuple, size_t... I>
		decltype(auto) apply_impl(F&& f, Tuple&& t, std::integer_sequence<size_t, I...>) {
			using namespace std;
			return std::forward<F>(f)(get<I>(std::forward<Tuple>(t))...);
		}
		template <typename F, typename Tuple>
		decltype(auto) apply(F&& f, Tuple&& t) {
			using namespace std;
			using Indices = make_index_sequence<tuple_size<decay_t<Tuple>>::value>;
			return apply_impl(std::forward<F>(f), std::forward<Tuple>(t), Indices{});
		}



	template<class T>
	T get_await_select_type(T t) {
		return t;
	}

	template<class T, class Ptr>
	await_channel_reader<T, Ptr>* get_await_select_type(await_channel_reader<T, Ptr>& t) {
		return &t;
	}

	

	template<class... T>
	auto get_fd_tuple(T&&... t) {
		return std::make_tuple(get_await_select_type(t)...);
	}


		template<class This,class Reader, class Func>
		void do_read(This* pthis,std::experimental::coroutine_handle<>& rh,Reader r, Func& f) {
			r->read(*pthis->s, rh,*pthis);
		}


		template<class This,class Reader, class Func, class R1, class F1,class... T>
		void do_read(This* pthis,std::experimental::coroutine_handle<>& rh,Reader r, Func& f, R1 r1, F1& f1, T&&... t) {
			r->read(*pthis->s, rh,*pthis);
			do_read(pthis,rh, r1, f1, std::forward<T>(t)...);
		}
		template<class This,class Reader, class Func>
		void do_wake(This* pthis,Reader r, Func& f) {
			r->remove();
			if (r->get_node() == pthis->selected_node) {
				f(r->get_node()->closed, r->get_node()->value);
			}
		}


		template<class This,class Reader, class Func, class R1, class F1,class... T>
		void do_wake(This* pthis,Reader r, Func& f, R1 r1, F1& f1, T&&... t) {
			r->remove();
			if (r->get_node() == pthis->selected_node) {
				f(r->get_node()->closed, r->get_node()->value);
			}
			do_wake(pthis,r1, f1, std::forward<T>(t)...);
		}



}

template<class... T>
auto read_select(channel_selector& s, T&&... t){
	auto tup = detail::get_fd_tuple(std::forward<T>(t)...);
	using Tuple = std::decay_t<decltype(tup)>;
	struct awaiter {
		Tuple t;
		channel_selector* s = nullptr;
		void* prh = nullptr;
		detail::node_base* selected_node = nullptr;

		awaiter(Tuple t, channel_selector* s) :t{ std::move(t) }, s{ s } {}

		bool await_ready() {
			return false;
		}
		void await_suspend(std::experimental::coroutine_handle<> rh) {
			
			detail::apply([this,&rh](auto&&... ts) {
				detail::do_read(this,rh, std::forward<decltype(ts)>(ts)...);
			},t);
		}
		auto await_resume() {
			detail::apply([this](auto&&... ts) {
				detail::do_wake(this,std::forward<decltype(ts)>(ts)...);
			},t);
		}
	};


	return awaiter{std::move(tup),&s};
}

template <class T, class PtrType = std::shared_ptr<channel<T>>> struct await_channel_writer {

	using node_t = typename channel<T>::node_t;
	node_t node{};

	PtrType ptr;

	auto get_node() const {
		return &node;
	}
	auto write(T t) {
		node.value = std::move(t);
		struct awaiter {
			await_channel_writer* pthis;

			bool await_ready() {
				return false;
			}
			void await_suspend(std::experimental::coroutine_handle<> rh) {
				pthis->node.func = [](void* n) {
					auto node = static_cast<node_t*>(n);
					auto rh = std::experimental::coroutine_handle<>::from_address(node->data);
					rh();
				};
				pthis->node.data = rh.to_address();;
				pthis->ptr->write(&pthis->node);
			}
			auto await_resume() {
				if (pthis) {
					return pthis->node.closed;
				}
				else {
					return false;
				}
			}
		};
		return awaiter{ this };

	}
	//template<class Select, class Context>
	//bool write(T t, Select& s, Context context) {
	//	node.value = std::move(t);

	//	node.func = [](void* n) {
	//		auto node = static_cast<node_t*>(n);
	//		auto context = Context::get_context(node->data);
	//		context(node, node->closed);
	//	};

	//	node.data = &context.f().value();
	//	node.pdone = &s.done;
	//	return ptr->write(&node);
	//}

	void remove() {
		ptr->remove_writer(&node);
	}

	explicit await_channel_writer(PtrType p) :ptr{ p } {}

	~await_channel_writer() {
		ptr->remove_writer(&node);
	}

};



#endif // _MSC_VER