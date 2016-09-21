#pragma once

#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <thread>

namespace detail {

struct node_base {
  using function_t = void (*)(node_base *node);
  std::atomic<int> *pdone = nullptr;
  node_base *next = nullptr;
  node_base *previous = nullptr;
  void *data = nullptr;
  function_t func = nullptr;
  bool closed = false;
  bool success = false;
};

using channel_function_t = void (*)(node_base *node);

// Assumes lock has already been taken
inline void add(node_base *&head, node_base *&tail, node_base *node) {
  if (!head) {
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
inline void add_head(node_base *&head, node_base *&tail, node_base *node) {
  if (!head) {
    node->next = nullptr;
    node->previous = nullptr;
    tail = node;
    head = node;
  } else {
    assert(head != node);
    assert(tail != nullptr);
    node->next = head;
    node->previous = nullptr;
    head->previous = node;
    head = node;
  }
}

// Assumes lock has already been taken
inline node_base *remove(node_base *&head, node_base *&tail, node_base *node) {
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

template <class T> struct node_t : node_base { T value; };
}

struct channel_executor {
  using node_base = detail::node_base;
  node_base *head{nullptr};
  node_base *tail = nullptr;

  std::atomic<bool> closed{false};
  std::atomic<unsigned int> running{0};
  std::mutex mut;
  using lock_t = std::unique_lock<std::mutex>;
  std::condition_variable cvar;
  std::condition_variable finished_running;

  bool add(node_base *node) {
    if (closed)
      return false;
    lock_t lock{mut};
    detail::add(head, tail, node);
    lock.unlock();
    cvar.notify_all();
    return true;
  }

  void run() {
    auto set_false = [&](void *) {
      --running;
      finished_running.notify_all();
    };

    std::unique_ptr<void, std::decay_t<decltype(set_false)>> false_setter{
        &running, set_false};

    ++running;

    while (true) {

      lock_t lock{mut};

      auto node = detail::remove(head, tail, head);
      while (!node) {
        if (closed.load())
          return;
        cvar.wait(lock);
        node = detail::remove(head, tail, head);
      }

      lock.unlock();
      node->func(node);
    }
  }

  void close() {
    if (closed.load())
      return;
    lock_t lock{mut};
    closed = true;
    cvar.notify_all();
  }

  ~channel_executor() {
    close();

    lock_t lock{mut};
    while (running.load() > 0) {
      finished_running.wait_for(lock, std::chrono::seconds{5});
    }
  }
};

namespace detail {
struct channel_runner {
  channel_executor *executor = nullptr;
  std::thread rt;

  channel_runner(channel_executor *e) : executor{e}, rt{[e]() { e->run(); }} {}

  ~channel_runner() {
    executor->close();
    rt.join();
  }
};
}

namespace detail {

template <class T> struct fixed_queue {
  fixed_queue(int capacity) : vec(capacity) {}
  bool empty() const { return vec.empty() || count == 0; }
  bool full() const { return vec.empty() || count == vec.size(); }
  T pop() {
    T ret = std::move(vec[read]);
    read = (read + 1) % vec.size();
    --count;
    return ret;
  }
  void push(T t) {
    vec[write] = std::move(t);
    write = (write + 1) % vec.size();
    ++count;
  }

private:
  std::vector<T> vec;
  int read = 0;
  int write = 0;
  int count = 0;
};
}

inline channel_executor &get_channel_executor() {
  static channel_executor e;
  static detail::channel_runner cr{&e};
  return e;
}

template <class T> struct channel {
  using node_t = detail::node_t<T>;
  using node_base = detail::node_base;
  using function_t = detail::channel_function_t;
  void write(node_t *writer) {
    if (closed) {
      if (set_success(writer->pdone)) {
        writer->closed = true;
        writer->success = false;
        executor->add(writer);
      }
    }

    lock_t lock{mut};

    auto reader =
        static_cast<node_t *>(detail::remove(read_head, read_tail, read_head));
    while (reader && !set_intermediate(reader->pdone)) {
      reader = static_cast<node_t *>(
          detail::remove(read_head, read_tail, read_head));
    }
    if (!reader) {
      if (!queue.full()) {
        if (set_success(writer->pdone)) {
          queue.push(std::move(writer->value));
          lock.unlock();
          writer->closed = false;
          writer->success = true;
          executor->add(writer);
          return;
        }
      }
      detail::add(write_head, write_tail, writer);
    } else {
      if ((writer->pdone && writer->pdone != reader->pdone) &&
          !set_success(writer->pdone)) {
        detail::add_head(read_head, read_tail, reader);
        clear_intermediate_failure(reader->pdone);
        return;
      }
      clear_intermediate_success(reader->pdone);
      lock.unlock();
      assert(reader->func);
      assert(writer->func);
      reader->value = std::move(writer->value);

      reader->closed = false;
      writer->closed = false;

      reader->success = true;
      writer->success = true;

      executor->add(reader);

      if (writer->pdone == nullptr || writer->pdone != reader->pdone)
        executor->add(writer);
    }
  }
  void read(node_t *reader) {
    lock_t lock{mut};
    if (!queue.empty()) {
      if (set_success(reader->pdone)) {
        reader->value = std::move(queue.pop());
        lock.unlock();
        reader->closed = false;

        reader->success = true;
        executor->add(reader);
        return;
      }
      return;
    }
    if (closed && write_head == nullptr) {
      lock.unlock();
      if (set_success(reader->pdone)) {
        reader->closed = true;
        reader->success = false;
        executor->add(reader);
      }
      return;
    }

    auto writer = static_cast<node_t *>(
        detail::remove(write_head, write_tail, write_head));
    while (writer && !set_intermediate(writer->pdone)) {
      writer = static_cast<node_t *>(
          detail::remove(write_head, write_tail, write_head));
    }
    if (!writer) {
      detail::add(read_head, read_tail, reader);
    } else {
      if ((reader->pdone && writer->pdone != reader->pdone) &&
          !set_success(reader->pdone)) {
        detail::add_head(write_head, write_tail, writer);
        clear_intermediate_failure(writer->pdone);
        return;
      }
      clear_intermediate_success(writer->pdone);
      lock.unlock();
      assert(writer->func);
      reader->value = std::move(writer->value);

      writer->closed = false;
      reader->closed = false;

      writer->success = true;
      reader->success = true;

      executor->add(writer);

      if (reader->pdone == nullptr || writer->pdone != reader->pdone)
        executor->add(reader);
    }
  }

  void remove_reader(node_t *reader) {
    lock_t lock{mut};
    detail::remove(read_head, read_tail, reader);
  }

  void remove_writer(node_t *writer) {
    lock_t lock{mut};
    detail::remove(write_head, write_tail, writer);
  }

  void close() {
    closed = true;
    lock_t lock{mut};

    auto writers = write_head;

    write_head = nullptr;
    write_tail = nullptr;

    auto readers = read_head;

    read_head = nullptr;
    read_tail = nullptr;

    lock.unlock();

    // Clear out all the writers
    for (auto n = writers; n != nullptr;) {
      auto old = n;
      n = n->next;

      old->closed = true;
      if (set_success(old->pdone)) {
        old->success = false;
        executor->add(old);
      }
    }

    // Clear out all the readers
    for (auto n = readers; n != nullptr;) {
      auto old = n;
      n = n->next;
      old->closed = true;
      if (set_success(old->pdone)) {
        old->success = false;
        executor->add(old);
      }
    }
  }

  channel(int buffer = 0, channel_executor *e = &get_channel_executor())
      : executor{e}, queue{buffer} {}

  bool push(T t) {

    lock_t lock{mut};
    if (!queue.full() && read_head == nullptr) {
      queue.push(std::move(t));
      return true;
    } else {
      return false;
    }
  }

private:
  node_base *read_head = nullptr;
  node_base *read_tail = nullptr;
  node_base *write_head = nullptr;
  node_base *write_tail = nullptr;

  std::atomic<bool> closed{false};
  std::mutex mut;
  using lock_t = std::unique_lock<std::mutex>;

  channel_executor *executor = nullptr;

  detail::fixed_queue<T> queue;

  // 0 = not done, 1 = done, -1 = indeterminate
  static bool set_done(std::atomic<int> *pdone, int expected, int value) {
    if (!pdone)
      return true;

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

  static bool set_intermediate(std::atomic<int> *pdone) {
    return set_done(pdone, 0, -1);
  }
  static bool clear_intermediate_success(std::atomic<int> *pdone) {
    return set_done(pdone, -1, 1);
  }
  static bool clear_intermediate_failure(std::atomic<int> *pdone) {
    return set_done(pdone, -1, 0);
  }
  static bool set_success(std::atomic<int> *pdone) {
    return set_done(pdone, 0, 1);
  }
};

namespace detail {
struct void_holder {
  void *value;
};

struct reader_type {};
struct writer_type {};
}

template <class T, class PtrType = std::shared_ptr<channel<T>>>
struct channel_reader {

  using node_t = typename channel<T>::node_t;
  node_t node{};

  static auto get_role() { return detail::reader_type{}; }

  using value_type = T;

  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  template <class Context> void read(Context context) {

    node.func = [](detail::node_base *n) {

      auto node = static_cast<node_t *>(n);
      auto context = Context::get_context(node->data);
      context(node, node->value, node->closed);
    };
    node.data = &context.f().value();
    node.pdone = nullptr;
    ptr->read(&node);
  }
  template <class Select, class Context> void read(Select &s, Context context) {

    node.func = [](detail::node_base *n) {

      auto node = static_cast<node_t *>(n);

      auto context = Context::get_context(node->data);
      context(node, detail::void_holder{&node->value}, node->closed);
    };
    node.data = &context.f().value();
    node.success = false;
    node.pdone = &s.done;
    ptr->read(&node);
  }
  void remove() { ptr->remove_reader(&node); }

  void close() { ptr->close(); }

  explicit channel_reader(PtrType p) : ptr{p} {}
  ~channel_reader() { ptr->remove_reader(&node); }

private:
  PtrType ptr;
};
template <class T, class PtrType = std::shared_ptr<channel<T>>>
struct channel_writer {

  using node_t = typename channel<T>::node_t;
  node_t node{};

  using value_type = T;
  static auto get_role() { return detail::writer_type{}; }

  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  template <class Context> void write(T t, Context context) {
    node.value = std::move(t);

    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);

      auto context = Context::get_context(node->data);
      context(node, node->closed);
    };

    node.data = &context.f().value();
    node.pdone = nullptr;
    ptr->write(&node);
  }
  template <class Select, class Context>
  void write(Select &s, T t, Context context) {
    node.value = std::move(t);

    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);

      auto context = Context::get_context(node->data);
      context(node, detail::void_holder{&node->value}, node->closed);
    };

    node.data = &context.f().value();
    node.pdone = &s.done;
    node.success = false;
    ptr->write(&node);
  }

  void remove() { ptr->remove_writer(&node); }

  explicit channel_writer(PtrType p) : ptr{p} {}

  void close() { ptr->close(); }

  ~channel_writer() { ptr->remove_writer(&node); }

private:
  PtrType ptr;
};

namespace detail {
template <class F, class Value>
void do_call(F &&f, Value *value, detail::reader_type) {
  f(*value);
}
template <class F, class Value>
void do_call(F &&f, Value *, detail::writer_type) {
  f();
}
}

struct channel_selector {
  std::atomic<int> done{0};

  struct select_helper {
    channel_selector *sel = nullptr;
    detail::node_base *channel = nullptr;
    detail::void_holder value;
    bool success = false;
    bool closed;
    select_helper(channel_selector *s, detail::node_base *channel,
                  detail::void_holder value, bool closed)
        : sel{s}, channel{channel}, value{value}, closed{closed} {}

    template <class ChannelReader, class F>
    select_helper &select(ChannelReader &c, F &&f) {
      c.remove();
      auto ns = c.get_node()->success;
      c.get_node()->success = false;
      if (ns) {
        using value_type = typename ChannelReader::value_type;
        success = true;
        detail::do_call(f, static_cast<value_type *>(value.value),
                        ChannelReader::get_role());
      }
      return *this;
    }

    template <class Range, class F>
    select_helper &select_range(Range &r, F &&f) {
      for (auto &c : r) {
        c.remove();

        auto ns = c.get_node()->success;

        c.get_node()->success = false;
        if (ns) {
          using ChannelReader = std::decay_t<decltype(c)>;
          using value_type = typename ChannelReader::value_type;
          success = true;
          detail::do_call(f, static_cast<value_type *>(value.value),
                          ChannelReader::get_role());
        }
      }
      return *this;
    }

    explicit operator bool() const { return success; }

    ~select_helper() { sel->done = 0; }
  };
  auto get_selector(detail::node_base *channel, detail::void_holder value,
                    bool closed) {
    return select_helper{this, channel, value, closed};
  }
};

#ifdef _MSC_VER
#include <experimental/coroutine>
#include <tuple>
struct goroutine {
  struct promise_type {
    bool initial_suspend() { return false; }
    bool final_suspend() { return false; }
    void return_void() {}
    void set_exception(std::exception_ptr) {}
    goroutine get_return_object() { return {}; }
  };
};

template <class T, class PtrType = std::shared_ptr<channel<T>>>
struct await_channel_reader {

  using node_t = typename channel<T>::node_t;
  node_t node{};

  static auto get_role() { return detail::reader_type{}; }

  using value_type = T;
  PtrType ptr;

  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  auto read() {
    struct awaiter {
      await_channel_reader *pthis;

      bool await_ready() { return false; }
      void await_suspend(std::experimental::coroutine_handle<> rh) {
        auto &node = pthis->node;
        node.func = [](detail::node_base *n) {
          auto node = static_cast<node_t *>(n);

          auto rh =
              std::experimental::coroutine_handle<>::from_address(node->data);
          rh();
        };
        node.data = rh.to_address();
        node.pdone = nullptr;
        pthis->ptr->read(&pthis->node);
      }
      auto await_resume() {
        return std::make_pair(!pthis->node.closed,
                              std::move(pthis->node.value));
      }
    };
    return awaiter{this};
  }
  template <class Select, class Awaiter>
  void read(Select &s, std::experimental::coroutine_handle<> rh, Awaiter &a) {
    a.prh = rh.to_address();
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);

      auto pa = static_cast<Awaiter *>(node->data);
      std::unique_lock<std::mutex> lock{pa->mut};
      lock.unlock();
      auto rh = std::experimental::coroutine_handle<>::from_address(pa->prh);
      pa->selected_node = node;
      rh();
    };
    node.data = &a;
    node.success = false;

    node.pdone = &s.done;
    ptr->read(&node);
  }
  void remove() { ptr->remove_reader(&node); }

  explicit await_channel_reader(PtrType p) : ptr{p} {}
  ~await_channel_reader() { ptr->remove_reader(&node); }
};

template <class T, class PtrType = std::shared_ptr<channel<T>>>
struct await_channel_writer {

  static auto get_role() { return detail::writer_type{}; }

  using node_t = typename channel<T>::node_t;
  node_t node{};

  PtrType ptr;

  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  auto write(T t) {
    node.value = std::move(t);
    struct awaiter {
      await_channel_writer *pthis;

      bool await_ready() { return false; }
      void await_suspend(std::experimental::coroutine_handle<> rh) {
        pthis->node.func = [](detail::node_base *n) {
          auto node = static_cast<node_t *>(n);
          auto rh =
              std::experimental::coroutine_handle<>::from_address(node->data);
          rh();
        };
        pthis->node.data = rh.to_address();
        pthis->node.pdone = nullptr;
        pthis->ptr->write(&pthis->node);
      }
      auto await_resume() { return !pthis->node.closed; }
    };
    return awaiter{this};
  }
  template <class Select, class T, class Awaiter>
  void write(Select &s, T t, std::experimental::coroutine_handle<> rh,
             Awaiter &a) {
    node.value = std::move(t);
    a.prh = rh.to_address();
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);

      auto pa = static_cast<Awaiter *>(node->data);
      std::unique_lock<std::mutex> lock{pa->mut};
      lock.unlock();
      auto rh = std::experimental::coroutine_handle<>::from_address(pa->prh);
      pa->selected_node = node;
      rh();
    };
    node.data = &a;

    node.success = false;
    node.pdone = &s.done;
    ptr->write(&node);
  }

  void remove() { ptr->remove_writer(&node); }

  explicit await_channel_writer(PtrType p) : ptr{p} {}

  ~await_channel_writer() { ptr->remove_writer(&node); }
};

namespace detail {
// Apply adapted from http://isocpp.org/files/papers/N3915.pdf
template <typename F, typename Tuple, size_t... I>
decltype(auto) apply_impl(F &&f, Tuple &&t,
                          std::integer_sequence<size_t, I...>) {
  using namespace std;
  return std::forward<F>(f)(get<I>(std::forward<Tuple>(t))...);
}
template <typename F, typename Tuple> decltype(auto) apply(F &&f, Tuple &&t) {
  using namespace std;
  using Indices = make_index_sequence<tuple_size<decay_t<Tuple>>::value>;
  return apply_impl(std::forward<F>(f), std::forward<Tuple>(t), Indices{});
}

template <class T> T get_await_select_type(T t) { return t; }

template <class T, class Ptr>
await_channel_reader<T, Ptr> *
get_await_select_type(await_channel_reader<T, Ptr> &t) {
  return &t;
}
template <class T, class Ptr>
await_channel_writer<T, Ptr> *
get_await_select_type(await_channel_writer<T, Ptr> &t) {
  return &t;
}

template <class... T> auto get_fd_tuple(T &&... t) {
  return std::make_tuple(get_await_select_type(t)...);
}

template <class This, class Reader, class Func>
void do_readwrite(detail::reader_type, This *pthis,
                  std::experimental::coroutine_handle<> &rh, Reader r,
                  Func &f) {
  r->get_node()->success = false;
  r->read(pthis->s, rh, *pthis);
}
template <class This, class Writer, class Func, class T>
void do_readwrite(detail::writer_type, This *pthis,
                  std::experimental::coroutine_handle<> &rh, Writer w, T t,
                  Func &f) {

  w->get_node()->success = false;
  w->write(pthis->s, std::move(t), rh, *pthis);
}

template <class This, class Reader, class Func, class R1, class... Rest>
void do_readwrite(detail::reader_type, This *pthis,
                  std::experimental::coroutine_handle<> &rh, Reader r, Func &f,
                  R1 r1, Rest &&... rest) {

  r->get_node()->success = false;
  r->read(pthis->s, rh, *pthis);
  do_readwrite(r1->get_role(), pthis, rh, r1, std::forward<Rest>(rest)...);
}
template <class This, class Writer, class Func, class R1, class T,
          class... Rest>
void do_readwrite(detail::writer_type, This *pthis,
                  std::experimental::coroutine_handle<> &rh, Writer w, T t,
                  Func &f, R1 r1, Rest &&... rest) {

  w->get_node()->success = false;
  w->write(pthis->s, std::move(t), rh, *pthis);
  do_readwrite(r1->get_role(), pthis, rh, r1, std::forward<Rest>(rest)...);
}

template <class This, class Reader, class Func>
void do_wake(detail::reader_type, This *pthis, Reader r, Func &f) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
}
template <class This, class Reader, class Func, class T>
void do_wake(detail::writer_type, This *pthis, Reader r, T &&, Func &f) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
}

template <class This, class Reader, class Func, class R1, class... Rest>
void do_wake(detail::reader_type, This *pthis, Reader r, Func &f, R1 r1,
             Rest &&... rest) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
  do_wake(r1->get_role(), pthis, r1, std::forward<Rest>(rest)...);
}

template <class This, class Reader, class Func, class T, class R1,
          class... Rest>
void do_wake(detail::writer_type, This *pthis, Reader r, T &&, Func &f, R1 r1,
             Rest &&... rest) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
  do_wake(r1->get_role(), pthis, r1, std::forward<Rest>(rest)...);
}
}

template <class... T> auto select(T &&... t) {
  auto tup = detail::get_fd_tuple(std::forward<T>(t)...);
  using Tuple = std::decay_t<decltype(tup)>;
  struct awaiter {
    Tuple t;
    channel_selector s;
    void *prh = nullptr;
    detail::node_base *selected_node = nullptr;
    std::mutex mut;

    awaiter(Tuple t) : t{std::move(t)} {}
    awaiter(awaiter &&other)
        : t{std::move(other.t)}, prh{other.prh},
          selected_node{other.selected_node} {}

    bool await_ready() { return false; }
    void await_suspend(std::experimental::coroutine_handle<> rh) {
      std::unique_lock<std::mutex> lock{mut};

      detail::apply(
          [this, &rh](auto &&r1, auto &&... ts) mutable {
            detail::do_readwrite(r1->get_role(), this, rh,
                                 std::forward<decltype(r1)>(r1),
                                 std::forward<decltype(ts)>(ts)...);
          },
          t);
    }
    auto await_resume() {
      detail::apply(
          [this](auto &&r1, auto &&... ts) {
            detail::do_wake(r1->get_role(), this,
                            std::forward<decltype(r1)>(r1),
                            std::forward<decltype(ts)>(ts)...);
          },
          t);
      return std::make_pair(!selected_node->closed, selected_node);
    }
  };

  return awaiter{std::move(tup)};
}

template <class Range, class Func> auto select_range(Range &r, Func f) {
  using std::begin;
  using Iter = decltype(begin(r));
  struct awaiter {
    Range *rng;
    Func f;
    channel_selector s;
    void *prh = nullptr;
    detail::node_base *selected_node = nullptr;
    std::mutex mut;

    awaiter(Range &rng, Func f) : rng{&rng}, f{f} {}
    awaiter(awaiter &&other)
        : rng{rng}, f{std::move(other.f)}, prh{other.prh},
          selected_node{other.selected_node} {}

    bool await_ready() { return false; }
    void await_suspend(std::experimental::coroutine_handle<> rh) {
      std::unique_lock<std::mutex> lock{mut};
      for (auto &r : *rng) {
        detail::do_readwrite(r.get_role(), this, rh, &r, f);
      }
    }
    auto await_resume() {
      using std::begin;
      using std::end;
      auto iter = begin(*rng);
      auto selected_iter = end(*rng);
      for (; iter != end(*rng); ++iter) {
        iter->remove();
        auto ns = iter->get_node()->success;
        iter->get_node()->success = false;
        if (ns) {
          detail::do_call(f, &iter->get_node()->value, iter->get_role());
        }

        if (iter->get_node() == selected_node) {
          selected_iter = iter;
        }
      }
      assert(selected_iter != end(*rng));

      return std::make_pair(!selected_node->closed, selected_iter);
    }
  };

  return awaiter{r, std::move(f)};
}

#endif // _MSC_VER
#include <condition_variable>

struct thread_suspender {
  std::mutex mut;
  std::condition_variable cvar;
  std::atomic<bool> suspended{false};

  void suspend() {
    std::unique_lock<std::mutex> lock{mut};
    suspended = true;
    while (suspended) {
      cvar.wait(lock);
    }
  }
  void resume() {
    while (!suspended)
      ;
    std::unique_lock<std::mutex> lock{mut};
    suspended = false;
    cvar.notify_all();
  }
};

template <class T, class SyncSuspender, class PtrType = std::shared_ptr<channel<T>>>
struct sync_channel_reader {

  using node_t = typename channel<T>::node_t;
  node_t node{};

  static auto get_role() { return detail::reader_type{}; }

  using value_type = T;
  PtrType ptr;

  SyncSuspender *psuspender = nullptr;

  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  auto read() {
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);
      auto ps = static_cast<SyncSuspender *>(node->data);
      ps->resume();
    };
    node.data = psuspender;
    node.pdone = nullptr;
    ptr->read(&node);
    psuspender->suspend();

    return std::make_pair(!node.closed, std::move(node.value));
  }
  template <class Select, class This> void read(Select &s, This *pthis) {
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);
      auto pthis = static_cast<This *>(node->data);
      pthis->selected_node = n;
      auto ps = pthis->ps;
      ps->resume();
    };
    node.data = pthis;
    node.success = false;

    node.pdone = &s.done;
    ptr->read(&node);
  }
  void remove() { ptr->remove_reader(&node); }

  explicit sync_channel_reader(PtrType p, SyncSuspender &s)
      : ptr{p}, psuspender{&s} {}
  ~sync_channel_reader() { ptr->remove_reader(&node); }
};
template <class T, class SyncSuspender, class PtrType = std::shared_ptr<channel<T>>>
struct sync_channel_writer {

  static auto get_role() { return detail::writer_type{}; }

  using node_t = typename channel<T>::node_t;
  node_t node{};

  PtrType ptr;

  SyncSuspender *psuspender = nullptr;
  auto get_node() const { return &node; }
  auto get_node() { return &node; }
  auto write(T t) {
    node.value = std::move(t);
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);
      auto ps = static_cast<SyncSuspender *>(node->data);
      ps->resume();
    };
    node.data = psuspender;
    node.pdone = nullptr;
    ptr->write(&node);
    psuspender->suspend();

    return !node.closed;
  }
  template <class Select, class T, class This>
  void write(Select &s, T t, This *pthis) {
    node.value = std::move(t);
    node.func = [](detail::node_base *n) {
      auto node = static_cast<node_t *>(n);
      auto pthis = static_cast<This *>(node->data);
      pthis->selected_node = n;
      auto ps = pthis->ps;
      ps->resume();
    };
    node.data = pthis;
    node.success = false;
    node.pdone = &s.done;
    ptr->write(&node);
  }

  void remove() { ptr->remove_writer(&node); }

  explicit sync_channel_writer(PtrType p, SyncSuspender &s)
      : ptr{p}, psuspender{&s} {}
  ~sync_channel_writer() { ptr->remove_reader(&node); }
};

namespace detail {
template <class T> T get_sync_select_type(T t) { return t; }

template <class T, class Ptr>
sync_channel_reader<T, Ptr> *
get_sync_select_type(sync_channel_reader<T, Ptr> &t) {
  return &t;
}
template <class T, class Ptr>
sync_channel_writer<T, Ptr> *
get_sync_select_type(sync_channel_writer<T, Ptr> &t) {
  return &t;
}

template <class... T> auto get_sync_tuple(T &&... t) {
  return std::make_tuple(get_sync_select_type(t)...);
}

template <class This, class Reader, class Func>
void do_sync_readwrite(detail::reader_type, This *pthis, Reader r, Func &f) {
  r->get_node()->success = false;
  r->read(pthis->s, pthis);
}
template <class This, class Writer, class Func, class T>
void do_sync_readwrite(detail::writer_type, This *pthis, Writer w, T t,
                       Func &f) {

  w->get_node()->success = false;
  w->write(pthis->s, std::move(t), pthis);
}

template <class This, class Reader, class Func, class R1, class... Rest>
void do_sync_readwrite(detail::reader_type, This *pthis, Reader r, Func &f,
                       R1 r1, Rest &&... rest) {

  r->get_node()->success = false;
  r->read(pthis->s, pthis);
  do_sync_readwrite(r1->get_role(), pthis, r1, std::forward<Rest>(rest)...);
}
template <class This, class Writer, class Func, class R1, class T,
          class... Rest>
void do_sync_readwrite(detail::writer_type, This *pthis, Writer w, T t, Func &f,
                       R1 r1, Rest &&... rest) {

  w->get_node()->success = false;
  w->write(pthis->s, std::move(t), pthis);
  do_sync_readwrite(r1->get_role(), pthis, r1, std::forward<Rest>(rest)...);
}

template <class Reader, class Func>
void do_sync_wake(detail::reader_type, Reader r, Func &f) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
}
template <class Reader, class Func, class T>
void do_sync_wake(detail::writer_type, Reader r, T &&, Func &f) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
}

template <class Reader, class Func, class R1, class... Rest>
void do_sync_wake(detail::reader_type, Reader r, Func &f, R1 r1,
                  Rest &&... rest) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
  do_sync_wake(r1->get_role(), r1, std::forward<Rest>(rest)...);
}

template <class Reader, class Func, class T, class R1, class... Rest>
void do_sync_wake(detail::writer_type, Reader r, T &&, Func &f, R1 r1,
                  Rest &&... rest) {
  r->remove();
  auto ns = r->get_node()->success;
  r->get_node()->success = false;
  if (ns) {
    do_call(f, &r->get_node()->value, r->get_role());
  }
  do_sync_wake(r1->get_role(), r1, std::forward<Rest>(rest)...);
}
}

template <class SyncSuspender,class... T> auto sync_select(SyncSuspender &s, T &&... t) {
  auto tup = detail::get_sync_tuple(std::forward<T>(t)...);
  using Tuple = std::decay_t<decltype(tup)>;
  std::mutex mut;

  struct this_t {
    channel_selector s;
    SyncSuspender *ps = nullptr;
    detail::node_base *selected_node = nullptr;
  };

  this_t athis;
  athis.ps = &s;

  detail::apply(
      [&](auto &&r1, auto &&... ts) mutable {
        detail::do_sync_readwrite(r1->get_role(), &athis,
                                  std::forward<decltype(r1)>(r1),
                                  std::forward<decltype(ts)>(ts)...);
      },
      tup);


  s.suspend();
  detail::apply(
      [&](auto &&r1, auto &&... ts) {
        detail::do_sync_wake(r1->get_role(), std::forward<decltype(r1)>(r1),
                             std::forward<decltype(ts)>(ts)...);
      },
      tup);
  return std::make_pair(!athis.selected_node->closed, athis.selected_node);
}
