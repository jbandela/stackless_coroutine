#include <assert.h>
#include <exception>
#include <tuple>
#include <type_traits>
#include <utility>

namespace stackless_coroutine {
enum class operation {
  _suspend,
  _next,
  _return,
  _break,
  _continue,
  _done,
  _exception,
  _size
};
const char *operation_descriptions[] = {"suspend",   "next",     "return",
                                        "break",     "continue", "done",
                                        "exception", "size"};

static_assert(static_cast<std::size_t>(operation::_size) + 1 ==
                  sizeof(operation_descriptions) /
                      sizeof(operation_descriptions[0]),
              "stackless_coroutine::operations does not match size");

namespace detail {
struct async_result {};

template <class CI, class F, std::size_t Pos> struct dummy_coroutine_context {

  template <class... T> void operator()(T &&... t) {}

  operation do_return() { return operation::_return; };
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; };
  operation do_next() { return operation::_next; };
  template <class... T> async_result do_async() { return async_result{}; }

  dummy_coroutine_context() {}
  template <class T> dummy_coroutine_context(T &) {}

  CI *ci_;
  F *f_;
  enum { position = Pos };
};

template <class CI, class Finished, size_t Pos, bool CopyCI = false>
struct base_coroutine_context {

  enum { position = Pos };
  CI *ci_;
  Finished f_;

  operation do_return() { return operation::_return; };
  operation do_next() { return operation::_next; };
  base_coroutine_context(CI &ci, Finished f) : ci_(&ci), f_{std::move(f)} {}
  CI &ci() { return *ci_; }
};
template <class CI, class Finished, size_t Pos>
struct base_coroutine_context<CI, Finished, Pos, true> {

  enum { position = Pos };
  CI ci_v_;
  Finished f_;

  operation do_return() { return operation::_return; };
  operation do_next() { return operation::_next; };
  base_coroutine_context(CI ci, Finished f)
      : ci_v_{std::move(ci)}, f_{std::move(f)} {}
  CI &ci() { return ci_v_; }
};

template <class Base> struct loop_coroutine_context : Base {
  using Base::Base;
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; }
};

template <class CI, class ReturnValue, std::size_t Pos, std::size_t Size,
          bool Loop>
struct coroutine_processor;

template <class CI, class Finished, std::size_t Pos, bool Loop>
struct coroutine_context_helper {
  using type = base_coroutine_context<CI, Finished, Pos, Loop>;
};

template <class CI, class Finished, std::size_t Pos>
struct coroutine_context_helper<CI, Finished, Pos, true> {

  using base = base_coroutine_context<CI, Finished, Pos, true>;
  using type = loop_coroutine_context<base>;
};

template <class CI, class Finished, std::size_t Pos, bool Loop>
using coroutine_context =
    typename coroutine_context_helper<CI, Finished, Pos, Loop>::type;

template <class T> void assign_helper(T &v, T t) { v = std::move(t); }

template <class CI, std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<CI, void, Pos, Size, Loop> {
  enum { position = Pos };
  using value_type = typename CI::value_type;

  using self = coroutine_processor;

  template <class Finished>
  static operation do_next(CI &ci, value_type &value, Finished &,
                           std::integral_constant<std::size_t, Size>) {
    return operation::_done;
  }
  template <class Finished, std::size_t P>
  static operation do_next(CI &ci, value_type &value, Finished &f,
                           std::integral_constant<std::size_t, P>) {

    using DC = dummy_coroutine_context<CI, Finished, P>;
    using next_return =
        decltype(std::get<P>(ci.tuple_)(std::declval<DC &>(), ci.value_));

    return coroutine_processor<CI, next_return, P, Size, Loop>::process(
        ci, value, f);
  }

  template <class Finished, class... T>
  static operation process(CI &ci, value_type &value, Finished f,
                           T &&... results) {

    coroutine_context<CI, Finished, Pos, Loop> ctx{ci, f};
    std::get<Pos>(ci.tuple_)(ctx, value, std::forward<T>(results)...);
    return do_next(ci, value, f,
                   std::integral_constant<std::size_t, Pos + 1>{});
  }
};
template <class CI, std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<CI, operation, Pos, Size, Loop> {

  enum { position = Pos };
  using value_type = typename CI::value_type;

  using self = coroutine_processor;

  template <class Finished>
  static operation do_next(CI &ci, value_type &value, Finished &f,
                           std::integral_constant<std::size_t, Size>) {
    return operation::_done;
  }
  template <class Finished, std::size_t P>
  static operation do_next(CI &ci, value_type &value, Finished &f,
                           std::integral_constant<std::size_t, P>) {
    using DC = dummy_coroutine_context<CI, Finished, P>;
    using next_return =
        decltype(std::get<P>(ci.tuple_)(std::declval<DC &>(), ci.value_));

    return coroutine_processor<CI, next_return, Pos + 1, Size, Loop>::process(
        ci, value, f);
  }

  template <class Finished, class... T>
  static operation process(CI &ci, value_type &value, Finished f,
                           T &&... results) {

    coroutine_context<CI, Finished, Pos, Loop> ctx{ci, f};
    operation op =
        std::get<Pos>(ci.tuple_)(ctx, value, std::forward<T>(results)...);
    if (op == operation::_break) {
      return operation::_break;
    } else if (op == operation::_continue) {
      return operation::_continue;
    } else if (op == operation::_next) {
      return do_next(ci, value, f,
                     std::integral_constant<std::size_t, Pos + 1>{});
    } else if (op == operation::_return) {
      return operation::_return;
    } else if (op == operation::_suspend) {
      return operation::_suspend;
    }
    throw std::logic_error{"Invalid stackless_coroutine::operation"};
  }
};

template <class CI, std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<CI, async_result, Pos, Size, Loop> {

  enum { position = Pos };

  using self = coroutine_processor;
  template <class Finished>
  struct async_context : coroutine_context<CI, Finished, Pos, Loop> {
    using base_t = coroutine_context<CI, Finished, Pos, Loop>;
    async_context(CI &ci, Finished f) : base_t{ci, std::move(f)} {}
    using base_t::ci;

    async_result do_async() { return async_result{}; }
    template <class... A> auto operator()(A &&... a) {
      using DC = dummy_coroutine_context<CI, Finished, Pos + 1>;
      using next_return = decltype(std::get<Pos + 1>(ci().tuple_)(
          std::declval<DC &>(), ci().value_, std::forward<decltype(a)>(a)...));

      using CP = coroutine_processor<CI, next_return, Pos + 1, Size, Loop>;

      operation op;
      try {
        op = CP::process(ci(), ci().value_, this->f_,
                         std::forward<decltype(a)>(a)...);
      } catch (...) {

        auto ep = std::current_exception();
        this->f_(ci().value_, ep, true, operation::_exception);
        return operation::_done;
      };

      if (op == operation::_done || op == operation::_return ||
          op == operation::_continue || op == operation::_break) {
        this->f_(ci().value_, nullptr, true, op);
      }
      return op;
    }
  };

  template <class Finished, class... T>
  static operation process(CI &ci, typename CI::value_type &value, Finished &f,
                           T &&... results) {
    async_context<Finished> ctx{ci, f};

    std::get<Pos>(ci.tuple_)(ctx, value, std::forward<T>(results)...);
    return operation::_suspend;
  }
};

template <class ValueType, bool Loop, class... T> struct coroutine {
  using value_type = ValueType;
  value_type value_;

  std::tuple<T...> tuple_;

  using lambda_tuple_t = std::tuple<T...>;

  enum { is_loop = Loop };
  enum { size = sizeof...(T) };
  enum { last_tuple_position = size - 1 };

  using self_t = coroutine;

  template <class V, class... A>
  coroutine(V v, A &&... a)
      : value_{std::move(v)}, tuple_{std::forward<A>(a)...} {}

  template <class V, class... A>
  coroutine(V v, std::tuple<A...> a)
      : value_{std::move(v)}, tuple_{std::move(a)} {}

  template <class Finished, class... A> auto run(Finished f, A &&... a) {

    using DC = dummy_coroutine_context<self_t, Finished, 0>;
    using ret_type = decltype(std::get<0>(tuple_)(std::declval<DC &>(), value_,
                                                  std::forward<A>(a)...));

    operation op;
    try {
      op = coroutine_processor<self_t, ret_type, 0, size, Loop>::process(
          *this, value_, f, std::forward<A>(a)...);
    } catch (...) {
      auto ep = std::current_exception();
      f(value_, ep, false, operation::_exception);
    };

    if (op == operation::_done || op == operation::_return) {
      f(value_, nullptr, false, op);
    }
    return op;
  }

  value_type &value() { return value_; }
};
}

template <class ValueFunc, class... T>
auto make_coroutine(ValueFunc &&vf, T &&... t) {
  using namespace stackless_coroutine::detail;

  auto dummy_terminator = [](auto &&...) {};
  return coroutine<std::decay_t<decltype(vf())>, false, std::decay_t<T>...,
                   decltype(dummy_terminator)>{vf(), std::forward<T>(t)...,
                                               dummy_terminator};
}

namespace detail {

template <class T, class OuterValue> struct while_value : T {
  T *value_;
  OuterValue *outer_;
  OuterValue *outer_most_;

  auto &outer_value() { return *outer_; }
  auto &outer_most_value() { return *outer_most_; }
  auto &value() { return *value_; }

  while_value(T *t, OuterValue *outer)
      : value_{t}, outer_{outer}, outer_most_{outer} {}
};
template <class T, class OuterT, class OuterOuterValue>
struct while_value<T, while_value<OuterT, OuterOuterValue>> : T {
  using OuterValue = while_value<OuterT, OuterOuterValue>;
  T *value_;
  OuterValue *outer_;
  OuterValue *outer_most_;

  auto &outer_value() { return *outer_; }
  auto &outer_most_value() { return *outer_most_; }
  auto &value() { return *value_; }

  while_value(T *t, OuterValue *outer)
      : value_{t}, outer_{outer}, outer_most_{outer->outer_most_} {}
};

template <class value_type, class V, class OV, class... T>
auto get_while_coroutine(V &value, OV &outer_value, T &... t) {
  return coroutine<value_type, true, T...>{value_type{&value, &outer_value},
                                           std::tie(t...)};
}

template <std::size_t Offset, class Context>
auto while_true_finished_helper(Context &context) {

  constexpr auto size =
      std::tuple_size<std::decay_t<decltype(context.ci().tuple_)>>::value;
  constexpr auto pos = Context::position + Offset;
  using CI = std::decay_t<decltype(context.ci())>;
  using F = std::decay_t<decltype(context.f_)>;

  using DC = dummy_coroutine_context<CI, F, pos>;
  using ret_type = decltype(std::get<pos>(context.ci().tuple_)(
      std::declval<DC &>(), context.ci().value_));

  while (true) {
    operation op;
    try {
      op = coroutine_processor<CI, ret_type, pos, size, false>::process(
          context.ci(), context.ci().value_, context.f_);
    } catch (...) {
      auto ep = std::current_exception();
      (context.f_)(context.ci().value_, ep, true, operation::_exception);
      return operation::_exception;
    };
    if (op == operation::_done || op == operation::_return ||
        op == operation::_break) {
      (context.f_)(context.ci().value_, nullptr, true, op);
      return op;
    }
    if (op == operation::_continue) {
      continue;
    }
    if (op == operation::_suspend) {
      return op;
    }
  }
}
}
template <class ValueFunc, class... T>
auto make_while_true(ValueFunc vf, T &&... t) {
  using namespace stackless_coroutine::detail;

  auto dummy_terminator = [](auto &&...) {};
  auto func =
      [ vf = std::move(vf), t..., dummy_terminator, value = decltype(vf()){} ](
          auto &context, auto &outer_value) mutable->operation {
    using value_type = detail::while_value<std::decay_t<decltype(value)>,
                                           std::decay_t<decltype(outer_value)>>;

    value = vf();

    auto finished = [context](auto &value, std::exception_ptr ep, bool async,
                              operation op) mutable {
      if (async) {
        if (ep && op == operation::_exception) {
          (context.f_)(context.ci().value_, ep, true, operation::_exception);
        } else if (op == operation::_break) {
          detail::while_true_finished_helper<1>(context);
        } else if (op == operation::_done || op == operation::_continue) {
          detail::while_true_finished_helper<0>(context);
        }

        else if (op == operation::_return) {

          (context.f_)(value.outer_value(), ep, true, operation::_return);
        }

        // don't have to do anything for operation::_next, and
        // operation::_suspend
      }
      return op;
    };

    while (true) {
      value = vf();
      auto c = detail::get_while_coroutine<value_type>(value, outer_value, t...,
                                                       dummy_terminator);
      operation op;
      try {
        op = c.run(finished);

        // reuse finished
        if (op == operation::_break) {
          return operation::_next;
        } else if (op == operation::_exception || op == operation::_return ||
                   op == operation::_suspend) {
          return op;
        } else if (op == operation::_done || op == operation::_continue) {
          continue;
        }
      } catch (...) {
        auto ep = std::current_exception();
        return operation::_exception;
      }
    }
  };

  return func;
};
}

#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <iostream>

auto get_coroutine(boost::asio::io_service &io, std::string host,
                   std::string service, std::string url) {
  return stackless_coroutine::make_coroutine(

      [&io, host, service, url]() {
        struct val {
          std::string host;
          std::string service;
          std::string url;
          boost::asio::io_service &io;

          std::unique_ptr<boost::asio::ip::tcp::resolver> resolver;
          boost::asio::ip::tcp::socket socket_;
          std::string request_string;
        };

        return val{std::move(host),
                   std::move(service),
                   std::move(url),
                   io,
                   std::make_unique<boost::asio::ip::tcp::resolver>(io),
                   boost::asio::ip::tcp::socket{io},
                   std::string{}};

      },
      // Resolve the host
      [](auto &context, auto &value) {
        boost::asio::ip::tcp::resolver::query query{value.host, value.service};
        value.resolver->async_resolve(query, context);
        return context.do_async();
      },
      // Connect to the host
      [](auto &context, auto &value, auto ec, auto iterator) {

        if (ec) {
          throw boost::system::system_error{ec};
        } else {
          boost::asio::async_connect(value.socket_, iterator, context);
          return context.do_async();
        }

      },
      // Check if connection successful
      [](auto &context, auto &value, auto &ec, auto iterator) {

        if (ec) {
          throw boost::system::system_error{ec};
        } else {
        }

      },
      // Send the GET message
      [](auto &context, auto &value) {

        std::stringstream request_stream;
        request_stream << "GET " << value.url << " HTTP/1.0\r\n";
        request_stream << "Host: " << value.host << "\r\n";
        request_stream << "Accept: */*\r\n";
        request_stream << "Connection: close\r\n\r\n";

        // Send the request.
        value.request_string = request_stream.str();
        auto &str = value.request_string;
        boost::asio::async_write(value.socket_,
                                 boost::asio::buffer(str.data(), str.size()),
                                 context);
        return context.do_async();
      },
      //  Check for errors
      [](auto &context, auto &value, auto &ec, auto n) {
        if (ec) {
          throw boost::system::system_error{ec};
        }

      },
      // Read in a loop
      stackless_coroutine::make_while_true(
          []() {
            struct dummy {
              std::string current;
            };
            return dummy{};
          },
          [](auto &context, auto &value) {
            value.value().current.resize(20);
            value.outer_value().socket_.async_read_some(
                boost::asio::buffer(&value.value().current[0],
                                    value.value().current.size()),
                context);
            return context.do_async();

          },
          [](auto &context, auto &value, auto &ec, auto n) {
            if (ec) {
              if (ec != boost::asio::error::eof)
                std::cerr << ec.message();
              return context.do_break();
            }
            value.value().current.resize(n);
            std::cout << value.value().current;
            return context.do_next();
          }));
}

#include <future>
#include <memory>

int main() {

  boost::asio::io_service io;
  auto work = std::make_unique<boost::asio::io_service::work>(io);

  auto ci = get_coroutine(io, "www.httpbin.org", "http", "/");

  std::promise<void> done_promise;

  auto done_future = done_promise.get_future();

  auto func = [&]() {
    ci.run([&](auto &a, std::exception_ptr e, bool as, auto op) {
      if (e) {
        done_promise.set_exception(e);
      } else {
        done_promise.set_value();
      }
    });
  };

  io.post(func);

  auto frun = std::async(std::launch::async, [&]() { io.run(); });

  done_future.wait();

  work.reset();

  frun.get();

  try {

    done_future.get();
  } catch (std::exception &e) {
    std::cerr << e.what() << "\n";
  }
};