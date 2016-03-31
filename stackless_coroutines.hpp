#pragma once
#include <exception>
#include <memory>
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

template <class F, std::size_t Pos> struct dummy_coroutine_context {

  template <class... T> void operator()(T &&... t) {}

  operation do_return() { return operation::_return; };
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; };
  operation do_next() { return operation::_next; };
  template <class... T> async_result do_async() { return async_result{}; }

  dummy_coroutine_context() {}
  template <class T> dummy_coroutine_context(T &) {}

  enum { position = Pos };
};

template <bool Loop> struct loop_base {};
template <> struct loop_base<true> {
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; }
};

template <class Finished, size_t Pos, bool Loop>
struct coroutine_context : loop_base<Loop> {

  enum { position = Pos };

  operation do_return() { return operation::_return; };
  operation do_next() { return operation::_next; };

  coroutine_context(Finished f) : f_{std::move(f)} {}
  Finished f_;
  Finished &f() { return f_; }
};

template <class ReturnValue, std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor;

template <std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<void, Pos, Size, Loop> {
  enum { position = Pos };

  using helper = coroutine_processor<operation, Pos, Size, Loop>;

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {

    coroutine_context<Finished, Pos, Loop> ctx{std::move(f)};
    auto &tuple = ctx.f().tuple();
    auto &value = ctx.f().value();
    std::get<Pos>(tuple)(ctx, value, std::forward<T>(results)...);
    return helper::do_next(f, std::integral_constant<std::size_t, Pos + 1>{});
  }
};
template <std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<operation, Pos, Size, Loop> {

  enum { position = Pos };

  template <class Finished>
  static operation do_next(Finished &f,
                           std::integral_constant<std::size_t, Size>) {
    return operation::_done;
  }
  template <class Finished, std::size_t P>
  static operation do_next(Finished &&f,
                           std::integral_constant<std::size_t, P>) {
    using DC = dummy_coroutine_context<Finished, P>;
    using next_return =
        decltype(std::get<P>(f.tuple())(std::declval<DC &>(), f.value()));

    return coroutine_processor<next_return, P, Size, Loop>::process(f);
  }

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {

    coroutine_context<Finished, Pos, Loop> ctx{std::move(f)};
    operation op = std::get<Pos>(ctx.f().tuple())(ctx, ctx.f().value(),
                                                  std::forward<T>(results)...);
    if (op == operation::_break) {
      return operation::_break;
    } else if (op == operation::_continue) {
      return do_next(f, std::integral_constant<std::size_t, 0>{});

    } else if (op == operation::_next) {
      return do_next(f, std::integral_constant<std::size_t, Pos + 1>{});
    } else if (op == operation::_return) {
      return operation::_return;
    } else if (op == operation::_suspend) {
      return operation::_suspend;
    }
    throw std::logic_error{"Invalid stackless_coroutine::operation"};
  }
};

template <std::size_t Pos, std::size_t Size, bool Loop>
struct coroutine_processor<async_result, Pos, Size, Loop> {

  enum { position = Pos };

  template <class Finished>
  struct async_context : coroutine_context<Finished, Pos, Loop> {
    using base_t = coroutine_context<Finished, Pos, Loop>;
    async_context(Finished f) : base_t{std::move(f)} {}
    using base_t::f;

    async_result do_async() { return async_result{}; }
    template <class... A> auto operator()(A &&... a) {
      using DC = dummy_coroutine_context<Finished, Pos + 1>;
      using next_return = decltype(std::get<Pos + 1>(f().tuple())(
          std::declval<DC &>(), f().value(), std::forward<decltype(a)>(a)...));

      using CP = coroutine_processor<next_return, Pos + 1, Size, Loop>;

      operation op;
      try {
        op = CP::process(f(), std::forward<decltype(a)>(a)...);
      } catch (...) {

        auto ep = std::current_exception();
        f()(f().value(), ep, true, operation::_exception);
        return operation::_done;
      };

      if (op == operation::_done || op == operation::_return ||
          op == operation::_break) {
        f()(f().value(), nullptr, true, op);
      }
      return op;
    }
  };

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {
    async_context<Finished> ctx{f};

    std::get<Pos>(ctx.f().tuple())(ctx, ctx.f().value(),
                                   std::forward<T>(results)...);
    return operation::_suspend;
  }
};

template <class Value, class Tuple, class F, class Destroyer = Value *>
struct finished_wrapper {

  Value *value_;
  Tuple tuple_;
  F f_;

  Tuple &tuple() { return tuple_; };
  Value &value() { return *value_; }
  enum { tuple_size = std::tuple_size<Tuple>::value };

  template <class... A> auto operator()(A &&... a) {
    Destroyer d{value_};
    (void)d;
    return f_(std::forward<A>(a)...);
  }
};
template <class Value, class Tuple, class F, class Destroyer>
struct finished_wrapper<Value, Tuple *, F, Destroyer> {

  Value *value_;
  Tuple *tuple_;
  F f_;

  Tuple &tuple() { return *tuple_; };
  Value &value() { return *value_; }
  enum { tuple_size = std::tuple_size<Tuple>::value };

  template <class... A> auto operator()(A &&... a) {
    Destroyer d{value_};
    (void)d;
    return f_(std::forward<A>(a)...);
  }
};

template <bool IsLoop, class Finished, class... A>
auto run_helper(Finished f, A &&... a) {

  using DC = dummy_coroutine_context<Finished, 0>;
  using ret_type = decltype(std::get<0>(f.tuple())(
      std::declval<DC &>(), f.value(), std::forward<A>(a)...));

  operation op;
  try {
    op =
        coroutine_processor<ret_type, 0, Finished::tuple_size, IsLoop>::process(
            f, std::forward<A>(a)...);
  } catch (...) {
    auto ep = std::current_exception();
    f(f.value(), ep, false, operation::_exception);
    return operation::_exception;
  };

  if (op == operation::_done || op == operation::_return) {
    f(f.value(), nullptr, false, op);
  }
  return op;
}

template <class Value, class Tuple, class FinishedTemp, class... A>
auto run_loop(Value *v, Tuple t, FinishedTemp f_temp, A &&... a) {
  using Finished = finished_wrapper<Value, Tuple, FinishedTemp>;

  return run_helper<true>(Finished{v, std::move(t), std::move(f_temp)});
}
}
template <class Value, class Tuple, class FinishedTemp, class... A>
auto run(Value *v, Tuple t, FinishedTemp f_temp, A &&... a) {
  using Finished = detail::finished_wrapper<Value, Tuple, FinishedTemp>;

  return detail::run_helper<false>(
      Finished{v, std::move(t), std::move(f_temp)});
}
template <class Type, class Deleter, class Tuple, class FinishedTemp,
          class... A>
auto run(std::unique_ptr<Type, Deleter> v, Tuple t, FinishedTemp f_temp,
         A &&... a) {
  using Finished = detail::finished_wrapper<Type, Tuple, FinishedTemp,
                                            std::unique_ptr<Type, Deleter>>;

  return detail::run_helper<false>(
      Finished{v.release(), std::move(t), std::move(f_temp)});
}

template <class... T> auto make_coroutine_tuple(T &&... t) {
  auto dummy = [](auto &, auto &) {};

  return std::make_tuple(std::forward<T>(t)..., std::move(dummy));
}
namespace detail {

template <std::size_t Offset, class Context, class Finished>
auto while_true_finished_helper(Finished &f) {

  constexpr auto size = Finished::tuple_size;
  constexpr auto pos = Context::position + Offset;

  using DC = dummy_coroutine_context<Finished, pos>;
  using ret_type =
      decltype(std::get<pos>(f.tuple())(std::declval<DC &>(), f.value()));

  operation op;
  try {
    op = coroutine_processor<ret_type, pos, size, false>::process(f);
  } catch (...) {
    auto ep = std::current_exception();
    (f)(f.value(), ep, true, operation::_exception);
    return operation::_exception;
  };
  if (op == operation::_done || op == operation::_return ||
      op == operation::_break) {
    (f)(f.value(), nullptr, true, op);
    return op;
  } else {
    return op;
  }
}
}
template <class... T> auto while_true(T &&... t) {

  auto dummy_terminator = [](auto &, auto &) { return operation::_continue; };
  auto tuple =
      make_coroutine_tuple(std::forward<T>(t)..., std::move(dummy_terminator));
  auto func = [tuple = std::move(tuple)](auto &context, auto &value)
                  ->operation {

    using context_type = std::decay_t<decltype(context)>;

    auto finished = [f = context.f()](auto &value, std::exception_ptr ep,
                                      bool async, operation op) mutable {
      if (async) {
        if (ep && op == operation::_exception) {
          (f)(f.value(), ep, true, operation::_exception);
        } else if (op == operation::_break) {
          detail::while_true_finished_helper<1, context_type>(f);
        } else if (op == operation::_return) {

          (f)(f.value(), ep, true, operation::_return);
        }

        // for operation::_suspend just return the op
        // We will not get back _done because of the dummy_terminator, and
        // _continue will get handled by the regular processors
      }
      return op;
    };

    operation op;
    try {
      op = detail::run_loop(&value, &tuple, finished);

      if (op == operation::_break) {
        return operation::_next;
      } else {
        return op;
      }
    } catch (...) {
      auto ep = std::current_exception();
      return operation::_exception;
    }
  };

  return func;
};
}
