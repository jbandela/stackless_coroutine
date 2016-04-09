// Copyright 2016 John R. Bandela
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#pragma once
#include <exception>
#include <stdexcept>
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
  F &f(){return *f_;}
  F* f_;
  dummy_coroutine_context() {}
  dummy_coroutine_context(const dummy_coroutine_context &) {}
  dummy_coroutine_context(dummy_coroutine_context &&) {}
  template <class T> dummy_coroutine_context(T &) {}

  enum { position = Pos };
  enum { is_loop = true };
  enum { is_if = true };
};

template <bool Loop, bool If> struct loop_base {
  enum { is_loop = false };
  enum { is_if = If };
};
template <bool If> struct loop_base<true, If> {
  enum { is_loop = true };
  enum { is_if = If };
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; }
};

template <class Finished, size_t Pos, bool Loop, bool If>
struct coroutine_context : loop_base<Loop, If> {

  enum { position = Pos };

  operation do_return() { return operation::_return; };
  operation do_next() { return operation::_next; };

  coroutine_context(Finished f) : f_{std::move(f)} {}
  Finished f_;
  Finished &f() { return f_; }
};

template <class ReturnValue, std::size_t Pos, std::size_t Size, bool Loop,
          bool If>
struct coroutine_processor;

template <std::size_t Pos, std::size_t Size, bool Loop, bool If>
struct coroutine_processor<void, Pos, Size, Loop, If> {
  enum { position = Pos };

  using helper = coroutine_processor<operation, Pos, Size, Loop, If>;

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {

    coroutine_context<Finished, Pos, Loop, If> ctx{std::move(f)};
    auto &tuple = ctx.f().tuple();
    auto &value = ctx.f().value();
    std::get<Pos>(tuple)(ctx, value, std::forward<T>(results)...);
    return helper::do_next(f, std::integral_constant<std::size_t, Pos + 1>{});
  }
};
template <std::size_t Pos, std::size_t Size, bool Loop, bool If>
struct coroutine_processor<operation, Pos, Size, Loop, If> {

  enum { position = Pos };

  template <class Finished>
  static operation do_next(Finished &f,
                           std::integral_constant<std::size_t, Size>) {
    return operation::_done;
  }
  template <class Finished, std::size_t P>
  static operation do_next(Finished &f,
                           std::integral_constant<std::size_t, P>) {
    using DC = dummy_coroutine_context<Finished, P>;
    using next_return =
        decltype(std::get<P>(f.tuple())(std::declval<DC &>(), f.value()));

    return coroutine_processor<next_return, P, Size, Loop, If>::process(f);
  }

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {

    coroutine_context<Finished, Pos, Loop, If> ctx{std::move(f)};
    operation op = std::get<Pos>(ctx.f().tuple())(ctx, ctx.f().value(),
                                                  std::forward<T>(results)...);
    if (op == operation::_break) {
      return operation::_break;
    } else if (op == operation::_continue) {
      if (If) {
        return op;
      } else {
        return do_next(f, std::integral_constant<std::size_t, Loop?0:Size>{});
      }

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

template <std::size_t Pos, std::size_t Size, bool Loop, bool If>
struct coroutine_processor<async_result, Pos, Size, Loop, If> {

  enum { position = Pos };

  template <class Finished>
  struct async_context : coroutine_context<Finished, Pos, Loop, If> {
    using base_t = coroutine_context<Finished, Pos, Loop, If>;
    async_context(Finished f) : base_t{std::move(f)} {}
    using base_t::f;

    async_result do_async() { return async_result{}; }
    template <class... A> auto operator()(A &&... a) {
      using DC = dummy_coroutine_context<Finished, Pos + 1>;
      using next_return = decltype(std::get<Pos + 1>(f().tuple())(
          std::declval<DC &>(), f().value(), std::forward<decltype(a)>(a)...));

      using CP = coroutine_processor<next_return, Pos + 1, Size, Loop, If>;

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
  const Tuple *tuple_;
  F f_;

  const Tuple &tuple() { return *tuple_; };
  Value &value() { return *value_; }
  enum { tuple_size = std::tuple_size<Tuple>::value };

  template <class... A> auto operator()(A &&... a) {
    Destroyer d{value_};
    (void)d;
    return f_(std::forward<A>(a)...);
  }
};

template <bool IsLoop, bool If, class Finished, class... A>
auto run_helper(Finished f, A &&... a) {

  using DC = dummy_coroutine_context<Finished, 0>;
  using ret_type = decltype(std::get<0>(f.tuple())(
      std::declval<DC &>(), f.value(), std::forward<A>(a)...));

  operation op;
  try {
    op = coroutine_processor<ret_type, 0, Finished::tuple_size, IsLoop,
                             If>::process(f, std::forward<A>(a)...);
  } catch (...) {
    auto ep = std::current_exception();
    f(f.value(), ep, false, operation::_exception);
    return operation::_exception;
  };

  if (op == operation::_done || op == operation::_return ||
      (If && op == operation::_continue) || (If && op == operation::_break)) {
    f(f.value(), nullptr, false, op);
  }
  return op;
}

template <class Value, class Tuple, class FinishedTemp, class... A>
auto run_loop(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = finished_wrapper<Value, Tuple, FinishedTemp>;

  return run_helper<true, false>(Finished{v, t, std::move(f_temp)});
}
template <bool Loop, class Value, class Tuple, class FinishedTemp, class... A>
auto run_if(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = finished_wrapper<Value, Tuple, FinishedTemp>;

  return run_helper<Loop, true>(Finished{v, t, std::move(f_temp)});
}
}
template <class Value, class Tuple, class FinishedTemp, class... A>
auto run(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = detail::finished_wrapper<Value, Tuple, FinishedTemp>;

  return detail::run_helper<false, false>(Finished{v, t, std::move(f_temp)});
}
template <class Type, class Deleter, class Tuple, class FinishedTemp,
          class... A>
auto run(std::unique_ptr<Type, Deleter> v, const Tuple *t, FinishedTemp f_temp,
         A &&... a) {
  using Finished = detail::finished_wrapper<Type, Tuple, FinishedTemp,
                                            std::unique_ptr<Type, Deleter>>;

  return detail::run_helper<false, false>(
      Finished{v.release(), t, std::move(f_temp)},std::forward<A>(a)...);
}

namespace detail {

auto dummy_terminator = [](auto &, auto &) {};
using dummy_terminator_t = std::decay_t<decltype(dummy_terminator)>;
}

template <class... T>
auto make_block(T &&... t)
    -> const std::tuple<std::decay_t<T>..., detail::dummy_terminator_t> *{

  static const std::tuple<std::decay_t<T>..., detail::dummy_terminator_t> tup{
      std::forward<T>(t)..., detail::dummy_terminator};
  return &tup;
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
    op = coroutine_processor<ret_type, pos, size, Context::is_loop,
                             Context::is_if>::process(f);
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

auto dummy_while_terminator = [](auto &, auto &) {
  return operation::_continue;
};
using dummy_while_terminator_t = std::decay_t<decltype(dummy_while_terminator)>;
}
template <class... T> auto make_while_true(T &&... t) {

  auto tuple =
      make_block(std::forward<T>(t)..., detail::dummy_while_terminator);
  auto func = [tuple](auto &context, auto &value) -> operation {

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
    op = detail::run_loop(&value, tuple, finished);

    if (op == operation::_break) {
      return operation::_next;
    } else {
      return op;
    }
  };

  return func;
};

template <class Pred, class Then, class Else>
auto make_if(Pred pred, Then t, Else e) {
  auto tup = make_block(
      [pred,t,e](auto &context, auto &value) {
        using context_t = std::decay_t<decltype(context)>;
        if (pred(value)) {
          detail::run_if<context_t::is_loop>(&value, t, context);
        } else {
          detail::run_if<context_t::is_loop>(&value, e, context);
        }
        return context.do_async();

      },
      [](auto &context, auto &value, auto &a, std::exception_ptr e, bool as,
         auto op) {
        if (op == operation::_done)
          return operation::_next;
        if (op == operation::_exception && e)
          context.f()(value, e, as, operation::_exception);
        return op;
      }

      );

  auto func = [tup](auto &context, auto &value) -> operation {

    using context_t = std::decay_t<decltype(context)>;

    return detail::run_if<context_t::is_loop>(
        &value, tup, [context](auto &value, std::exception_ptr ep, bool async,
                               operation op) mutable {
          auto &f = context.f();

          if (ep && op == operation::_exception) {
            f(value, ep, async, op);
          } else if (op == operation::_break || op == operation::_continue ||
                     op == operation::_return) {
            f(value, ep, async, op);
          } else if (op == operation::_done) {
            detail::while_true_finished_helper<1, context_t>(f);
          }
        });

  };
  return func;
}

template<class P,class... T>
auto make_while(P p, T&&... t) {
	return make_while_true([=](auto& context, auto& value) {
		if (p(value)) {
			return context.do_next();
		}
		else {
			return context.do_break();
		}
	},t...);
}
}
