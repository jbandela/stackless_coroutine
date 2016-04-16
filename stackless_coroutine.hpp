// Copyright 2016 John R. Bandela
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#pragma once
#include <algorithm>
#include <array>
#include <exception>
#include <memory>
#include <stdexcept>
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
namespace detail {
struct async_result {
  operation op = operation::_suspend;

  async_result() {}
  async_result(operation op) : op(op) {}
};

template <class F, std::size_t Pos> struct dummy_coroutine_context {

  template <class... T> void operator()(T &&... t) {}

  operation do_return() { return operation::_return; };
  operation do_break() { return operation::_break; }
  operation do_continue() { return operation::_continue; };
  operation do_next() { return operation::_next; };
  async_result do_async() { return async_result{}; }
  async_result do_async_return() { return async_result{}; }
  async_result do_async_break() { return async_result{}; }
  async_result do_async_continue() { return async_result{}; }
  F &f() { return *f_; }
  F *f_;
  dummy_coroutine_context() {}
  dummy_coroutine_context(const dummy_coroutine_context &) {}
  dummy_coroutine_context(dummy_coroutine_context &&) {}
  template <class T> dummy_coroutine_context(T &) {}

  template <class V> static dummy_coroutine_context get_context(V *v);

  enum { position = Pos };
  enum { level = F::level };
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
  async_result do_async_break() { return async_result{operation::_break}; }
  async_result do_async_continue() {
    return async_result{operation::_continue};
  }
};

template <class Finished, size_t Pos, bool Loop, bool If>
struct coroutine_context : loop_base<Loop, If> {

  enum { position = Pos };
  enum { level = Finished::level };

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
    if (op == operation::_continue) {
      if (If) {
        return op;
      } else {
        return do_next(f, std::integral_constant < std::size_t,
                       Loop ? 0 : Size > {});
      }
    } else if (op == operation::_next) {
      return do_next(f, std::integral_constant<std::size_t, Pos + 1>{});
    }
    return op;
  }
};

template <typename V, typename F, typename = void>
struct has_finished_storage : std::false_type {};
template <typename V, typename F>
struct has_finished_storage<
    V, F,
    decltype((void)std::declval<V>().stackless_coroutine_finished_storage)> {
  using storage_t = std::decay_t<decltype(
      std::declval<V>().stackless_coroutine_finished_storage)>;
  static constexpr bool value =
      sizeof(F) <= sizeof(storage_t) && alignof(F) <= alignof(storage_t);
};

template <class V, class F>
constexpr bool has_finished_storage_v = has_finished_storage<V, F>::value;

template <class AsyncContext, class Finished, bool>
struct get_context_from_void {
  template <class V> auto static get_context(V *v) {
    Finished f{static_cast<typename Finished::value_t *>(v)};
    return AsyncContext{f};
  }
};
template <class AsyncContext, class Finished>
struct get_context_from_void<AsyncContext, Finished, false> {};

template <std::size_t Pos, std::size_t Size, bool Loop, bool If>
struct coroutine_processor<async_result, Pos, Size, Loop, If> {

  enum { position = Pos };

  template <class Finished>
  struct async_context
      : coroutine_context<Finished, Pos, Loop, If>,
        get_context_from_void<
            async_context<Finished>, Finished,
            has_finished_storage<typename Finished::value_t, Finished>::value> {
    using base_t = coroutine_context<Finished, Pos, Loop, If>;
    async_context(Finished f) : base_t{std::move(f)} {}
    using base_t::f;

    async_result do_async() { return async_result{operation::_suspend}; }
    async_result do_async_return() { return async_result{operation::_return}; }
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
        f()(f().value(), ep, operation::_exception);
        return operation::_done;
      };

      if (op == operation::_done || op == operation::_return ||
          op == operation::_break) {
        f()(f().value(), nullptr, op);
      }
      return op;
    }
  };

  template <class Finished, class... T>
  static operation process(Finished &f, T &&... results) {
    async_context<Finished> ctx{f};

    auto r = std::get<Pos>(ctx.f().tuple())(ctx, ctx.f().value(),
                                            std::forward<T>(results)...);
    return r.op;
  }
};

template <class F, class Tuple> struct finished_tuple_holder {
  F f;
  const Tuple *t;
};
template <std::size_t Level, class Value, class Tuple, class F, class Destroyer,
          bool HasFinishedStorage>
struct finished_wrapper_impl {

  enum { level = Level };
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
template <std::size_t Level, class Value, class Tuple, class F, class Destroyer>
struct finished_wrapper_impl<Level, Value, Tuple, F, Destroyer, true> {

  enum { level = Level };
  Value *value_;

  finished_tuple_holder<F, Tuple> &ft_holder() {
    static_assert(
        std::tuple_size<decltype(
                value_->stackless_coroutine_finished_storage)>::value > Level,
        "stackless_coroutine_finished_storage array not large enough");
    return *reinterpret_cast<finished_tuple_holder<F, Tuple> *>(
        &(value_->stackless_coroutine_finished_storage[Level]));
  };
  const Tuple &tuple() { return *ft_holder().t; };
  F &f() { return ft_holder().f; };
  Value &value() { return *value_; }
  enum { tuple_size = std::tuple_size<Tuple>::value };

  finished_wrapper_impl(Value *v, const Tuple *t, F f_temp) : value_{v} {

    static_assert(
        std::tuple_size<decltype(
                value_->stackless_coroutine_finished_storage)>::value > Level,
        "stackless_coroutine_finished_storage array not large enough");

    using element_t = std::tuple_element_t<
        Level, decltype(value_->stackless_coroutine_finished_storage)>;

    static_assert(

        sizeof(finished_tuple_holder<F, Tuple>) <= sizeof(element_t) &&
            alignof(finished_tuple_holder<F, Tuple>) <= alignof(element_t),
        "stackless_coroutine_finished_storage element not large enough or not "
        "aligned correctly");
    new (&(value_->stackless_coroutine_finished_storage[Level]))
        finished_tuple_holder<F, Tuple>{std::move(f_temp), t};
  }
  finished_wrapper_impl(Value *v) : value_{v} {}

  struct f_destroyer {
    finished_wrapper_impl *pthis;

    ~f_destroyer() { pthis->f().~F(); }
  };
  template <class... A> auto operator()(A &&... a) {
    if (Level == 0) {
      Destroyer d{value_};
      f_destroyer fd{this};
      (void)d;
      f()(std::forward<A>(a)...);

    } else {
      f_destroyer fd{this};
      f()(std::forward<A>(a)...);
    }
  }
};
template <std::size_t Level, class Value, class Tuple, class F,
          class Destroyer = Value *>
struct finished_wrapper
    : finished_wrapper_impl<Level, Value, Tuple, F, Destroyer,
                            has_finished_storage_v<Value, F>> {

  using value_t = Value;
  using fimpl = finished_wrapper_impl<Level, Value, Tuple, F, Destroyer,
                                      has_finished_storage_v<Value, F>>;

  template <class... T>
  finished_wrapper(T &&... t) : fimpl{std::forward<T>(t)...} {}
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
    f(f.value(), ep, operation::_exception);
    return operation::_exception;
  };

  if (op == operation::_done || op == operation::_return ||
      (If && op == operation::_continue) || (If && op == operation::_break)) {
    f(f.value(), nullptr, op);
  }
  return op;
}

template <std::size_t Level, class Value, class Tuple, class FinishedTemp,
          class... A>
auto run_loop(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = finished_wrapper<Level, Value, Tuple, FinishedTemp>;

  return run_helper<true, false>(Finished{v, t, std::move(f_temp)});
}
template <bool Loop, std::size_t Level, class Value, class Tuple,
          class FinishedTemp, class... A>
auto run_if(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = finished_wrapper<Level, Value, Tuple, FinishedTemp>;

  return run_helper<Loop, true>(Finished{v, t, std::move(f_temp)});
}
}
template <class Value, class Tuple, class FinishedTemp, class... A>
auto run(Value *v, const Tuple *t, FinishedTemp f_temp, A &&... a) {
  using Finished = detail::finished_wrapper<0, Value, Tuple, FinishedTemp>;

  return detail::run_helper<false, false>(Finished{v, t, std::move(f_temp)});
}
template <class Type, class Deleter, class Tuple, class FinishedTemp,
          class... A>
auto run(std::unique_ptr<Type, Deleter> v, const Tuple *t, FinishedTemp f_temp,
         A &&... a) {
  using Finished = detail::finished_wrapper<0, Type, Tuple, FinishedTemp,
                                            std::unique_ptr<Type, Deleter>>;

  return detail::run_helper<false, false>(
      Finished{v.release(), t, std::move(f_temp)}, std::forward<A>(a)...);
}

namespace detail {

const auto dummy_terminator = [](auto &, auto &) {};
using dummy_terminator_t = std::decay_t<decltype(dummy_terminator)>;
}

template <class... T>
auto make_block(T &&... t)
    -> const std::tuple<std::decay_t<T>..., detail::dummy_terminator_t> * {

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
    (f)(f.value(), ep, operation::_exception);
    return operation::_exception;
  };
  if (op == operation::_done || op == operation::_return ||
      op == operation::_break) {
    (f)(f.value(), nullptr, op);
    return op;
  } else {
    return op;
  }
}

const auto dummy_while_terminator = [](auto &, auto &) {
  return operation::_continue;
};
using dummy_while_terminator_t = std::decay_t<decltype(dummy_while_terminator)>;
}
template <class Tuple> struct make_while_func_t {
  Tuple tuple;

  using tuple_t = std::remove_pointer_t<Tuple>;

  template <class Context, class Value>
  auto operator()(Context &context, Value &value) const {
    using context_type = std::decay_t<decltype(context)>;

    auto finished = [f = context.f()](auto &value, std::exception_ptr ep,
                                      operation op) mutable {
      if (ep && op == operation::_exception) {
        (f)(f.value(), ep, operation::_exception);
      } else if (op == operation::_break) {
        detail::while_true_finished_helper<1, context_type>(f);
      } else if (op == operation::_return) {
        (f)(f.value(), ep, operation::_return);
      }
      return op;
    };

    detail::run_loop<context_type::level + 1>(&value, tuple, finished);
    return operation::_suspend;
  }
};
template <class... T> auto make_while_true(T &&... t) {

  auto tuple =
      make_block(std::forward<T>(t)..., detail::dummy_while_terminator);
  using mwf_t = make_while_func_t<decltype(tuple)>;
  mwf_t func{tuple};
  return func;
};

template <class Tuple> struct make_if_func_t {
  Tuple tuple;
  using tuple_t = std::remove_pointer_t<Tuple>;

  template <class Context, class Value>
  auto operator()(Context &context, Value &value) const {

    using context_t = std::decay_t<decltype(context)>;

    return detail::run_if<context_t::is_loop, context_t::level + 1>(
        &value, tuple,
        [context](auto &value, std::exception_ptr ep, operation op) mutable {
          auto &f = context.f();

          if (ep && op == operation::_exception) {
            f(value, ep, op);
          } else if (op == operation::_break || op == operation::_continue ||
                     op == operation::_return) {
            f(value, ep, op);
          } else if (op == operation::_done) {
            detail::while_true_finished_helper<1, context_t>(f);
          }
        });
  };
};

template <class Pred, class Then, class Else>
auto make_if(Pred pred, Then t, Else e) {
  auto tup = make_block(
      [pred, t, e](auto &context, auto &value) {
        using context_t = std::decay_t<decltype(context)>;
        if (pred(value)) {
          detail::run_if<context_t::is_loop, context_t::level + 1>(&value, t,
                                                                   context);
        } else {
          detail::run_if<context_t::is_loop, context_t::level + 1>(&value, e,
                                                                   context);
        }
        return context.do_async();

      },
      [](auto &context, auto &value, auto &a, std::exception_ptr e, auto op) {
        if (op == operation::_done)
          return operation::_next;
        if (op == operation::_exception && e)
          context.f()(value, e, operation::_exception);
        return op;
      }

      );

  make_if_func_t<decltype(tup)> func{tup};

  return func;
}

template <class P, class... T> auto make_while(P p, T &&... t) {
  return make_while_true(
      [=](auto &context, auto &value) {
        if (p(value)) {
          return context.do_next();
        } else {
          return context.do_break();
        }
      },
      t...);
}

namespace detail {
template <class T> struct calculate_function_level;

template <class T> struct function_level {
  enum { value = 0 };
};

template <std::size_t... I> struct calc_max;

template <std::size_t First> struct calc_max<First> {
  enum { value = First };
};

template <std::size_t First, std::size_t Second, std::size_t... I>
struct calc_max<First, Second, I...> {
  enum { value_rest = calc_max<Second, I...>::value };
  enum { value = First > value_rest ? First : value_rest };
};

template <class... T> struct calculate_function_level<std::tuple<T...>> {
  enum { value = calc_max<function_level<T>::value...>::value };
};
template <class... T> struct calculate_function_level<const std::tuple<T...>> {
  enum {

    value = calc_max<function_level<T>::value...>::value
  };
};

template <class T> struct function_level<make_while_func_t<T>> {
  using inner_tuple = typename make_while_func_t<T>::tuple_t;
  enum { value = 1 + calculate_function_level<inner_tuple>::value };
};
template <class T> struct function_level<make_if_func_t<T>> {
  enum {
    value =
        2 + calculate_function_level<typename make_if_func_t<T>::tuple_t>::value
  };
};

template <class Value, std::size_t Size, std::size_t Levels>
struct value_t : Value {
  using Value::Value;

  std::array<std::aligned_storage_t<Size>, Levels>
      stackless_coroutine_finished_storage;
};

template <class Value, class Tuple, class FinishedTemp>
struct coroutine_holder {

  std::unique_ptr<Value> ptr;
  Tuple t;
  FinishedTemp f_temp;
  template <class... A> explicit operator bool() { return ptr.get(); }
  template <class... A> auto operator()(A &&... a) {
    return run(std::move(ptr), t, std::move(f_temp),
               std::forward<decltype(a)>(a)...);
  }
};
}
template <class Value, class Tuple, class FinishedTemp, class... A>
auto make_coroutine(const Tuple *t, FinishedTemp f_temp, A &&... a) {
  constexpr auto levels = 1 + detail::calculate_function_level<Tuple>::value;
  using f_t = detail::finished_wrapper<0, Value, Tuple, FinishedTemp,
                                       std::unique_ptr<Value>>;
  using f_t_h = detail::finished_tuple_holder<f_t, Tuple>;
  using v_t = detail::value_t<Value, sizeof(f_t_h), levels>;

  auto ptr = std::make_unique<v_t>(std::forward<A>(a)...);

  return detail::coroutine_holder<v_t, const Tuple *, FinishedTemp>{
      std::move(ptr), t, std::move(f_temp)};
}
}
