// Copyright 2016 John R. Bandela
// Copyright 2017 Google Inc.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#pragma once
#include <algorithm>
#include <array>
#include <assert.h>
#include <iterator>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#ifdef STACKLESS_COROUTINE_NO_EXCEPTIONS
namespace stackless_coroutine {
using exception_ptr = void *;
}
#else
#include <exception>
namespace stackless_coroutine {
using exception_ptr = std::exception_ptr;
}
#endif

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

	template<class Variables>
	struct dummy_coroutine_context {

		template <class... T> operation operator()(T &&... t) { return operation::_continue; }

		operation do_return() { return operation::_return; };
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue; };
		operation do_next() { return operation::_next; };
		async_result do_async() { return async_result{}; }
		template <class Value> async_result do_async_yield(Value v) {
			return async_result{};
		}
		async_result do_async_return() { return async_result{}; }
		async_result do_async_break() { return async_result{}; }
		async_result do_async_continue() { return async_result{}; }
		dummy_coroutine_context() {}
		dummy_coroutine_context(const dummy_coroutine_context &) {}
		dummy_coroutine_context(dummy_coroutine_context &&) {}
		template <class T> dummy_coroutine_context(T &) {}

		template <class V> static dummy_coroutine_context get_context(V *v) { return{}; }

		Variables& variables(){return *pv;}

		Variables* pv;

	};
	template<class Variables>
	struct void_coroutine_context {
		Variables* pv;

		Variables& variables() { return *pv; };

	};
	template<class Variables>
	struct operation_coroutine_context {


		operation do_return() { return operation::_return; };
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue; };
		operation do_next() { return operation::_next; };

		Variables* pv;

		Variables& variables() { return *pv; };


	};



	template<class T> struct type2type { using type = T; };

	template<class F>
	struct if_else_helper_base {
		F& f_;
		template<class... A>
		auto operator()(A&&... a) {
			return f_(std::forward<A>(a)...);
		}
	};

	template<bool b, class F1, class F2>
	struct if_else_helper:if_else_helper_base<F2> {
		if_else_helper(F1&, F2& f2) :if_else_helper_base<F2>{ f2 } {}
	};
	template<class F1, class F2>
	struct if_else_helper<true, F1, F2>:if_else_helper_base<F1>  {


		if_else_helper(F1& f1, F2&) :if_else_helper_base<F1>{ f1 } {}

	};
	template<bool b, class F1, class F2, class... A>
	auto if_else_(F1 f1, F2 f2, A&&... a) {
		if_else_helper<b, F1, F2> helper{ f1,f2 };
		return helper(std::forward<A>(a)...);

	}

	template<int N, int... Ns>
	auto append_sequence(std::integer_sequence<int, Ns...>) {
		return std::integer_sequence<int, Ns..., N>{};
	}



	template<class Tuple>
	struct while_t {
		Tuple t;
	};
	template<class F,class Tuple, int Else>
	struct if_t {
		F f;
		Tuple t;
		enum{else_start = Else};
	};

   	


	template<class T>
	struct is_while_t { enum { value = 0 }; };

	template<class T>
	struct is_while_t<while_t<T>> {
		enum {
			value = 1
		};
	};




	template<int N, class... Ts>
	const auto& get_function(const std::tuple<Ts...>& t) {
		return std::get<N>(t);
	}
	template<int N, class Tuple>
	const auto& get_function(const while_t<Tuple>& t) {
		return get_function<N>(t.t);
	}
	template<int N, class F,class Tuple, int Else>
	const auto& get_function(const if_t<F,Tuple,Else>& t) {
		return get_function<N>(t.t);
	}



	template<class T>struct  get_size_helper;

	template<class... T>
	struct get_size_helper<std::tuple<T...>> {
		enum { size = sizeof...(T) };
	};
	template<class...T>
	struct get_size_helper<while_t<std::tuple<T...>>> {
		enum { size = sizeof...(T) };
	};

	template<class F,class Tuple, int Else>
	struct get_size_helper<if_t<F,Tuple,Else>> {
		enum { size = std::tuple_size<Tuple>::value };
	};


	template<class T>
	constexpr int get_size_v = get_size_helper<T>::size;




	struct processor {

		struct while_type {};
		struct if_type {};
		template<class Variables, class Function, class... A>
		static auto get_return(Variables& vars, Function& f, A&&... a) {


			dummy_coroutine_context<Variables> dc;
			return type2type<decltype(f(dc, vars, std::forward<A>(a)...)) > {};
		}
		template<class Variables, class WhileTuple>
		static auto get_return(Variables& , const while_t<WhileTuple>&) {

			
			return type2type<while_type>{};
		}
		template<class Variables, class IfF, class IfTuple, int IfElse>
		static auto get_return(Variables&, const if_t<IfF,IfTuple,IfElse>& ift) {


			return type2type<if_type>{};
		}


		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables>
		static operation process_next(operation op, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars) {
			if (op == operation::_next) {
				return if_else_<N < get_size_v<Tuple> - 1>(
					[&](auto& t) ->operation {

					auto next_return = get_return(vars, get_function<N + 1>(t));
					return process<N + 1>(next_return, function_level_tuple, outer_sequence, t, vars);

				},
					[](auto& t) {return operation::_done; },
					t);


			}
			else {
				return op;
			}


		}


		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process(type2type<void>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
                auto func = [&](auto&&... a) {
				get_function<N>(t)(std::forward<decltype(a)>(a)...); 
				return operation::_next; };

				void_coroutine_context<Variables> vc{&vars};
			auto op = func(
				vc,vars, std::forward<Args>(args)...);
			return process_next<N>(op, function_level_tuple, outer_sequence, t, vars);
		}

		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process(type2type<operation>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			operation_coroutine_context<Variables> oc{ &vars };
			auto op = get_function<N>(t)( oc, vars, std::forward<Args>(args)...);
			return process_next<N>(op, function_level_tuple, outer_sequence, t, vars);
		}
		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process(type2type<while_type>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			process_outermost_block(append_sequence<0>(append_sequence<N>(outer_sequence)), function_level_tuple, vars, std::forward<Args>(args)...);
			return operation::_suspend;
		}
		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process(type2type<if_type>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			const auto& ift = get_function<N>(t);
			using ift_type = std::decay_t<decltype(ift)>;
			operation op;
			if (ift.f(vars)) {
				op = process_block(std::integer_sequence<int, 0>{}, function_level_tuple, append_sequence<N>(outer_sequence), ift.t, vars);
			}
			else {
				op = process_block(std::integer_sequence<int, ift_type::else_start >{}, function_level_tuple, append_sequence<N>(outer_sequence), ift.t, vars);
			}
			if (op == operation::_done) { op = operation::_next; }
			return process_next<N>(op, function_level_tuple, outer_sequence, t, vars);
		}


		template<class Sequence, class Variables>
		struct async_coroutine_context {
			Variables* pv;
			template <class... A> void operator()(A &&... a) {
				process_outermost_block(Sequence{}, *pv->stackless_coroutine_tuple, *pv, std::forward<A>(a)...);
			}

			async_result do_async() { return async_result{ operation::_suspend }; }
			template <class Value> async_result do_async_yield(Value v) {
				variables().stackless_coroutine_set_generator_value(std::move(v));
				variables().stackless_coroutine_set_generator_next(*this);
				return do_async();
			}
 
			async_result do_async_return() { return async_result{ operation::_return }; }
			async_result do_async_break() { return async_result{ operation::_break }; }
			async_result do_async_continue() { return async_result{ operation::_continue }; }

			template <class V> static async_coroutine_context get_context(V *v) { return async_coroutine_context{ static_cast<Variables*>(v) }; }

			Variables& variables() { return *pv; };



		};
		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process(type2type<async_result>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			async_coroutine_context<decltype(append_sequence<N + 1>(outer_sequence)),Variables> ac{ &vars };
			async_result op = get_function<N>(t)(ac,vars, std::forward<Args>(args)...);
			return op.op;
		}



		template<int N, int Next, int... Rest, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process_block(std::integer_sequence<int, N, Next, Rest...>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			operation op;

			op = process_block(std::integer_sequence<int, Next, Rest...>{}, function_level_tuple, append_sequence<N>(outer_sequence), get_function<N>(t), vars, std::forward<Args>(args)...);

			using continue_sequence = std::integer_sequence<int, 0>;

			op = if_else_<is_while_t<std::decay_t<decltype(get_function<N>(t))>>::value>([&](auto& t) {
				operation lop = op;
				while (lop == operation::_continue) {

					lop = process_block(continue_sequence{}, function_level_tuple, append_sequence<N>(outer_sequence), get_function<N>(t), vars);
				}
				if (lop == operation::_break) { lop = operation::_done; }
				return lop;

			}, [&](auto&) {return op; }, t);

			if (op == operation::_done) {
				return if_else_ < (N < get_size_v<Tuple> - 1)>([&](auto& t) {
					return process_block(std::integer_sequence<int, N + 1>{}, function_level_tuple, outer_sequence, t, vars);
				},
					[] {return operation::_done; }, t);

			}
			return op;
		}
		template<int N, class FunctionLevelTuple, class OuterSequence, class Tuple, class Variables, class... Args>
		static operation process_block(std::integer_sequence<int, N>, FunctionLevelTuple& function_level_tuple, OuterSequence outer_sequence, const Tuple& t, Variables& vars, Args&&... args) {
			auto return_type = get_return(vars, get_function<N>(t), std::forward<Args>(args)...);
			return process<N>(return_type, function_level_tuple, outer_sequence, t, vars, std::forward<Args>(args)...);
		}

		template<int N, int... Rest, class FunctionLevelTuple, class Variables, class... Args>
		static void process_outermost_block(std::integer_sequence<int, N, Rest...> seq, FunctionLevelTuple& function_level_tuple, Variables& vars, Args&&... args) {

			operation op;
			exception_ptr eptr{ nullptr };
#ifndef STACKLESS_COROUTINE_NO_EXCEPTIONS
			try {
#endif

				op = process_block(seq, function_level_tuple, std::integer_sequence<int>{}, function_level_tuple, vars, std::forward<Args>(args)...);
#ifndef STACKLESS_COROUTINE_NO_EXCEPTIONS
			}
			catch (...) {
				eptr = std::current_exception();
				op = operation::_exception;

		}
#endif
			if (op == operation::_done || op == operation::_return || op == operation::_exception) {
				vars.stackless_coroutine_callback_function(vars, eptr, op);
			}
		}




	};



	template<class Destroyer>
	struct finished_wrapper_impl {
		template <class Variables, class... A> auto operator()(Variables& v, A &&... a) {
			Destroyer d{ &v };
			(void)d;
			return v.stackless_coroutine_callback_function(v, std::forward<A>(a)...);
		}
	};
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




}

template <class... T> auto make_while_true(T &&... t) {
	const auto dummy_while_terminator = [](auto &, auto &) {
		return operation::_continue;
	};
	auto tuple =
      std::make_tuple(std::forward<T>(t)..., dummy_while_terminator);
  return detail::while_t<decltype(tuple)>{ std::move(tuple) };

};
template <class F, class... T> auto make_while(F f,T &&... t) {

	auto condition = [f = std::move(f)](auto& context, auto& vars){
		if (!f(vars)) {
			return context.do_break();
		}
		else {
			return context.do_next();
		}
	};
	return make_while_true(std::move(condition), std::forward<T>(t)...);
};


template<class F, class T1, class T2>
auto make_if(F&& f, T1&& t1, T2&& t2) {
	auto dummy_done = [](auto&, auto&) {return operation::_done; };

	auto combined = std::tuple_cat(std::forward<T1>(t1), std::make_tuple(dummy_done), std::forward<T2>(t2), std::make_tuple(dummy_done));
	using combined_type = decltype(combined);

	return detail::if_t<std::decay_t<F>, std::decay_t<combined_type>, std::tuple_size<T1>::value + 1>{std::forward<F>(f), std::move(combined)};
}

namespace detail {
	template <class Variables, class Tuple, class Callback>
	struct variables_t : Variables {
		template <class... A> variables_t(const Tuple* tup, Callback callback, A&&... a) : Variables{ std::forward<A>(a)... }, stackless_coroutine_tuple{ tup },
			stackless_coroutine_callback_function{ std::move(callback) } {}

		const Tuple* stackless_coroutine_tuple;
		Callback stackless_coroutine_callback_function;
	};

}
template <class Variables, class Tuple, class Callback, class... A>
auto make_stack_variables(const Tuple* block, Callback cb, A&&... a) {

	return detail::variables_t<Variables, Tuple,Callback>{block, std::move(cb), std::forward<A>(a)...};
};


template <class Variables, class Tuple, class Callback, class... A>
auto make_unique_ptr_variables(const Tuple* block, Callback cb, A&&... a) {


	auto real_callback = [cb = std::move(cb)](auto& variables, auto ep, auto op)mutable{
		std::unique_ptr<std::decay_t<decltype(variables)>> destroyer{ &variables };
		cb(variables, ep, op);
	};

	return std::make_unique<detail::variables_t<Variables, Tuple,decltype(real_callback)>>(block, std::move(real_callback), std::forward<A>(a)...);
};

template<class UVar>
struct coroutine_holder{
	UVar variables;
 template <class... A> auto operator()(A &&... a) {
	 auto vars = variables.release();
	 return detail::processor::process_outermost_block(std::integer_sequence<int, 0>{}, *vars->stackless_coroutine_tuple, *vars, std::forward<A>(a)...);
  }
};

template <class Variables, class Tuple, class Callback, class... A>
auto make_coroutine(const Tuple *t, Callback cb, A &&... a) {
	auto ptr = make_unique_ptr_variables<Variables>(t, std::move(cb), std::forward<A>(a)...);
	return coroutine_holder<decltype(ptr)>{std::move(ptr)};
}

// Generator

namespace detail {

using stackless_coroutine_function_ptr_t = operation (*)(void *);
template <class Generator, class Value, class Variables, class Block>
struct generator_imp {
  struct generator_variables_t : Variables {
    Generator *stackless_coroutine_generator_ = nullptr;

    void stackless_coroutine_set_generator_value(Value v) {
      stackless_coroutine_generator_->generator_value_ = std::move(v);
    }

    template <class Context>
    void stackless_coroutine_set_generator_next(Context context) {
      stackless_coroutine_generator_->next_func_ = [](void *v) {
        auto context = Context::get_context(v);
        context();
		return operation::_next;
      };
      stackless_coroutine_generator_->close_func_ = [](void *v) {
        auto context = Context::get_context(v);
        context.variables().stackless_coroutine_callback_function(*static_cast<generator_variables_t *>(v), nullptr,
                    operation::_return);
        ;
        return operation::_return;
      };
    }

    template <class... T>
    generator_variables_t(Generator *g, T &&... t)
        : Variables{std::forward<T>(t)...}, stackless_coroutine_generator_{g} {}
  };

  template <class... T> static void create(Generator *g, Block b, T &&... t) {
    auto co = stackless_coroutine::make_coroutine<generator_variables_t>(
        b,
        [](auto &variables, exception_ptr ep, operation op) mutable {
          auto g = static_cast<generator_variables_t *>(&variables)
                       ->stackless_coroutine_generator_;
          g->generator_variables_ = nullptr;
          g->next_func_ = nullptr;
          g->close_func_ = nullptr;
          g->move_func_ = nullptr;
        },
        g, std::forward<T>(t)...);
    g->generator_variables_ = co.variables.get();
    g->move_func_ = [](void *v, Generator *g) {
      static_cast<generator_variables_t *>(v)->stackless_coroutine_generator_ =
          g;
    };
    co();
  }
};
}

template <class Value> class generator {

public:
  using stackless_coroutine_move_t = void (*)(void *, generator<Value> *);
  Value generator_value_;

  detail::stackless_coroutine_function_ptr_t next_func_ = nullptr;
  detail::stackless_coroutine_function_ptr_t close_func_ = nullptr;
  stackless_coroutine_move_t move_func_ = nullptr;

  void *generator_variables_ = nullptr;

public:
  bool valid() const {
    assert(!(generator_variables_ && next_func_ == nullptr));
    return generator_variables_ != nullptr;
  }
  void next() {
    auto vf = next_func_;
    next_func_ = nullptr;
    vf(generator_variables_);
  }

  class iterator : public std::iterator<std::input_iterator_tag, Value> {
    generator<Value> *gen_;

    void set_gen() {
      if (!gen_)
        return;
      if (!*gen_) {
        gen_ = nullptr;
        return;
      }
    }

    void next() {
      gen_->next();
      set_gen();
    }

    class proxy {
      Value val_;

    public:
      proxy(Value v) : val_{std::move(v)} {};
      Value &operator*() { return val_; }
      const Value &operator*() const { return val_; }
    };

  public:
    iterator(generator<Value> *g = nullptr) : gen_{g} { set_gen(); }

    Value &operator*() { return gen_->value(); }
    const Value &operator*() const { return gen_->value(); }

    bool operator==(const iterator &other) const { return gen_ == other.gen_; }

    bool operator!=(const iterator &other) const { return gen_ != other.gen_; }

    iterator &operator++() {
      next();
      return *this;
    }

    proxy operator++(int) {
      proxy ret(*(*this));
      next();
      return ret;
    }
  };

public:
  explicit operator bool() { return valid(); }

  Value &value() { return generator_value_; }

  generator() = default;
  generator(generator &&other) { (*this) = std::move(other); }
  generator &operator=(generator &&other) {
    close_func_ = other.close_func_;
    other.close_func_ = nullptr;

    generator_value_ = std::move(other.generator_value_);

    generator_variables_ = other.generator_variables_;
    other.generator_variables_ = nullptr;

    move_func_ = other.move_func_;
    other.move_func_ = nullptr;

    next_func_ = other.next_func_;
    other.next_func_ = nullptr;

    if (move_func_) {
      move_func_(generator_variables_, this);
    }
    return *this;
  }

  generator(const generator &) = delete;
  generator &operator=(const generator &) = delete;

  ~generator() {
    if (close_func_) {
      assert(generator_variables_ != nullptr && next_func_ != nullptr);
      close_func_(generator_variables_);
    }
  }

  iterator begin() { return iterator{this}; }
  iterator end() { return iterator{}; }
};

template <class Value, class Variables, class Block, class... T>
generator<Value> make_generator(Block b, T &&... t) {

  generator<Value> g;
  detail::generator_imp<generator<Value>, Value, Variables, Block>::create(
      &g, b, std::forward<T>(t)...);
  return g;
}

}
