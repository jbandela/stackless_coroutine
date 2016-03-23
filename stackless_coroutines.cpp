#include <iostream>
#include <functional>
#include <memory>
#include <tuple>
#include <utility>
#include <type_traits>

namespace stackless_coroutine {
	enum class operation {
		_suspend,
		_next,
		_return,
		_break,
		_continue,
		_done
	};

	namespace detail {
		struct value_base {
			virtual ~value_base() {};

		};
		template<class T>
		struct value{
			T t_;
			template<class... A>
			value(A&&... a) :t_{ std::forward<A>(a)... } {}

		};
		template< class T >
		typename std::add_lvalue_reference<T>::type ldeclval();


	}

	template<class Base, class T>
	struct values : Base, T {
		void assign(T t) {
			*static_cast<T*>(this) = std::move(t);
		}
	
	
	};

	template<class Base>
	struct values<Base,void> : Base {};

	template<class T>
	struct values<void,T> : T {
		void assign(T t) {
			*static_cast<T*>(this) = std::move(t);
		}
	
	
	
	};

	template<>
	struct values<void,void>  {};




	template<class... T>
	struct async_result {
		using type = std::tuple<T...>;
	
	
	};

	template<class T>
	struct async_result<T> {
		using type = T;
	};

	template<class... T>
	using async_result_t = typename async_result<T...>::type;

	
	struct dummy_coroutine_context {

		template<class... T>
		void operator()(T&&... t) {
		}

		operation do_return() { return operation::_return; };
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue; };
		template<class... T>
		async_result<T...> do_async(){ return async_result<T...>{}; }

		dummy_coroutine_context() {}
		template<class T>
		dummy_coroutine_context(T&) {}



		

	};



	template<class CI>
	struct base_coroutine_context {
		CI* ci_;

		operation do_return() { return operation::_return; };
		base_coroutine_context(CI& ci) :ci_(&ci) {}
		};
	template<class Base>
	struct loop_coroutine_context:Base {
		using Base::Base;
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue };
	
	};
	template<class CI,class CP>
	struct async_coroutine_context:base_coroutine_context<CI> {
		using base_coroutine_context<CI>::base_coroutine_context;
		template<class... T>
		void operator()(T&&... t) {
			CP::process(*ci, ci_->value_, std::forward<T>(t)...);
		}
		template<class... T>
		async_result<T...> do_async(){ return async_result<T...>{}; }
	


	};

	template<class CI, class ReturnValue, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor;

	struct dummy_coroutine_processor {

	
		template<class CI, class value_type,class... T>
		static operation process(CI& ci, value_type& value, T&&... results) {
			return operation::_done;
		}

	};

	
	template<class CI, class ReturnValue, std::size_t Pos, std::size_t Size,bool Loop,bool Last = Pos == Size - 1>
	struct coroutine_next_processor_helper {
		using value_type = typename CI::value_type;
		using return_tuple = typename CI::return_tuple;

		using processor = coroutine_processor<CI, std::tuple_element_t<Pos + 1, return_tuple>, Pos + 1, Size,Loop>;

		
		
	};

	template<class CI, class ReturnValue,std::size_t Pos,  std::size_t Size, bool Loop>
	struct coroutine_next_processor_helper<CI,ReturnValue,Pos, Size,Loop,true> {

		using processor = dummy_coroutine_processor;

		
		
	};



	template<class CI,class CP,bool Async>
	struct async_coroutine_context_helper {
		using type = base_coroutine_context<CI>;

	};
	template<class CI,class CP>
	struct async_coroutine_context_helper<CI,CP,true> {
		using type = async_coroutine_context<CI, CP>;

	};

	template<class CI,class CP,bool Loop, bool Async>
	struct coroutine_context_helper {
		using type = typename async_coroutine_context_helper<CI, CP, Async>::type;

	};

	template<class CI,class CP,bool Async>
	struct coroutine_context_helper<CI,CP,true,Async> {
		using base = typename async_coroutine_context_helper<CI, CP, Async>::type;
		using type = loop_coroutine_context<base>;

	};

	template<class CI, class CP,bool Loop, bool Async>
	using coroutine_context = typename coroutine_context_helper<CI, CP, Loop, Async>::type;


	template<class CI, class ReturnValue, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor {
		using value_type = typename CI::value_type;
		using return_tuple = typename CI::return_tuple;
		using self = coroutine_processor;

		using next_processor = typename coroutine_next_processor_helper<CI, ReturnValue, Pos, Size,Loop>::processor;

	
		static operation do_next(CI& ci, value_type& value, std::integral_constant<std::size_t,Size - 1>) {
			return operation::_done;
		}
		template<class T>
		static operation do_next(CI& ci, value_type& value, T) {

			return coroutine_processor<CI, std::tuple_element_t<Pos + 1, return_tuple>, Pos + 1, Size>::process(ci, value);
		}
	
		template<class... T>
		static operation process(CI& ci, value_type& value, T&&... results) {
			coroutine_context<CI, CP, Loop, false> ctx{ ci };
			auto ret = std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)...);
			value.assign(std::move(ret));
			return do_next(ci, value, std::integral_constant<Pos + 1>{});
		}

	};
	template<class CI, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,void,Pos,Size,Loop> {
		using value_type = typename CI::value_type;
		using return_tuple = typename CI::return_tuple;

	
		static operation do_next(CI& ci, value_type& value, std::integral_constant<std::size_t,Size - 1>) {
			return operation::_done;
		}
		template<class T>
		static operation do_next(CI& ci, value_type& value, T) {

			return coroutine_processor<CI, std::tuple_element_t<Pos + 1, return_tuple>, Pos + 1, Size>::process(ci, value);
		}
	
		template<class... T>
		static operation process(CI& ci, value_type& value, T&&... results) {

			coroutine_context<CI, CP, Loop, false> ctx{ ci };
			std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)...);
			return do_next(ci, value, std::integral_constant<Pos + 1>{});
		}

	};
	template<class CI, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,operation,Pos,Size,Loop> {
		using value_type = typename CI::value_type;
		using return_tuple = typename CI::return_tuple;

	
		static operation do_next(CI& ci, value_type& value, std::integral_constant<std::size_t,Size - 1>) {
			return operation::_done;
		}
		template<class T>
		static operation do_next(CI& ci, value_type& value, T) {

			return coroutine_processor<CI, std::tuple_element_t<Pos + 1, return_tuple>, Pos + 1, Size>::process(ci, value);
		}
	
		template<class... T>
		static operation process(CI& ci, value_type& value, T&&... results) {

			coroutine_context<CI, CP, Loop, false> ctx{ ci };
			operation op = std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)... );
			if (op == operation::_break) {
				return operation::_next;
			}
			else if (op == operation::_continue) {
				return coroutine_processor<CI, std::tuple_element_t<0, return_tuple>, 0, Size>::process(ci, value);
			}
			else if (op == operation::_next) {
				return do_next(ci, value, std::integral_constant<Pos + 1>{});
			}
			else if (op == operation::_return) {
				return operation::_return;
			}
			else if (op == operation::_suspend) {
				return operation::_suspend;
			}
		}

	};

	template<class CI, class... A, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,async_result<A...>,Pos,Size,Loop> {
		using value_type = typename CI::value_type;
		using return_tuple = typename CI::return_tuple;

	
		static operation do_next(CI& ci, value_type& value, std::integral_constant<std::size_t,Size - 1>) {
			return operation::_done;
		}
		template<class T>
		static operation do_next(CI& ci, value_type& value, T) {

			return coroutine_processor<CI, std::tuple_element_t<Pos + 1, return_tuple>, Pos + 1, Size>::process(ci, value);
		}
	
		template<class... T>
		static operation process(CI& ci, value_type& value, T&&... results) {

			coroutine_context<CI, CP, Loop, true> ctx{ ci };
			auto ret = std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)...);
			value.assign(std::move(ret));
			return operation::_suspend;
		}

	};


	template<class... T>
	struct coroutine_info {
		using function_t = std::function<void(detail::value_base*)>;
		function_t function_;


		std::tuple<T...> tuple_;

		using lambda_tuple_t = std::tuple<T...>;

		enum { size = sizeof...(T) };
		enum { last_tuple_position= size - 1 };

		using self_t = coroutine_info;

		template<class... A>
		coroutine_info(A&&... a) :tuple_{ std::forward<A>(a)... } {}

		template<class R, class Previous>
		struct process_return_type {
			using type = values<Previous, R>;
		};

		template<class Previous>
		struct process_return_type<operation, Previous> {
			using type = Previous;

		};
		template<class Previous>
		struct process_return_type<void, Previous> {
			using type = Previous;
		};
		template<class Previous, class... T>
		struct process_return_type<async_result<T...>, Previous> {
			using type = Previous;
		};

		template<class A, class B>
		struct append_tuple;

		template<class... A, class B>
		struct append_tuple<std::tuple<A...>, B> {
			using type = std::tuple<A..., B>;
		};

		template<class A, class B>
		using append_tuple_t = typename append_tuple<A, B>::type;

		template<class LastReturn, class CurrentValue,class ReturnTuple, std::size_t Pos,bool>
		struct calculate_values_type{
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>()));
			using current_type = typename process_return_type<return_type, CurrentValue>::type;
			using current_tuple_type = append_tuple_t<ReturnTuple, return_type>;
			using next = calculate_values_type<return_type, current_type, current_tuple_type, Pos + 1,Pos+1 == last_tuple_position>;

			using type = typename next::type;
			using tuple_type = typename next::tuple_type;
		};
		template<class... A,class  CurrentValue,class ReturnTuple, std::size_t Pos>
		struct calculate_values_type<async_result<A...>,CurrentValue,ReturnTuple,Pos,false> {
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>(),std::declval<A>()... ));
			using current_type = typename process_return_type<return_type, CurrentValue>::type;
			using current_tuple_type = append_tuple_t<ReturnTuple, return_type>;
			using next = calculate_values_type<return_type, current_type, current_tuple_type, Pos + 1,Pos + 1  == last_tuple_position>;

			using type = typename next::type;
			using tuple_type = typename next::tuple_type;
		};
		template<class LastReturn,class  CurrentValue,class ReturnTuple,std::size_t Pos>
		struct calculate_values_type<LastReturn,CurrentValue,ReturnTuple,Pos,true>{
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>()));
			using type = typename process_return_type<return_type, CurrentValue>::type;
			using tuple_type = append_tuple_t<ReturnTuple, return_type>;
		};
		template<class... A, class  CurrentValue, class ReturnTuple, std::size_t Pos>
		struct calculate_values_type<async_result<A...>, CurrentValue, ReturnTuple, Pos, true> {
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>(),std::declval<A>()...  ));
			using type = typename process_return_type<return_type, CurrentValue>::type;
			using tuple_type = append_tuple_t<ReturnTuple, return_type>;

		};



		using cv_t = calculate_values_type<void, values<void, void>, std::tuple<>, 0,0 == last_tuple_position>;

		using value_type = typename cv_t::type;
		using return_tuple = typename cv_t::tuple_type;
		


		







	};

	template<class... T>
	auto make_coroutine_info(T&&... t) {
		return coroutine_info<std::decay_t<T>...>{std::forward<T>(t)...};
	}




};


int main() {
	auto ci = stackless_coroutine:: make_coroutine_info(
		
		[](auto& a, auto& b) {
		struct val {
			int x;
			int y;
		};

		return val{ 0,1 };


		},
		
		[](auto& a, auto& b) {
			struct val {
				int z;
			};
			return val{ 3 };
		},
		[](auto& a, auto& b) {
			return stackless_coroutine::async_result<int>();

		},
		[](auto& a, auto& b,int v) {
			return stackless_coroutine::async_result<int,int>();

		},
		[](auto& a, auto& b, int x, int y) {
			

		}


		);

	decltype(ci)::value_type vt;
	vt.x = 5;
	vt.z = 2;

	
	std::cout << typeid(vt).name() << std::endl;
};