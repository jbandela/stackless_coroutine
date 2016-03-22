#include <iostream>
#include <functional>
#include <memory>
#include <tuple>
#include <utility>
#include <type_traits>

namespace stackless_coroutine {
	enum class operation {
		_suspend,
		_yield,
		_break,
		_continue,
		_return
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
	struct values : Base, T {};

	template<class Base>
	struct values<Base,void> : Base {};

	template<class T>
	struct values<void,T> : T {};

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

	

	template<class T>
	struct coroutine_context {
		std::shared_ptr<T> ptr_;

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
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(std::declval<async_result_t<A...>>(),detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>()));
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
			using return_type = decltype(std::get<Pos>(std::declval<lambda_tuple_t>())(std::declval<async_result_t<A...>>(), detail::ldeclval<coroutine_info>(), detail::ldeclval<CurrentValue>()));
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
		[](int v,auto& a, auto& b) {
			return stackless_coroutine::async_result<int,int>();

		},
		[](std::tuple<int,int> v,auto& a, auto& b) {
			

		}


		);

	decltype(ci)::value_type vt;
	vt.x = 5;
	vt.z = 2;

	
	std::cout << typeid(vt).name() << std::endl;
};