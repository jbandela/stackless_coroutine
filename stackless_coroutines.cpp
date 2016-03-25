#include <iostream>
#include <functional>
#include <memory>
#include <tuple>
#include <utility>
#include <type_traits>
#include <atomic>
#include <exception>

namespace stackless_coroutine {
	enum class operation {
		_suspend,
		_next,
		_return,
		_break,
		_continue,
		_done,
		_exception
	};
	const char* operations[] ={
		"_suspend",
		"_next",
		"_return",
		"_break",
		"_continue",
		"_done",
		"_exception"
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

	template<class... T>
	struct values : T... {

	
	};

	template<class...V,class... T>
	struct values<values<V...>,T...> : V...,T... {

	
	
	};





	struct async_result {
	
	};

	

	template<class CI, class F,std::size_t Pos>
	struct dummy_coroutine_context {

		template<class... T>
		void operator()(T&&... t) {
		}

		operation do_return() { return operation::_return; };
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue; };
		template<class... T>
		async_result do_async(){ return async_result{}; }

		dummy_coroutine_context() {}
		template<class T>
		dummy_coroutine_context(T&) {}

		CI* ci_;
		F* f_;
		enum { position = Pos };



		

	};



	template<class CI,class Finished, size_t Pos>
	struct base_coroutine_context {
		
		enum { position = Pos };
		CI* ci_;
		Finished* f_;

		operation do_return() { return operation::_return; };
		base_coroutine_context(CI& ci) :ci_(&ci) {}
		};
	template<class Base>
	struct loop_coroutine_context:Base {
		using Base::Base;
		operation do_break() { return operation::_break; }
		operation do_continue() { return operation::_continue; }
	
	};
	//template<class CI,class CP>
	//struct async_coroutine_context:base_coroutine_context<CI> {
	//	using base_coroutine_context<CI>::base_coroutine_context;
	//	template<class... T>
	//	void operator()(T&&... t) {
	//		CP::process(*this->ci_, this->ci_->value_, std::forward<T>(t)...);
	//	}
	//	template<class... T>
	//	async_result do_async(){ return async_result{}; }
	//


	//};

	template<class CI, class ReturnValue, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor;

	


	template<class CI,class CP,class Finished,bool Async>
	struct async_coroutine_context_helper {
		using type = base_coroutine_context<CI,Finished,CP::position>;

	};

	template<class CI,class CP,class Finished,bool Loop, bool Async>
	struct coroutine_context_helper {
		using type = typename async_coroutine_context_helper<CI, CP, Finished,Async>::type;

	};

	template<class CI,class CP,class Finished,bool Async>
	struct coroutine_context_helper<CI,CP,Finished,true,Async> {
		using base = typename async_coroutine_context_helper<CI, CP,Finished, Async>::type;
		using type = loop_coroutine_context<base>;

	};

	template<class CI, class CP,class Finished,bool Loop, bool Async>
	using coroutine_context = typename coroutine_context_helper<CI, CP,Finished, Loop, Async>::type;


	template<class T>
	void assign_helper(T& v, T t) {
		v = std::move(t);
	}


	template<class CI, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,void,Pos,Size,Loop> {
		enum { position = Pos };
		using value_type = typename CI::value_type;

		using self = coroutine_processor;
	
		template<class Finished>
		static operation do_next(CI& ci, value_type& value,Finished& , std::integral_constant<std::size_t,Size>) {
			return operation::_done;
		}
		template<class Finished,std::size_t P>
		static operation do_next(CI& ci, value_type& value,Finished& f, std::integral_constant<std::size_t,P>) {

			dummy_coroutine_context<CI,Finished,P> dc;
			using next_return = decltype(std::get<P>(ci.tuple_)(dc, ci.value_));

			return coroutine_processor<CI, next_return, P, Size,Loop>::process(ci, value,f);
		}
	
		template<class Finished,class... T>
		static operation process(CI& ci, value_type& value,Finished& f, T&&... results) {

			coroutine_context<CI, self,Finished, Loop, false> ctx{ ci };
			std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)...);
			return do_next(ci, value,f, std::integral_constant<std::size_t,Pos + 1>{});
		}

	};
	template<class CI, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,operation,Pos,Size,Loop> {

		enum { position = Pos };
		using value_type = typename CI::value_type;

	
		using self = coroutine_processor;
	
		template<class Finished>
		static operation do_next(CI& ci, value_type& value,Finished& f, std::integral_constant<std::size_t,Size>) {
			return operation::_done;
		}
		template<class Finished,std::size_t P>
		static operation do_next(CI& ci, value_type& value,Finished& f, std::integral_constant<std::size_t,P>) {
			dummy_coroutine_context<CI,Finished,P> dc;
			using next_return = decltype(std::get<P>(ci.tuple_)(dc, ci.value_));


			return coroutine_processor<CI, next_return, Pos + 1, Size,Loop>::process(ci, value,f);
		}
	
		template<class Finished,class... T>
		static operation process(CI& ci, value_type& value,Finished& f, T&&... results) {

			coroutine_context<CI, self, Finished,Loop, false> ctx{ ci };
			operation op = std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)... );
			if (op == operation::_break) {
				return operation::_break;
			}
			else if (op == operation::_continue) {
				return operation::_continue;
			}
			else if (op == operation::_next) {
				return do_next(ci, value,f, std::integral_constant<std::size_t,Pos + 1>{});
			}
			else if (op == operation::_return) {
				return operation::_return;
			}
			else if (op == operation::_suspend) {
				return operation::_suspend;
			}
			throw std::logic_error{ "Invalid stackless_coroutine::operation" };
		}

	};

	template<class CI, std::size_t Pos, std::size_t Size,bool Loop>
	struct coroutine_processor<CI,async_result,Pos,Size,Loop> {

		enum { position = Pos };

		using self = coroutine_processor;

	

		template<class Finished,class... T>
		static operation process(CI& ci, typename CI::value_type& value,Finished& f, T&&... results) {
				auto ctx = [ci = &ci,f = std::move(f)](auto&&... a)mutable {
					dummy_coroutine_context<CI,Finished,Pos+1> dc;
					using next_return = decltype(std::get<Pos + 1>(ci->tuple_)(dc, ci->value_, std::forward<decltype(a)>(a)...));

					using CP =  coroutine_processor<CI, next_return, Pos + 1, Size,Loop>;
					coroutine_context<CI, CP, Finished,Loop, true> ctx{ *ci };

					
					operation op;
					try {
						op = CP::process(*ci, ci->value_,f, std::forward<decltype(a)>(a)...);
					}
					catch (...) {
						auto ep = std::current_exception();
						f(ci->value_, ep,true,operation::_exception);
						return operation::_done;
					};

					if (op == operation::_done || op == operation::_return) {
						f(ci->value_, nullptr,true,op);
					}
					return op;
				};


	
			std::get<Pos>(ci.tuple_)(ctx, value,std::forward<T>(results)...);
			return operation::_suspend;
		}

	};


	template<class ValueType,bool Loop,class... T>
	struct coroutine{
		using value_type = ValueType;
		value_type value_;


		std::tuple<T...> tuple_;

		using lambda_tuple_t = std::tuple<T...>;

		enum { size = sizeof...(T) };
		enum { last_tuple_position= size - 1 };

		using self_t = coroutine;

		template<class V, class... A>
		coroutine(V v, A&&... a) :value_{ std::move(v) }, tuple_ { std::forward<A>(a)... } {}

		template<class V, class... A>
		coroutine(V v, std::tuple<A...> a) :value_{ std::move(v) }, tuple_ { std::move(a) } {}




		template<class Finished,class... A>
		auto run(Finished f,A&&... a) {

			dummy_coroutine_context<self_t,Finished,0> dc;
			using ret_type = decltype(std::get<0>(tuple_)(dc, value_, std::forward<A>(a)...));
			
			operation op;
			try {
				op = coroutine_processor<self_t, ret_type, 0, size, false>::process(*this, value_,f, std::forward<A>(a)...);
			}
			catch (...) {
				auto ep = std::current_exception();
				f(value_, ep,false,operation::_exception);

			};

			if (op == operation::_done || op == operation::_return) {
				f(value_, nullptr,false,op);
			}
			return op;
	}

		value_type& value() { return value_; }
		


		







	};



	template<class ValueFunc,class... T>
	auto make_coroutine(ValueFunc&& vf,T&&... t) {

			
		auto dummy_terminator = [](auto&&...) {};
			return coroutine<std::decay_t<decltype(vf())>, false, std::decay_t<T>...,decltype(dummy_terminator)> { vf(),  std::forward<T>(t)...,dummy_terminator };

		
	}


	namespace detail {

		template<class T, class OuterValue>
		struct while_value :T {
			T* value_;
			OuterValue* outer_;
			OuterValue* outer_most_;

			auto& outer_value() { return *outer_; }
			auto& outer_most_value() { return *outer_most_; }
			auto& value() { return *value_; }

			while_value(T* t, OuterValue* outer) :value_{}, outer_{ outer }, outer_most_{ outer } {}

		};
		template<class T, class OuterT, class OuterOuterValue>
		struct while_value<T,while_value<OuterT,OuterOuterValue>> :T {
			using OuterValue = while_value<OuterT, OuterOuterValue>;
			T* value_;
			OuterValue* outer_;
			OuterValue* outer_most_;

			auto& outer_value() { return *outer_; }
			auto& outer_most_value() { return *outer_most_; }
			auto& value() { return *value_; }

			while_value(T* t, OuterValue* outer) :value_{t}, outer_{ outer }, outer_most_{ outer->outer_most_ } {}

		};
	
		template<class value_type,class V, class OV,class... T>
		auto get_while_coroutine(V& value,OV& outer_value,T&... t) {
			return coroutine<value_type, true, T...> { value_type{ &value,&outer_value },  std::tie(t...) };
		}


	}
	template<class ValueFunc, class...T>
	auto  make_while_true(ValueFunc vf, T&&... t) {

		auto dummy_terminator = [](auto&&...) {};
		auto func = [vf = std::move(vf), t...,dummy_terminator, value = decltype(vf()){}](auto& context, auto& outer_value)mutable->operation {
			using value_type = detail::while_value<std::decay_t<decltype(value)>, std::decay_t<decltype(outer_value)>>;


			value = vf();

			auto finished = [context, pouter = &outer_value](auto& value, std::exception_ptr ep, bool async,operation op) mutable {
				if (async) {

					auto constexpr size = std::tuple_size < std::decay_t<decltype(context.ci_->tuple_)>>::value;
					using CI = std::decay_t<decltype(*context.ci_) > ;
					using F = std::decay_t<decltype(*context.f_)>;

					constexpr auto pos = std::decay_t<decltype(context)>::position;
					using DC = dummy_coroutine_context<CI,F,pos>;

					if (ep && op == operation::_exception) {
						(*context.f_)(*pouter, ep, true, operation::_exception);
					}
					else if (op == operation::_break) {


					constexpr auto pos = std::decay_t<decltype(context)>::position + 1;
						using DC = dummy_coroutine_context<CI,F,pos>;
						DC dc;
						using ret_type = decltype(std::get<pos>(context.ci_->tuple_)(dc, *pouter));

						operation op;
						try {
							op = coroutine_processor<std::decay_t<decltype(*context.ci_)>, ret_type, pos,size , false>::process(*context.ci_, context.ci_->value_, *context.f_);
						}
						catch (...) {
							auto ep = std::current_exception();
							(*context.f_)(context.ci_->value_, ep, true, operation::_exception);
						};
						if (op == operation::_done || op == operation::_return) {
							(*context.f_)(value.outer_value(), nullptr, true, op);
						}

					}

					else if (op == operation::_done || op == operation::_continue) {


						DC dc;
						using ret_type = decltype(std::get<pos>(context.ci_->tuple_)(dc, *pouter));



						operation op;
						try {
							op = coroutine_processor<std::decay_t<decltype(*context.ci_)>, ret_type, pos, size, false>::process(*context.ci_, context.ci_->value_, *context.f_);
						}
						catch (...) {
							auto ep = std::current_exception();
							(*context.f_)(context.ci_->value_, ep, true, operation::_exception);
						};
						if (op == operation::_done || op == operation::_return) {
							(*context.f_)(value.outer_value(), nullptr, true, op);
						}

					}
	
					else if(op == operation::_return){

						(*context.f_)(value.outer_value(), ep, true, operation::_return);
					
					}

					// don't have to do anything for operation::_next, and operation::_suspend


				}
				return op;

			};

			while (true) {
				//coroutine<value_type, true, std::decay_t<decltype(t)>...> c{ value_type{&value,&outer_value},  std::tie(t...) };
				auto c = detail::get_while_coroutine<value_type>(value, outer_value, t...,dummy_terminator);
				operation op;
				try {
					op = c.run(finished);

					// reuse finished
					if (op == operation::_break) {
						return operation::_next;
					}
					else if (op == operation::_exception) {
						return op;
					}
					else if (op == operation::_return) {
						return op;
					}
					else if (op == operation::_suspend) {
						return op;
					}
	
					else if (op == operation::_done) {
						continue;
					}
				}
				catch (...) {
					auto ep = std::current_exception();
					return finished(c.value_, ep, true, operation::_exception);
				}
			}


		};

		return func;









	};





};


int main() {

  auto ci = stackless_coroutine::make_coroutine(

      []() {
        struct val {
          int x;
          int y;
          int z;
        };

        return val{0, 1, 2};

      },
      [](auto &a, auto &b) {
        //			return stackless_coroutine::operation::_return;
      },
      [](auto &a, auto &b) {
        a(1);
        return stackless_coroutine::async_result();

      },
      [](auto &a, auto &b, int v) {
        a(1, 2);
        return stackless_coroutine::async_result();

      },
      [](auto &a, auto &b, int x, int y) {
        b.y = b.x + b.y + b.z;
        return stackless_coroutine::operation::_next;

      },
      stackless_coroutine::make_while_true(
          []() {
            struct dummy {};
            return dummy{};
          },
          [](auto &a, auto &b) {
            std::cout << "Inside breaker " << b.outer_value().x << std::endl;
            if (b.outer_value().x > 5) {
              return stackless_coroutine::operation::_break;
            } else {
              return stackless_coroutine::operation::_next;
            }

          },
          [](auto &a, auto &b) { ++b.outer_value().x; }

          )
		  
		  );

  ci.run([](auto &a, std::exception_ptr, bool as, auto op) {
    std::cout << stackless_coroutine::operations[static_cast<int>(op)]
              << " Finished\n";
  });

  auto &v = ci.value();

  std::cout << v.x << std::endl;
};