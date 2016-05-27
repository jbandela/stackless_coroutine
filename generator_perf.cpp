#include "stackless_coroutine.hpp"
#include <experimental/coroutine>
#include <experimental/generator>
#include "channel.hpp"


std::experimental::generator<std::uint32_t> test_co_yield(std::uint32_t count) {
	for (std::uint32_t i = 0; i < count; ++i) {
		co_yield i;
	}
}

stackless_coroutine::generator<std::uint32_t> test_stackless(std::uint32_t count) {
	struct variables {
		std::uint32_t count;
		std::uint32_t i;

	};

	auto block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
		[](auto context, auto& variables) {
		if (variables.i >= variables.count)return context.do_async_break();
		return context.do_async_yield(variables.i);
	},
		[](auto context, auto& variables) {
		++variables.i;


	}


		)


	);

	return stackless_coroutine::make_generator<std::uint32_t,variables>(block, count, 0u);

}

#include <future>
using channel_t = std::shared_ptr<channel<std::uint32_t>>;

	template<class PtrType = channel_t>
	struct variables {
		channel_reader<std::uint32_t,PtrType> reader;
		channel_writer<std::uint32_t,PtrType> writer;

		variables(PtrType ic, PtrType oc) :reader{ ic }, writer{ oc } {}
	};


auto make_coroutine(channel_t inchan, channel_t outchan) {
	auto block = stackless_coroutine::make_block(
		[](auto context, auto& variables) {
		variables.reader.read(context);
		return context.do_async();
	},
		[](auto context, auto& variables, void* channel, std::uint32_t& value, bool closed) {
		variables.writer.write(value + 1, context);
		return context.do_async();
	},
		[](auto context, auto& variables, void* channel, bool closed) {
	}



	);

	return stackless_coroutine::make_coroutine<variables<>>(block, [](auto&&...) {},inchan, outchan);

}

template<class V>
auto make_coroutine(V& variables) {
	auto block = stackless_coroutine::make_block(
		[](auto context, auto& variables) {
		variables.reader.read(context);
		return context.do_async();
	},
		[](auto context, auto& variables, void* channel, std::uint32_t& value, bool closed) {
		variables.writer.write(value + 1, context);
		return context.do_async();
	},
		[](auto context, auto& variables, void* channel, bool closed) {
	}



	);

	return stackless_coroutine::make_coroutine<V>(variables,block, [](auto&&...) {});

}


auto make_starter(channel_t inchan, channel_t outchan, std::promise<std::uint32_t> promise) {
	struct variables {
		channel_reader<std::uint32_t> reader;
		channel_writer<std::uint32_t> writer;
		std::uint32_t value = 0;
		std::promise<std::uint32_t> promise;

		variables(channel_t ic, channel_t oc, std::promise<std::uint32_t> p) :reader{ ic }, writer{ oc }, promise{ std::move(p) } {}
	};

	auto block = stackless_coroutine::make_block(
		[](auto context, auto& variables, std::uint32_t value) {
		variables.writer.write(value,context);
		return context.do_async();
	},
	[](auto context, auto& variables, void* channel, bool closed) {
		variables.reader.read(context);
		return context.do_async();
	
	},
		[](auto context, auto& variables, void* channel, std::uint32_t& value, bool closed) {
		variables.value = value;
	}
	



	);

	auto co = stackless_coroutine::make_coroutine<variables>(block, [](variables &value, std::exception_ptr ep, stackless_coroutine::operation op) {
		if (ep) {
			value.promise.set_exception(ep);
		}
		else {
			value.promise.set_value(value.value);
		}
	}, inchan, outchan, std::move(promise));
	return co;

	

}


auto make_starter(channel<uint32_t>* inchan, channel<uint32_t>* outchan, std::promise<std::uint32_t> promise) {
	struct variables {
		channel_reader<std::uint32_t,channel<uint32_t>*> reader;
		channel_writer<std::uint32_t,channel<uint32_t>*> writer;
		std::uint32_t value = 0;
		std::promise<std::uint32_t> promise;

		variables(channel<uint32_t>* ic, channel<uint32_t>* oc, std::promise<std::uint32_t> p) :reader{ ic }, writer{ oc }, promise{ std::move(p) } {}
	};

	auto block = stackless_coroutine::make_block(
		[](auto context, auto& variables, std::uint32_t value) {
		variables.writer.write(value,context);
		return context.do_async();
	},
	[](auto context, auto& variables, void* channel, bool closed) {
		variables.reader.read(context);
		return context.do_async();
	
	},
		[](auto context, auto& variables, void* channel, std::uint32_t& value, bool closed) {
		variables.value = value;
	}
	



	);

	auto co = stackless_coroutine::make_coroutine<variables>(block, [](variables &value, std::exception_ptr ep, stackless_coroutine::operation op) {
		if (ep) {
			value.promise.set_exception(ep);
		}
		else {
			value.promise.set_value(value.value);
		}
	}, inchan, outchan, std::move(promise));
	return co;

	

}




#include <iostream>
#include <chrono>
int main() {

	while(true){
		std::uint32_t count = 0;
		std::cin >> count;
		if (count == 0) break;
#if 0
		{
			channel_t first = std::make_shared < channel<std::uint32_t>>();
			channel_t ichan = first;
			auto start = std::chrono::steady_clock::now();
			for (unsigned int i = 0; i < count; ++i) {
				channel_t c = std::make_shared < channel<std::uint32_t>>();

				auto co = make_coroutine(ichan, c);
				co();
				ichan = c;

			}
			auto end = std::chrono::steady_clock::now();

			std::cout << "Took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " microseconds to init " << count << " coroutines\n Starting first \n";

			std::promise<std::uint32_t> promise;
			auto fut = promise.get_future();
			auto starter_co = make_starter(ichan, first, std::move(promise));

			start = std::chrono::steady_clock::now();
			starter_co(1);

			auto value = fut.get();
			end = std::chrono::steady_clock::now();


			std::cout << "Took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " to run " << count << " coroutines\n";
			std::cout << "Result = " << value << "\n";
		}
#endif

		{
		
			channel<uint32_t> first;
			auto start = std::chrono::steady_clock::now();
			using variables_t = stackless_coroutine::variables_t<variables<channel<uint32_t>*>, 1>;
			//auto channels = std::make_unique<channel<std::uint32_t>>[0] > (2*count);// channels;

			channel<std::uint32_t >* channels = new channel<std::uint32_t>[count+1];// > (2 * count);// channels;
			//channels.reserve(2*count);

			std::vector<variables_t> vec;
			vec.reserve(count);
			for (unsigned int i = 0; i < count; ++i) {
				//channels.emplace_back();
				//channels.emplace_back();
				vec.emplace_back(&channels[i], &channels[i+1]);

				auto co = make_coroutine(vec.back());
				co();

			}
			auto end = std::chrono::steady_clock::now();

			std::cout << "Took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " microseconds to init " << count << " coroutines\n Starting first \n";

			std::promise<std::uint32_t> promise;
			auto fut = promise.get_future();
			auto starter_co = make_starter(&channels[count], &channels[0], std::move(promise));

			start = std::chrono::steady_clock::now();
			starter_co(1);

			auto value = fut.get();
			end = std::chrono::steady_clock::now();


			std::cout << "Took " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << " to run " << count << " coroutines\n";
			std::cout << "Result = " << value << "\n";
			vec.clear();
			delete[] channels;
		}





	}

#ifdef _MSC_VER

	while (true) {
		std::uint32_t count = 0;
		std::cin >> count;
		if (count == 0) break;

		{
			auto start = std::chrono::steady_clock::now();
			std::uint64_t sum = 0;
			for (auto i : test_co_yield(count)) {
				sum += i;
			}

			auto end = std::chrono::steady_clock::now();

			std::cout << "calculated value of " << sum << " in " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "microseconds\n";

		}

		{
			auto start = std::chrono::steady_clock::now();
			std::uint64_t sum = 0;
			for (auto i : test_co_yield(count)) {
				sum += i;
			}

			auto end = std::chrono::steady_clock::now();

			std::cout << "calculated value of " << sum << " in " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count() << "microseconds\n";

		}







	}
#endif




}