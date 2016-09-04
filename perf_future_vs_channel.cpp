
#define BOOST_THREAD_PROVIDES_FUTURE_CONTINUATION
#define BOOST_THREAD_PROVIDES_EXECUTORS
#include <boost/thread/executor.hpp>
#include <boost/thread/executors/loop_executor.hpp>
#include <boost/thread/future.hpp>
#include <experimental/coroutine>
#include <iostream>
#include <chrono>
#include <atomic>
#include <thread>


auto test_future(int count) {
	boost::executors::loop_executor loop;

	for (int i = 0; i < count; ++i) {
		boost::promise<int> p;
		auto fut = p.get_future();
		fut.then(loop, [&](auto&&) {});
		p.set_value(i);
		loop.run_queued_closures();
	}
}
auto test_callback(int count) {
	boost::executors::loop_executor loop;

	for (int i = 0; i < count; ++i) {
		loop.submit([]() {});
		loop.run_queued_closures();
	}
}


#include "channel.hpp"

goroutine writer(std::shared_ptr<channel<int>> chan, int count) {
	await_channel_writer<int> writer{ chan };
	for (int i = 0; i < count; ++i) {
		co_await writer.write(i);
	}
	chan->close();

}
goroutine reader(std::shared_ptr<channel<int>> chan,std::atomic<int>& f) {
	await_channel_reader<int> reader{ chan };
	for (;;) {
		auto p = co_await reader.read();
		if (p.first == false) break;
	}
	f = 1;
}

auto test_channel(int count) {
	std::atomic<int> f;
	f = 0;

	auto chan = std::make_shared<channel<int>>();

	writer(chan, count);
	reader(chan, f);
	while (f == 0);

}


int main() {

	auto count = 1'000'000;
	{
		auto start = std::chrono::steady_clock::now();
		test_future(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " iterations for future in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}
	{
		auto start = std::chrono::steady_clock::now();
		test_callback(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " iterations for callback in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}


	{

		auto start = std::chrono::steady_clock::now();
		test_channel(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " channel iterations in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}

}