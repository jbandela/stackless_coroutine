
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


#include "stackless_coroutine.hpp"
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

auto make_reader(std::shared_ptr<channel<int>> chan, std::atomic<int> &f) {
  struct values {
    channel_reader<int> reader;
    std::atomic<int> *pf;

    values(std::shared_ptr<channel<int>> c, std::atomic<int> &f)
        : reader{c}, pf{&f} {}
  };
  auto co = stackless_coroutine::make_coroutine<values>(
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, values &variables) {
            variables.reader.read(context);
            return context.do_async();

          },
          [](auto &context, values &variables, auto channel, int value,
             bool closed) {
            if (closed) {
              *variables.pf = 1;
              return context.do_break();
            }
            return context.do_continue();

          }

          )

                                          ),
      [](auto &&...) {}, chan, f);

  return co;
}

auto make_writer(std::shared_ptr<channel<int>> chan, int count) {
  struct values {
    channel_writer<int> writer;
    int count;
    int i;
    values(std::shared_ptr<channel<int>> chan, int c)
        : writer{chan}, count{c}, i{0} {}
  };
  auto co = stackless_coroutine::make_coroutine<values>(
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, values &variables) {
            if (variables.i >= variables.count) {
              variables.writer.ptr->close();
              return context.do_async_return();
            }
            variables.writer.write(variables.i, context);
            ++variables.i;
            return context.do_async();

          },
          [](auto &context, values &variables, auto channel, bool closed) {}

          )

                                          ),
      [](auto &&...) {}, chan, count);
  return co;
}

auto test_channel_stackless_library(int count) {
	std::atomic<int> f;
	f = 0;

	auto chan = std::make_shared<channel<int>>();

	auto w = make_writer(chan, count);
	auto r = make_reader(chan, f);
	w();
	r();
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
	{

		auto start = std::chrono::steady_clock::now();
		test_channel_stackless_library(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " channel stackless library iterations in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}


}