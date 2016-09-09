
#define BOOST_THREAD_PROVIDES_FUTURE_CONTINUATION
#define BOOST_THREAD_PROVIDES_EXECUTORS
#define BOOST_THREAD_PROVIDES_FUTURE_WHEN_ALL_WHEN_ANY
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
goroutine writer_select(std::shared_ptr<channel<int>> chan1, std::shared_ptr<channel<int>> chan2, int count) {
	await_channel_writer<int> writer1{ chan1 };
	await_channel_writer<int> writer2{ chan2 };
	for (int i = 0; i < count; ++i) {
		if (i % 2) {
			co_await writer1.write(i);
		}
		else {

			co_await writer2.write(i);
		}
	}
	chan1->close();
	chan2->close();

}
goroutine reader_select(std::shared_ptr<channel<int>> chan1, std::shared_ptr<channel<int>> chan2, std::atomic<int>& f) {
	await_channel_reader<int> reader1{ chan1 };
	await_channel_reader<int> reader2{ chan2 };
	for (;;) {
		auto p = co_await read_select(reader1, [](auto) {}, reader2,[](auto) {});
		if (p.first == false) break;
	}
	f = 1;
}

auto test_channel_select(int count) {
	std::atomic<int> f;
	f = 0;

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	writer_select(chan1,chan2, count);
	reader_select(chan1,chan2, f);
	while (f == 0);

}
auto test_future_select(int count) {
	boost::executors::loop_executor loop;
	boost::promise<int> p1;
	boost::promise<int> p2;
	auto fut1 = p1.get_future();
	auto fut2 = p2.get_future();


	for (int i = 0; i < count; ++i) {
		auto selected = boost::when_any(std::move(fut1), std::move(fut2));
		bool done = false;
		selected.then(loop, [&](auto&& f) {
			auto p = f.get();
			if (std::get<0>(p).is_ready()) {
				p1 = boost::promise<int>{};
				fut1 = p1.get_future();
				fut2 = std::move(std::get<1>(p));
			}
			else {
				p2 = boost::promise<int>{};
				fut2 = p2.get_future();
				fut1 = std::move(std::get<0>(p));
			}
			done = true;
		});
		if (i % 2) {
			p1.set_value(i);
		}
		else {
			p2.set_value(i);
		}
		while (!done) { loop.run_queued_closures(); }

	}
}

auto make_reader_select(std::shared_ptr<channel<int>> chan1,
                        std::shared_ptr<channel<int>> chan2,
                        std::atomic<int> &f) {
  struct values {
    channel_reader<int> reader1;
    channel_reader<int> reader2;
    channel_selector selector;
    std::mutex mut;
    std::atomic<int> *pf;

    values(std::shared_ptr<channel<int>> c1, std::shared_ptr<channel<int>> c2,
           std::atomic<int> &f)
        : reader1{c1}, reader2{c2}, pf{&f} {}
  };
  auto co = stackless_coroutine::make_coroutine<values>(
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, values &variables) {
            std::unique_lock<std::mutex> lock{variables.mut};
            variables.reader1.read(variables.selector, context);
            variables.reader2.read(variables.selector, context);
            return context.do_async();

          },
          [](auto &context, values &variables, auto channel, auto value,
             bool closed) {

            std::unique_lock<std::mutex> lock{variables.mut};
            lock.unlock();
           auto sel = get_selector(channel, value);
            sel.select(variables.reader1, [](auto &&) {})
                .select(variables.reader2, [](auto &&) {});

            if (closed) {
              *variables.pf = 1;
              return context.do_break();
            }
 
            return context.do_continue();

          }

          )

                                          ),
      [](auto &&...) {}, chan1, chan2, f);

  return co;
}

auto make_writer_select(std::shared_ptr<channel<int>> chan1,
                        std::shared_ptr<channel<int>> chan2, int count) {
  struct values {
    channel_writer<int> writer1;
    channel_writer<int> writer2;
    int count;
    int i;
    values(std::shared_ptr<channel<int>> chan1,
           std::shared_ptr<channel<int>> chan2, int c)
        : writer1{chan1}, writer2{chan2}, count{c}, i{0} {}
  };
  auto co = stackless_coroutine::make_coroutine<values>(
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, values &variables) {
            if (variables.i >= variables.count) {
              variables.writer1.ptr->close();
              variables.writer2.ptr->close();
              return context.do_async_return();
            }
            if (variables.i % 2) {

              variables.writer1.write(variables.i, context);
            } else {

              variables.writer2.write(variables.i, context);
            }
            ++variables.i;
            return context.do_async();

          },
          [](auto &context, values &variables, auto channel, bool closed) {}

          )

                                          ),
      [](auto &&...) {}, chan1, chan2, count);
  return co;
}

auto test_channel_stackless_library_select(int count) {
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
	{

		auto start = std::chrono::steady_clock::now();
		test_channel_select(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " channel_select iterations in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}
	{

		auto start = std::chrono::steady_clock::now();
		test_future_select(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " future_select iterations in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}
	{

		auto start = std::chrono::steady_clock::now();
		test_channel_stackless_library_select(count);
		auto end = std::chrono::steady_clock::now();
		std::cout << "Did " << count << " channel stackless library iterations in " << std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count() << "\n";
	}

}

