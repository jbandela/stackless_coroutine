// Copyright 2016 John R. Bandela
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "stackless_coroutine.hpp"
#include <iostream>
#include <thread>
#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include <functional>
#include <future>
#include <memory>
#include <utility>

#include "channel.hpp"

template <class F> void do_thread(F f) {
  std::thread t{[f]() mutable { f(); }};
  t.detach();
}

template <class R> struct value_t {
  R return_value;
  std::promise<R> p;
  int finished_count = 0;
};

template <class V, class T> auto get_future(T t) {
  auto co = stackless_coroutine::make_coroutine<V>(
      t,
      [](V &value, std::exception_ptr ep, stackless_coroutine::operation op) {
        ++value.finished_count;
        REQUIRE(value.finished_count == 1);
        if (ep) {
          value.p.set_exception(ep);
        } else {
          value.p.set_value(value.return_value);
        }
      });
  auto fut = co.ptr->p.get_future();
  co();
  return fut;
}


TEST_CASE("while if async", "[stackless]") {
	auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
		[](auto &context, auto &value) { value.return_value = 1; },
		stackless_coroutine::make_while_true(
			stackless_coroutine::make_if(
				[](auto &value) { return value.return_value < 5; },
				stackless_coroutine::make_block(
					[](auto &context, auto &value) { return context.do_next(); }),
				stackless_coroutine::make_block([](auto &context, auto &value) {
		return context.do_break();
	})),
			[](auto &context, auto &value) {
		do_thread([context]() mutable { context(1); });
		return context.do_async();
	},
		[](auto &context, auto &value, int aval) {
		value.return_value += aval;
	})));
	REQUIRE(f.get() == 5);
}

TEST_CASE("Simple test while", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },
      stackless_coroutine::make_while_true([](auto &context, auto &value) {
        ++value.return_value;
        return context.do_return();
      })

          ));
  REQUIRE(f.get() == 2);
}

TEST_CASE("Simple test if while", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) {
            ++value.return_value;
            return context.do_return();
          },
          stackless_coroutine::make_if(
              [](auto &value) { return value.return_value == 1; },
              stackless_coroutine::make_block([](auto &context, auto &value) {
                value.return_value = 2;
                return context.do_return();
              }),
              stackless_coroutine::make_block([](auto &context, auto &value) {
                value.return_value = 3;
                return context.do_return();
              })))

          ));
  REQUIRE(f.get() == 2);
}

TEST_CASE("Simple test", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; }));
  REQUIRE(f.get() == 1);
}
TEST_CASE("Simple test if", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },

      stackless_coroutine::make_if(
          [](auto &value) { return value.return_value == 1; },
          stackless_coroutine::make_block([](auto &context, auto &value) {
            value.return_value = 2;
            return context.do_return();
          }),
          stackless_coroutine::make_block([](auto &context, auto &value) {
            value.return_value = 3;
            return context.do_return();
          }))

          ));
  REQUIRE(f.get() == 2);
}

TEST_CASE("Simple async", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 1;
        do_thread([context]() mutable { context(1); });
        return context.do_async();
      },
      [](auto &context, auto &value, int aval) {
        value.return_value += aval;
      }));
  REQUIRE(f.get() == 2);
}

TEST_CASE("while async", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) {
            if (value.return_value < 5)
              return context.do_next();
            else
              return context.do_break();

          },
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.return_value += aval;
          })));
  REQUIRE(f.get() == 5);
}

TEST_CASE("while async get_context", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) {
            if (value.return_value < 5)
              return context.do_next();
            else
              return context.do_break();

          },
          [](auto &context, auto &value) {
            using context_t = std::decay_t<decltype(context)>;
            void *v = &value;
            do_thread([v]() { context_t::get_context(v)(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.return_value += aval;
          })));
  REQUIRE(f.get() == 5);
}



template <class R> struct value_temp_t {
  R return_value;
  R inner;
  R outer;
  std::promise<R> p;
  int finished_count = 0;
};
TEST_CASE("inner and outer while if async", "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while_true(
          stackless_coroutine::make_if(
              [](auto &value) { return value.outer < 5; },
              stackless_coroutine::make_block(
                  [](auto &context, auto &value) { return context.do_next(); }),
              stackless_coroutine::make_block([](auto &context, auto &value) {
                return context.do_break();
              })),
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while_true(
              stackless_coroutine::make_if(
                  [](auto &value) { return value.inner < 7; },
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_next();
                      }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_break();
                      })),
              [](auto &context, auto &value) {
                do_thread([context]() mutable { context(1); });
                return context.do_async();
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              }))));
  REQUIRE(f.get() == 35);
}

TEST_CASE("inner and outer while async", "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) {
            if (value.outer >= 5) {
              return context.do_break();
            } else {
              return context.do_next();
            }

          },
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while_true(

              [](auto &context, auto &value) {

                if (value.inner >= 7) {
                  return context.do_break();
                } else {
                  return context.do_next();
                }

              },
              [](auto &context, auto &value) {
                do_thread([context]() mutable { context(1); });
                return context.do_async();
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              }))));
  REQUIRE(f.get() == 35);
}

TEST_CASE("inner and outer while if async with early return", "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while_true(
          stackless_coroutine::make_if(
              [](auto &value) { return value.outer < 5; },
              stackless_coroutine::make_block(
                  [](auto &context, auto &value) { return context.do_next(); }),
              stackless_coroutine::make_block([](auto &context, auto &value) {
                return context.do_break();
              })),
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while_true(
              stackless_coroutine::make_if(
                  [](auto &value) { return value.inner < 7; },
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_next();
                      }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_break();
                      })),
              [](auto &context, auto &value) {
                do_thread([context]() mutable { context(1); });
                return context.do_async();
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              },
              stackless_coroutine::make_if(
                  [](auto &value) { return value.return_value == 29; },
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_return();
                      }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {})

                      )))));
  REQUIRE(f.get() == 29);
}
TEST_CASE("inner and outer while if async with early return using make_while "
          "instead of make_while_true",
          "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while(
          [](auto &value) { return value.outer < 5; },
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while(
              [](auto &value) { return value.inner < 7; },
              [](auto &context, auto &value) {
                do_thread([context]() mutable { context(1); });
                return context.do_async();
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              },
              stackless_coroutine::make_if(
                  [](auto &value) { return value.return_value == 29; },
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_return();
                      }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {})

                      )))));
  REQUIRE(f.get() == 29);
}

TEST_CASE("inner and outer while if async with early return using make_while "
          "instead of make_while_true, but use do_async_break",
          "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while(
          [](auto &value) { return value.outer < 5; },
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while_true(
              [](auto &context, auto &value) {
                if (value.inner < 7) {
                  do_thread([context]() mutable { context(1); });
                  return context.do_async();
                } else {
                  return context.do_async_break();
                }
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              },
              stackless_coroutine::make_if(
                  [](auto &value) { return value.return_value == 29; },
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {
                        return context.do_return();
                      }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {})

                      )))));
  REQUIRE(f.get() == 29);
}

TEST_CASE("inner and outer while if async with early return using exception "
          "using make_while "
          "instead of make_while_true, but use do_async_break",
          "[stackless]") {

  int val = -1;
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::make_while(
          [](auto &value) { return value.outer < 5; },
          [](auto &context, auto &value) {
            do_thread([context]() mutable { context(1); });
            return context.do_async();
          },
          [](auto &context, auto &value, int aval) {
            value.outer += aval;
            value.inner = 0;
          },
          stackless_coroutine::make_while_true(
              [](auto &context, auto &value) {
                if (value.inner < 7) {
                  do_thread([context]() mutable { context(1); });
                  return context.do_async();
                } else {
                  return context.do_async_break();
                }
              },
              [](auto &context, auto &value, int aval) {
                value.return_value += aval;
                value.inner += aval;
              },
              stackless_coroutine::make_if(
                  [](auto &value) { return value.return_value == 29; },
                  stackless_coroutine::make_block([](auto &context,
                                                     auto &value) {

                    std::string message = "Exception thrown. value: " +
                                          std::to_string(value.return_value);
                    throw std::runtime_error(message);
                  }),
                  stackless_coroutine::make_block(
                      [](auto &context, auto &value) {})

                      )))));
  try {
    f.get();
  } catch (std::exception &e) {

    REQUIRE(e.what() == std::string("Exception thrown. value: 29"));
    return;
  }
  REQUIRE(1 == 2);
}

TEST_CASE("simple generator [stackless]") {
  bool destructed = false;
  struct variables {
    int i = 0;
    bool *b = nullptr;
    ~variables() { *b = true; }
    variables(int it, bool *bt) : i{it}, b{bt} {}
  };

  auto block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, variables &v) {
            return context.do_async_yield(v.i);
          },
          [](auto &context, variables &v) { ++v.i; }));

  std::vector<int> v;
  {
    auto gen = stackless_coroutine::make_generator<int, variables>(block, 0,
                                                                   &destructed);
    for (auto i : gen) {
      v.push_back(i);
      if (i == 10)
        break;
    }
  }
  std::vector<int> answer = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  REQUIRE(v == answer);
  REQUIRE(destructed == true);
};

#include <experimental/filesystem>
namespace directory_generator {
struct variables_t {
  std::string value;
  std::experimental::filesystem::directory_iterator begin;
  std::experimental::filesystem::directory_iterator end;
  stackless_coroutine::generator<std::string> gen;

  variables_t(const std::experimental::filesystem::path &path) : begin{path} {}
};

stackless_coroutine::generator<std::string>
get_directory_generator(const std::experimental::filesystem::path &p);

inline auto get_block() {

  return stackless_coroutine::make_block(stackless_coroutine::make_while_true(
      [](auto &context, auto &variables) {
        if (variables.begin != variables.end)
          return context.do_async_yield(variables.begin->path().string());
        else
          return context.do_async_break();
      },
      [](auto &context, auto &variables) {
        if (std::experimental::filesystem::is_directory(
                variables.begin->path())) {
          variables.gen = get_directory_generator(variables.begin->path());
          return context.do_next();
        } else {
          ++variables.begin;
          return context.do_continue();
        }
      },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &variables) {
            auto iter = variables.gen.begin();
            if (iter != variables.gen.end())
              return context.do_async_yield(std::move(*iter));
            else
              return context.do_async_break();
          },
          [](auto &context, auto &variables) { ++variables.gen.begin(); }),

      [](auto &context, auto &variables) { ++variables.begin; }

      ));
}

inline stackless_coroutine::generator<std::string>
get_directory_generator(const std::experimental::filesystem::path &p) {
  return stackless_coroutine::make_generator<std::string, variables_t>(
      get_block(), p);
}
};

TEST_CASE("directory generator [stackless]") {

  auto g = directory_generator::get_directory_generator(".");

  std::vector<std::string> vgen(g.begin(), g.end());

  // Verify that we did the right thing by also checking using Filesystem TS
  // recursive_directory_iterator

  std::experimental::filesystem::recursive_directory_iterator begin{"."};
  std::vector<std::string> vrdi;

  for (auto &e : begin) {
    vrdi.push_back(e.path().string());
  }

  std::sort(vgen.begin(), vgen.end());
  std::sort(vrdi.begin(), vrdi.end());

  REQUIRE(vgen == vrdi);
}

TEST_CASE("many loop while [stackless]") {
	auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
		[](auto &context, auto &value) { value.return_value = 1; },
		stackless_coroutine::make_while_true([](auto &context, auto &value) {
		++value.return_value;
		if (value.return_value > 9999)
			return context.do_return();
		else
			return context.do_continue();
	})

	));
	REQUIRE(f.get() == 10000);
}

TEST_CASE("simple channel [stackless]") {

	auto chan = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	struct reader_variables {
		channel_reader<int> rchan;
		int* result = nullptr;

		reader_variables(std::shared_ptr<channel<int>> pchan, int* r) :rchan{ pchan }, result{ r } {}
	};

	auto reader_block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
			[](auto& context, reader_variables& variables) {
		variables.rchan.read(context);
		return context.do_async();

			},
			[](auto& context, reader_variables& variables,auto channel, int& value, bool closed) {
				if (closed) {
					return context.do_break();
				}
				(*variables.result) += value;
				return context.do_continue();
			}

		)

	);

	struct writer_variables {
		channel_writer<int> wchan;
		int counter = 0;

		writer_variables(std::shared_ptr<channel<int>> pchan, int c) :wchan{ pchan }, counter{ c } {}
	};


	auto writer_block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
			[](auto& context, writer_variables& variables) {
		if (variables.counter <= max) {
			variables.wchan.write(variables.counter, context);
			return context.do_async();
				}
		else {
			variables.wchan.ptr->close();
			return context.do_async_break();
		}

			},
			[](auto& context, writer_variables& variables,auto channel, bool closed) {
				++variables.counter;
			}

		)

	);
	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	auto rco = stackless_coroutine::make_coroutine<reader_variables>(reader_block, [&](auto&&...) {++finished;cvar.notify_one();}, chan, &total);
	auto wco = stackless_coroutine::make_coroutine<writer_variables>(writer_block, [&](auto&&...) {++finished;cvar.notify_one();}, chan, 0);

	rco();
	wco();

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 2) {
		cvar.wait(lock);
	}











 
  REQUIRE(total == (max*max + max)/2);
}

TEST_CASE("simple channel select [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	struct reader_variables {
		channel_reader<int> rchan1;
		channel_reader<int> rchan2;
		int* result = nullptr;
		channel_selector s;

		reader_variables(std::shared_ptr<channel<int>> pchan1,std::shared_ptr<channel<int>> pchan2, int* r) :rchan1{ pchan1 },rchan2{ pchan2 }, result{ r } {}
	};

	auto reader_block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
			[](auto& context, reader_variables& variables) {
		variables.rchan1.read(variables.s, context);
		variables.rchan2.read(variables.s, context);
		return context.do_async();

	},
			[](auto& context, reader_variables& variables, auto channel, auto value, bool closed) {
		if (closed) {
			return context.do_break();
		}
		auto sel = variables.s.get_selector(channel, value);
		sel.select(variables.rchan1, [&](auto& v) {

		(*variables.result) += v;
		})
		.select(variables.rchan2, [&](auto& v) {

		(*variables.result) += v;
		});
	
		return context.do_continue();
	}

		)

	);

	struct writer_variables {
		channel_writer<int> wchan1;
		channel_writer<int> wchan2;
		int counter = 0;

		writer_variables(std::shared_ptr<channel<int>> pchan1, std::shared_ptr<channel<int>> pchan2,int c) :wchan1{ pchan1 }, wchan2{ pchan2 },counter{ c } {}
	};


	auto writer_block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
			[](auto& context, writer_variables& variables) {
		if (variables.counter <= max) {
			if (variables.counter % 2) {
				variables.wchan1.write(variables.counter, context);
			}
			else {

				variables.wchan2.write(variables.counter, context);
			}
			return context.do_async();
		}
		else {
			variables.wchan1.ptr->close();
			variables.wchan2.ptr->close();
			return context.do_async_break();
		}

	},
			[](auto& context, writer_variables& variables, auto channel, bool closed) {
		++variables.counter;
	}

		)

	);
	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	auto rco = stackless_coroutine::make_coroutine<reader_variables>(reader_block, [&](auto&&...) {
		++finished;cvar.notify_one();
	}, chan1,chan2, &total);
	auto wco = stackless_coroutine::make_coroutine<writer_variables>(writer_block, [&](auto&&...) {
		++finished;cvar.notify_one();
	}, chan1,chan2, 0);

	rco();
	wco();

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 2) {
		cvar.wait(lock);
	}












	REQUIRE(total == (max*max + max) / 2);
}
#ifdef _MSC_VER
template<class Func>
goroutine await_reader(std::shared_ptr<channel<int>> reader_chan, int& ptotal,Func f) {
	await_channel_reader<int> reader{ reader_chan };
	while (true) {
		auto p = co_await reader.read();
		if (p.first == false) {
			f();
			return;
		}
		ptotal += p.second;
	}

}
template<class Func>
goroutine await_writer(std::shared_ptr<channel<int>> writer_chan, int max,Func f) {
	await_channel_writer<int> writer{ writer_chan };
	for(int i = 0; i <= max; ++i){
		co_await writer.write(i);
	}
	writer_chan->close();
	f();

}



TEST_CASE("simple await channel [stackless]") {

	auto chan = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	await_reader(chan,total, [&](auto&&...) {++finished;cvar.notify_one();});
	await_writer(chan,max, [&](auto&&...) {++finished;cvar.notify_one();});

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 2) {
		cvar.wait(lock);
	}












	REQUIRE(total == (max*max + max) / 2);
}



template<class Func>
goroutine await_select_reader(std::shared_ptr<channel<int>> reader_chan1, std::shared_ptr<channel<int>> reader_chan2,int& ptotal, Func f) {
	await_channel_reader<int> reader1{ reader_chan1 };
	await_channel_reader<int> reader2{ reader_chan2 };
	while (true) {
		auto lambda = [&](auto& v) {
			ptotal += v;
		};


		auto res = co_await read_select(reader1, lambda,
			reader2, lambda);
			
		if (res.first == false) {
			f();
			return;
		}
	}

}
template<class Func>
goroutine await_select_writer(std::shared_ptr<channel<int>> writer_chan1, std::shared_ptr<channel<int>> writer_chan2, int max, Func f) {
	await_channel_writer<int> writer1{ writer_chan1 };
	await_channel_writer<int> writer2{ writer_chan2 };
	for (int i = 0; i <= max; ++i) {
		if (i % 2) {
			co_await writer1.write(i);
		}
		else {

			co_await writer2.write(i);
		}
	}
	writer_chan1->close();
	writer_chan2->close();
	f();

}

TEST_CASE("simple await select channel [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	await_select_reader(chan1,chan2, total, [&](auto&&...) {++finished;cvar.notify_one();});
	await_select_writer(chan1,chan2, max, [&](auto&&...) {++finished;cvar.notify_one();});

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 2) {
		cvar.wait(lock);
	}












	REQUIRE(total == (max*max + max) / 2);
}
template<class Func>
goroutine await_select_range_reader(std::shared_ptr<channel<int>> reader_chan1, std::shared_ptr<channel<int>> reader_chan2,int& ptotal, Func f) {
	std::vector<await_channel_reader<int>> vec;
	vec.emplace_back(reader_chan1);
	vec.emplace_back(reader_chan2);
	while (true) {
		auto lambda = [&](auto& v) {
			ptotal += v;
		};


		auto res = co_await read_select_range(vec, lambda);
			
		if (res.first == false) {
			f();
			return;
		}
	}

}
TEST_CASE("simple await select range channel [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	await_select_range_reader(chan1,chan2, total, [&](auto&&...) {++finished;cvar.notify_one();});
	await_select_writer(chan1,chan2, max, [&](auto&&...) {++finished;cvar.notify_one();});

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 2) {
		cvar.wait(lock);
	}












	REQUIRE(total == (max*max + max) / 2);
}


#endif 
