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
    int *result = nullptr;

    reader_variables(std::shared_ptr<channel<int>> pchan, int *r)
        : rchan{pchan}, result{r} {}
  };

  auto reader_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, reader_variables &variables) {
            variables.rchan.read(context);
            return context.do_async();

          },
          [](auto &context, reader_variables &variables, auto channel,
             int &value, bool closed) {
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

    writer_variables(std::shared_ptr<channel<int>> pchan, int c)
        : wchan{pchan}, counter{c} {}
  };

  auto writer_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, writer_variables &variables) {
            if (variables.counter <= max) {
              variables.wchan.write(variables.counter, context);
              return context.do_async();
            } else {
              variables.wchan.close();
              return context.do_async_break();
            }

          },
          [](auto &context, writer_variables &variables, auto channel,
             bool closed) { ++variables.counter; }

          )

                                          );
  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  auto rco =
      stackless_coroutine::make_coroutine<reader_variables>(reader_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan, &total);
  auto wco =
      stackless_coroutine::make_coroutine<writer_variables>(writer_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan, 0);

  rco();
  wco();

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait(lock);
  }

  REQUIRE(total == (max * max + max) / 2);
}

TEST_CASE("simple channel select [stackless]") {

  auto chan1 = std::make_shared<channel<int>>();
  auto chan2 = std::make_shared<channel<int>>();

  static constexpr int max = 10000;
  int total = 0;

  struct reader_variables {
    channel_reader<int> rchan1;
    channel_reader<int> rchan2;
    int *result = nullptr;
    channel_selector s;
    std::mutex mut;

    reader_variables(std::shared_ptr<channel<int>> pchan1,
                     std::shared_ptr<channel<int>> pchan2, int *r)
        : rchan1{pchan1}, rchan2{pchan2}, result{r} {}
  };

  auto reader_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, reader_variables &variables) {
            std::unique_lock<std::mutex> lock{variables.mut};
            variables.rchan1.read(variables.s, context);
            variables.rchan2.read(variables.s, context);
            lock.unlock();
            return context.do_async();

          },
          [](auto &context, reader_variables &variables, auto... v) {

            std::unique_lock<std::mutex> lock{variables.mut};
            lock.unlock();

            auto sel = variables.s.get_selector(v...);
            sel.select(variables.rchan1,
                       [&](auto &v) { (*variables.result) += v; })
                .select(variables.rchan2,
                        [&](auto &v) { (*variables.result) += v; });
            if (!sel) {
              return context.do_break();
            }

            return context.do_continue();
          }

          )

                                          );

  struct writer_variables {
    channel_writer<int> wchan1;
    channel_writer<int> wchan2;
    int counter = 0;

    writer_variables(std::shared_ptr<channel<int>> pchan1,
                     std::shared_ptr<channel<int>> pchan2, int c)
        : wchan1{pchan1}, wchan2{pchan2}, counter{c} {}
  };

  auto writer_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, writer_variables &variables) {
            if (variables.counter <= max) {
              if (variables.counter % 2) {
                variables.wchan1.write(variables.counter, context);
              } else {

                variables.wchan2.write(variables.counter, context);
              }
              return context.do_async();
            } else {
              variables.wchan1.close();
              variables.wchan2.close();
              return context.do_async_break();
            }

          },
          [](auto &context, writer_variables &variables, auto channel,
             bool closed) { ++variables.counter; }

          )

                                          );
  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  auto rco = stackless_coroutine::make_coroutine<reader_variables>(
      reader_block,
      [&](auto &&...) {
        ++finished;
        cvar.notify_all();
      },
      chan1, chan2, &total);
  auto wco =
      stackless_coroutine::make_coroutine<writer_variables>(writer_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan1, chan2, 0);

  rco();
  wco();

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait_for(lock, std::chrono::seconds{1});
  }

  REQUIRE(total == (max * max + max) / 2);
}

TEST_CASE("simple channel buffered [stackless]") {

  auto chan = std::make_shared<channel<int>>(100);

  for (int i = 1; i <= 6; ++i) {
    chan->push(i);
  }

  static constexpr int max = 10000;
  int total = 0;

  struct reader_variables {
    channel_reader<int> rchan;
    int *result = nullptr;

    reader_variables(std::shared_ptr<channel<int>> pchan, int *r)
        : rchan{pchan}, result{r} {}
  };

  auto reader_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, reader_variables &variables) {
            variables.rchan.read(context);
            return context.do_async();

          },
          [](auto &context, reader_variables &variables, auto channel,
             int &value, bool closed) {
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

    writer_variables(std::shared_ptr<channel<int>> pchan, int c)
        : wchan{pchan}, counter{c} {}
  };

  auto writer_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, writer_variables &variables) {
            if (variables.counter <= max) {
              variables.wchan.write(variables.counter, context);
              return context.do_async();
            } else {
              variables.wchan.close();
              return context.do_async_break();
            }

          },
          [](auto &context, writer_variables &variables, auto channel,
             bool closed) { ++variables.counter; }

          )

                                          );
  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  auto rco =
      stackless_coroutine::make_coroutine<reader_variables>(reader_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan, &total);
  auto wco =
      stackless_coroutine::make_coroutine<writer_variables>(writer_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan, 0);

  wco();
  rco();

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait(lock);
  }

  REQUIRE(total == 21 + (max * max + max) / 2);
}

TEST_CASE("simple channel select buffered [stackless]") {

  auto chan1 = std::make_shared<channel<int>>(10);
  auto chan2 = std::make_shared<channel<int>>(10);

  for (int i = 1; i <= 10; ++i) {
    REQUIRE(chan1->push(i) == true);
    REQUIRE(chan2->push(i) == true);
  }
  REQUIRE(chan1->push(11) == false);
  REQUIRE(chan2->push(11) == false);

  static constexpr int max = 10000;
  int total = 0;

  struct reader_variables {
    channel_reader<int> rchan1;
    channel_reader<int> rchan2;
    int *result = nullptr;
    channel_selector s;
    std::mutex mut;

    reader_variables(std::shared_ptr<channel<int>> pchan1,
                     std::shared_ptr<channel<int>> pchan2, int *r)
        : rchan1{pchan1}, rchan2{pchan2}, result{r} {}
  };

  auto reader_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, reader_variables &variables) {
            std::unique_lock<std::mutex> lock{variables.mut};
            variables.rchan1.read(variables.s, context);
            variables.rchan2.read(variables.s, context);
            lock.unlock();
            return context.do_async();

          },
          [](auto &context, reader_variables &variables, auto channel,
             auto value, bool closed) {

            std::unique_lock<std::mutex> lock{variables.mut};
            lock.unlock();
            auto sel = variables.s.get_selector(channel, value, closed);
            sel.select(variables.rchan1,
                       [&](auto &v) { (*variables.result) += v; })
                .select(variables.rchan2,
                        [&](auto &v) { (*variables.result) += v; });
            if (closed) {
              return context.do_break();
            }

            return context.do_continue();
          }

          )

                                          );

  struct writer_variables {
    channel_writer<int> wchan1;
    channel_writer<int> wchan2;
    int counter = 0;

    writer_variables(std::shared_ptr<channel<int>> pchan1,
                     std::shared_ptr<channel<int>> pchan2, int c)
        : wchan1{pchan1}, wchan2{pchan2}, counter{c} {}
  };

  auto writer_block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, writer_variables &variables) {
            if (variables.counter <= max) {
              if (variables.counter % 2) {
                variables.wchan1.write(variables.counter, context);
              } else {

                variables.wchan2.write(variables.counter, context);
              }
              return context.do_async();
            } else {
              variables.wchan1.close();
              variables.wchan2.close();
              return context.do_async_break();
            }

          },
          [](auto &context, writer_variables &variables, auto channel,
             bool closed) { ++variables.counter; }

          )

                                          );
  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  auto rco = stackless_coroutine::make_coroutine<reader_variables>(
      reader_block,
      [&](auto &&...) {
        ++finished;
        cvar.notify_all();
      },
      chan1, chan2, &total);
  auto wco =
      stackless_coroutine::make_coroutine<writer_variables>(writer_block,
                                                            [&](auto &&...) {
                                                              ++finished;
                                                              cvar.notify_all();
                                                            },
                                                            chan1, chan2, 0);

  wco();
  rco();

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait_for(lock, std::chrono::seconds{1});
  }

  REQUIRE(total == 110 + (max * max + max) / 2);
}

TEST_CASE("read write mixed channel select [stackless]") {

  auto chan1 = std::make_shared<channel<int>>(1);
  auto chan2 = std::make_shared<channel<int>>(1);

  static constexpr int max = 10000;
  int total = 0;

  struct variables {
    channel_reader<int> rchan1;
    channel_reader<int> rchan2;
    channel_writer<int> wchan1;
    channel_writer<int> wchan2;

    int *result = nullptr;
    channel_selector s;
    std::mutex mut;

    int write_count = 0;
    int read_count = 0;

    variables(std::shared_ptr<channel<int>> pchan1,
              std::shared_ptr<channel<int>> pchan2, int *r)
        : rchan1{pchan1}, rchan2{pchan2}, wchan1{pchan1}, wchan2{pchan2},
          result{r} {}
  };

  auto block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, variables &variables) {
            std::unique_lock<std::mutex> lock{variables.mut};

            if (variables.write_count <= max) {
              if (variables.write_count % 2) {
                variables.wchan1.write(variables.s, variables.write_count,
                                       context);
              } else {
                variables.wchan2.write(variables.s, variables.write_count,
                                       context);
              }
            }
            if (variables.read_count <= max) {
              variables.rchan1.read(variables.s, context);
              variables.rchan2.read(variables.s, context);
            } else {
              return context.do_async_return();
            }

            lock.unlock();
            return context.do_async();

          },
          [](auto &context, variables &variables, auto... v) {

            std::unique_lock<std::mutex> lock{variables.mut};
            lock.unlock();

            auto sel = variables.s.get_selector(v...);
            sel.select(variables.rchan1,
                       [&](auto &v) {

                         (*variables.result) += v;
                         ++variables.read_count;
                       })
                .select(variables.rchan2,
                        [&](auto &v) {

                          (*variables.result) += v;
                          ++variables.read_count;
                        })
                .select(variables.wchan1, [&]() { ++variables.write_count; })
                .select(variables.wchan2, [&]() { ++variables.write_count; });

            if (!sel)
              return context.do_return();
            return context.do_continue();
          }

          )

                                          );

  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  auto co =
      stackless_coroutine::make_coroutine<variables>(block,
                                                     [&](auto &&...) {
                                                       ++finished;
                                                       cvar.notify_all();
                                                     },
                                                     chan1, chan2, &total);
  co();
  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 1) {
    cvar.wait_for(lock, std::chrono::seconds{1});
  }

  REQUIRE(total == (max * max + max) / 2);
}

#ifdef _MSC_VER
template <class Func>
goroutine await_reader(std::shared_ptr<channel<int>> reader_chan, int &ptotal,
                       Func f) {
  await_channel_reader<int> reader{reader_chan};
  while (true) {
    auto p = co_await reader.read();
    if (p.first == false) {
      f();
      return;
    }
    ptotal += p.second;
  }
}
template <class Func>
goroutine await_writer(std::shared_ptr<channel<int>> writer_chan, int max,
                       Func f) {
  await_channel_writer<int> writer{writer_chan};
  for (int i = 0; i <= max; ++i) {
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
  std::atomic<int> finished{0};
  await_reader(chan, total, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });
  await_writer(chan, max, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait_for(lock, std::chrono::seconds{1});
  }

  REQUIRE(total == (max * max + max) / 2);
}

template <class Func>
goroutine await_select_reader(std::shared_ptr<channel<int>> reader_chan1,
                              std::shared_ptr<channel<int>> reader_chan2,
                              int &ptotal, Func f) {
  await_channel_reader<int> reader1{reader_chan1};
  await_channel_reader<int> reader2{reader_chan2};
  while (true) {
    auto lambda = [&](auto &v) { ptotal += v; };

    auto res = co_await select(reader1, lambda, reader2, lambda);

    if (res.first == false) {
      f();
      return;
    }
  }
}
template <class Func>
goroutine await_select_writer(std::shared_ptr<channel<int>> writer_chan1,
                              std::shared_ptr<channel<int>> writer_chan2,
                              int max, Func f) {
  await_channel_writer<int> writer1{writer_chan1};
  await_channel_writer<int> writer2{writer_chan2};
  for (int i = 0; i <= max; ++i) {
    if (i % 2) {
      co_await writer1.write(i);
    } else {

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
  std::atomic<int> finished{0};
  await_select_reader(chan1, chan2, total, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });
  await_select_writer(chan1, chan2, max, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait(lock);
  }

  REQUIRE(total == (max * max + max) / 2);
}
template <class Func>
goroutine await_select_range_reader(std::shared_ptr<channel<int>> reader_chan1,
                                    std::shared_ptr<channel<int>> reader_chan2,
                                    int &ptotal, Func f) {
  std::vector<await_channel_reader<int>> vec;
  vec.emplace_back(reader_chan1);
  vec.emplace_back(reader_chan2);
  while (true) {
    auto lambda = [&](auto &v) { ptotal += v; };

    auto res = co_await select_range(vec, lambda);

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
  std::atomic<int> finished{0};
  await_select_range_reader(chan1, chan2, total, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });
  await_select_writer(chan1, chan2, max, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });

  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 2) {
    cvar.wait(lock);
  }

  REQUIRE(total == (max * max + max) / 2);
}

template <class Func>
goroutine await_select_mixed(std::shared_ptr<channel<int>> chan1,
                             std::shared_ptr<channel<int>> chan2, int &ptotal,
                             int max, Func f) {

  await_channel_reader<int> reader1{chan1};
  await_channel_reader<int> reader2{chan2};
  await_channel_writer<int> writer1{chan1};
  await_channel_writer<int> writer2{chan2};

  int count = 0;

  auto lambda = [&](auto &v) { ptotal += v; };
  while (true) {

    if (count <= max) {
      auto &writer = *[&]() {
        if (count % 2) {
          return &writer1;
        } else {
          return &writer2;
        }
      }();
      co_await select(writer, count, [&]() { ++count; }, reader1, lambda,
                      reader2, lambda);

    } else {
      // if (!chan1->closed)
      chan1->close();
      // if (!chan2->closed)
      chan2->close();
      auto res = co_await select(reader1, lambda, reader2, lambda);

      if (res.first == false) {
        f();
        return;
      }
    }
  }
}
TEST_CASE("mixed await select channel [stackless]") {
  return;

  auto chan1 = std::make_shared<channel<int>>();
  auto chan2 = std::make_shared<channel<int>>();

  static constexpr int max = 10000;
  int total = 0;

  std::mutex m;
  std::condition_variable cvar;
  std::atomic<int> finished{0};
  await_select_mixed(chan1, chan2, total, max, [&](auto &&...) {
    ++finished;
    cvar.notify_all();
  });
  std::unique_lock<std::mutex> lock{m};
  while (finished.load() < 1) {
    cvar.wait(lock);
  }

  REQUIRE(total == (max * max + max) / 2);
}



TEST_CASE("simple await select sync_channel reader [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
;
	await_select_writer(chan1, chan2, max, [&](auto &&...) {
		++finished;
		cvar.notify_all();
	});

	thread_suspender susp;
	sync_channel_reader<int,thread_suspender> reader1{ chan1,susp };
	sync_channel_reader<int,thread_suspender> reader2{ chan2,susp };
	while (true) {
		auto lambda = [&](auto &v) { total += v; };

		auto res = sync_select(susp,reader1, lambda, reader2, lambda);

		if (res.first == false) {
			break;;
		}
	}


	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 1) {
		cvar.wait(lock);
	}

	REQUIRE(total == (max * max + max) / 2);
}

TEST_CASE("simple await select sync_channel writer [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
	;
	await_select_reader(chan1, chan2, total, [&](auto &&...) {
		++finished;
		cvar.notify_all();
	});

	thread_suspender susp;
	sync_channel_writer<int,thread_suspender> writer1{ chan1,susp };
	sync_channel_writer<int,thread_suspender> writer2{ chan2,susp };
	for (int i = 0; i <= max; ++i) {
		if (i % 2) {
			writer1.write(i);
		}
		else {

			writer2.write(i);
		}
	}
	chan1->close();
	chan2->close();

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 1) {
		cvar.wait(lock);
	}

	REQUIRE(total == (max * max + max) / 2);
}

void sync_select_range_reader(std::shared_ptr<channel<int>> reader_chan1,
	std::shared_ptr<channel<int>> reader_chan2,
	int &ptotal) {

	thread_suspender s;
	std::vector<sync_channel_reader<int,thread_suspender>> vec;
	vec.emplace_back(reader_chan1,s);
	vec.emplace_back(reader_chan2,s);
	while (true) {
		auto lambda = [&](auto &v) { ptotal += v; };

		auto res = sync_select_range(s,vec, lambda);

		if (res.first == false) {
			return;
		}
	}
}


TEST_CASE("simple await sync_select range channel [stackless]") {

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;

	std::mutex m;
	std::condition_variable cvar;
	std::atomic<int> finished{ 0 };
await_select_writer(chan1, chan2, max, [&](auto &&...) {
		++finished;
		cvar.notify_all();
	});
sync_select_range_reader(chan1, chan2, total);

	std::unique_lock<std::mutex> lock{ m };
	while (finished.load() < 1) {
		cvar.wait(lock);
	}

	REQUIRE(total == (max * max + max) / 2);
}


#endif


template<class SyncSuspender>
void sync_select_mixed(SyncSuspender& s, std::shared_ptr<channel<int>> chan1,
	std::shared_ptr<channel<int>> chan2, int &ptotal,
	int max) {

	sync_channel_reader<int,thread_suspender> reader1{ chan1,s };
	sync_channel_reader<int,thread_suspender> reader2{ chan2,s };
	sync_channel_writer<int,thread_suspender> writer1{ chan1,s };
	sync_channel_writer<int,thread_suspender> writer2{ chan2,s };

	int count = 0;

	auto lambda = [&](auto &v) { ptotal += v; };
	while (true) {

		if (count <= max) {
			auto &writer = *[&]() {
				if (count % 2) {
					return &writer1;
				}
				else {
					return &writer2;
				}
			}();
			 sync_select(s,writer, count, [&]() { ++count; }, reader1, lambda,
				reader2, lambda);

		}
		else {
			chan1->close();
			chan2->close();
			auto res = sync_select(s,reader1, lambda, reader2, lambda);

			if (res.first == false) {
				return;
			}
		}
	}
}

TEST_CASE("mixed await sync_select channel") {
	return;

	auto chan1 = std::make_shared<channel<int>>();
	auto chan2 = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;
	thread_suspender s;
	sync_select_mixed(s,chan1, chan2, total, max);

	REQUIRE(total == (max * max + max) / 2);
}

void sync_reader(std::shared_ptr<channel<int>> reader_chan, int &ptotal) {
	thread_suspender s;
	sync_channel_reader<int,thread_suspender> reader{ reader_chan,s };
	while (true) {
		auto p = reader.read();
		if (p.first == false) {
			return;
		}
		ptotal += p.second;
	}
}
void sync_writer(std::shared_ptr<channel<int>> writer_chan, int max) {
	thread_suspender s;
	sync_channel_writer<int,thread_suspender> writer{ writer_chan, s };
	for (int i = 0; i <= max; ++i) {
		writer.write(i);
	}
	writer_chan->close();
}

TEST_CASE("simple sync channel [stackless]") {

	auto chan = std::make_shared<channel<int>>();

	static constexpr int max = 10000;
	int total = 0;


	std::thread rt{ [&]() {
		sync_reader(chan,total);
	
	} };
	std::thread wt{ [&]() {
		sync_writer(chan,max);
	
	} };

	rt.join();
	wt.join();


	REQUIRE(total == (max * max + max) / 2);
}
