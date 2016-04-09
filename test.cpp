#include "stackless_coroutines.hpp"
#include <thread>
#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include <functional>
#include <future>
#include <memory>
#include <utility>

template <class F> void do_thread(F f) {
  std::thread t{[f]() mutable { f(); }};
  t.detach();
}

template <class R> struct value_t {
  R return_value;
  std::promise<R> p;
};

template <class V, class T> auto get_future(T t) {
  auto p = std::make_unique<V>();
  auto fut = p->p.get_future();
  stackless_coroutine::run(std::move(p), t,
                           [](V &value, std::exception_ptr ep, bool async,
                              stackless_coroutine::operation op) {
                             if (ep) {
                               value.p.set_exception(ep);
                             } else {
                               value.p.set_value(value.return_value);
                             }
                           });
  return fut;
}

TEST_CASE("Simple test", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; }));
  REQUIRE(f.get() == 1);
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
      stackless_coroutine::while_true(
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

TEST_CASE("while if async", "[stackless]") {
  auto f = get_future<value_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) { value.return_value = 1; },
      stackless_coroutine::while_true(
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

template <class R> struct value_temp_t {
  R return_value;
  R inner;
  R outer;
  std::promise<R> p;
};
TEST_CASE("inner and outer while if async", "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::while_true(
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
          stackless_coroutine::while_true(
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
TEST_CASE("inner and outer while if async with early return", "[stackless]") {
  auto f = get_future<value_temp_t<int>>(stackless_coroutine::make_block(
      [](auto &context, auto &value) {
        value.return_value = 0;
        value.inner = 0;
        value.outer = 0;
      },
      stackless_coroutine::while_true(
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
          stackless_coroutine::while_true(
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
