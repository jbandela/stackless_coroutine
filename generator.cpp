#include "stackless_coroutine.hpp"
#include <assert.h>
#include <functional>
#include <iostream>

namespace stackless_coroutine {
}
#include <experimental/filesystem>
struct variables_t {
  std::string value;
  std::experimental::filesystem::directory_iterator begin;
  std::experimental::filesystem::directory_iterator end;
  stackless_coroutine::generator<std::string> gen;

  variables_t(const std::experimental::filesystem::path &path) : begin{path} {}
};

stackless_coroutine::generator<std::string>
get_directory_generator(const std::experimental::filesystem::path &p);

auto get_block() {

  return stackless_coroutine::make_block(stackless_coroutine::make_while_true(
      [](auto &context, auto &variables) {
        if (variables.begin != variables.end)
          return context.do_async_yield(variables.begin->path().string());
        else
          return context.do_async_break();
      },
      [](auto &context, auto &variables) {
        variables.gen = get_directory_generator(variables.begin->path());
      },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &variables) {
            if (variables.gen)
              return context.do_async_yield(variables.gen.value());
            else
              return context.do_async_break();
          },
          [](auto &context, auto &variables) { variables.gen(); }),

      [](auto &context, auto &variables) { ++variables.begin; }

      ));
}

inline stackless_coroutine::generator<std::string>
get_directory_generator(const std::experimental::filesystem::path &p) {
  return stackless_coroutine::make_generator<std::string, variables_t>(
      get_block(), p);
}

#include <algorithm>
#include <vector>
int main() {

  auto g = get_directory_generator(".");

  std::vector<std::string> vgen;
  while (g) {
	  std::cout << g.value() <<  "\n";
    vgen.push_back(g.value());
    g();
  }

  //Verify that we did the right thing by also checking using Filesystem TS recursive_directory_iterator

  std::experimental::filesystem::recursive_directory_iterator begin{"."};
  std::vector<std::string> vrdi;

  for (auto &e : begin) {
    vrdi.push_back(e.path().string());
  }

  std::sort(vgen.begin(), vgen.end());
  std::sort(vrdi.begin(), vrdi.end());

  std::cout << "Generator matches recursive_directory_iterator = " << std::boolalpha
            << std::equal(vgen.begin(), vgen.end(), vrdi.begin(), vrdi.end())
            << "\n";
}
