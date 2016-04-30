#include "stackless_coroutine.hpp"
#include <functional>
#include <iostream>

namespace stackless_coroutine {
namespace detail {
template <class Value> struct generator_base {
  virtual Value &value_imp() = 0;
  virtual bool valid() const = 0;
  virtual void next() = 0;
};

template <class Value, class Variables, class Block>
struct generator_imp : generator_base<Value> {
  struct generator_variables_t : Variables {
    Value generator_value_;
    using function_ptr_t = operation (*)(Variables *);

    function_ptr_t next_func_ = nullptr;

    void set_generator_value(Value v) { generator_value_ = std::move(v); }

    template <class Context> void set_generator_next(Context context) {
      next_func_ = [](Variables *v) {
        auto context = Context::get_context(v);
        return context();
      };
    }

    template <class... T>
    generator_variables_t(T &&... t) : Variables{std::forward<T>(t)...} {}
  };

  generator_variables_t *v_ = nullptr;

  template <class... T> generator_imp(Block b, T &&... t) {
    auto co = stackless_coroutine::make_coroutine<generator_variables_t>(
        b, [this](auto &variables, std::exception_ptr ep,
                  operation op) mutable { this->v_ = nullptr; },
        std::forward<T>(t)...);
    v_ = co.ptr.get();
    co();
  }
  virtual Value &value_imp() override { return v_->generator_value_; }

  virtual bool valid() const override {
    if (v_ && !v_->next_func_) {
      std::cout << "Error\n";
    }
    return v_;
  }

  virtual void next() override {
    auto vf = v_->next_func_;
    v_->next_func_ = nullptr;
    vf(v_);
  }
};
}

template <class Value> class generator {

  std::unique_ptr<detail::generator_base<Value>> base;

public:
  explicit operator bool() {
    if (!base)
      return false;
    return base->valid();
  }

  Value &value() { return base->value_imp(); }

  void operator()() { base->next(); }

  generator() = default;
  generator(generator &&) = default;
  generator &operator=(generator &&) = default;

  generator(std::unique_ptr<detail::generator_base<Value>> b)
      : base{std::move(b)} {}
};

template <class Value, class Variables, class Block, class... T>
generator<Value> make_generator(Block b, T &&... t) {
  std::unique_ptr<detail::generator_base<Value>> p =
      std::make_unique<detail::generator_imp<Value, Variables, Block>>(
          b, std::forward<T>(t)...);
  generator<Value> g{std::move(p)};
  return g;
}
}
#include <experimental/filesystem>
struct variables_t {
  std::string value;
  std::experimental::filesystem::directory_iterator begin;
  std::experimental::filesystem::directory_iterator end;
  stackless_coroutine::generator<std::string> gen;
  int level = 0;

  variables_t(const std::experimental::filesystem::path &path, int l)
      : begin{path}, level{l} {}
};

auto get_block() {

  auto block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto &context, auto &variables) {
            if (variables.begin != variables.end)
              return context.do_next();
            else
              return context.do_break();
          },
          [](auto &context, auto &variables) {
            auto str = variables.begin->path().string();
            variables.set_generator_value(str);
            variables.set_generator_next(context);
            return context.do_async();
          },
          [](auto &context, auto &variables) {
            variables.gen =
                stackless_coroutine::make_generator<std::string, variables_t>(
                    get_block(), variables.begin->path(), variables.level + 1);

          },

          stackless_coroutine::make_while_true(
              [](auto &context, auto &variables) {

                auto b = static_cast<bool>(variables.gen);
                if (b)
                  return context.do_next();
                else
                  return context.do_break();
              },
              [](auto &context, auto &variables) {
                auto str = variables.gen.value();
                variables.set_generator_value(str);

                variables.set_generator_next(context);
                return context.do_async();
              },
              [](auto &context, auto &variables) { variables.gen(); }),

          [](auto &context, auto &variables) { ++variables.begin; }

          ));
  return block;
}

int main() {

  auto g = stackless_coroutine::make_generator<std::string, variables_t>(
      get_block(), ".", 0);

  while (g) {
    std::cout << g.value() << "\n";
    g();
  }
}
