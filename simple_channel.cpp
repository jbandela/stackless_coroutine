#define NDEBUG
#include "stackless_coroutine.hpp"
struct context_function {
	using function_t = void(*)(void *, void *);
	function_t func_;
	void *v_;
	void *data_ = nullptr;

	void operator()() const { func_(v_, data_); }

	template <class Context>
	context_function(Context context, void *data = nullptr) : data_{ data } {
		func_ = [](void *v, void *) {
			auto context = Context::get_context(v);
			context();
		};
		v_ = &context.f().value();
	}

	context_function() : func_{ nullptr }, v_{ nullptr } {};
	context_function(std::nullptr_t) : func_{ nullptr }, v_{ nullptr } {};

	context_function(function_t func, void *v, void *data)
		: func_{ func }, v_{ v }, data_{ data } {}

	explicit operator bool() const { return v_ != nullptr; }
};

bool cancel = false;


struct goroutine
{
	static int const N = 10;
	static int count;
	static context_function stack[N];

	static void schedule(context_function& rh)
	{
		assert(count < N);
		stack[count++] = rh;
		rh = nullptr;
	}

	~goroutine() {}

	static void go(goroutine) {}

	static void run_one()
	{
		assert(count > 0);
		stack[--count]();
	}


};
int goroutine::count;
context_function goroutine::stack[N];

template <typename T>
class channel
{
	T const* pvalue = nullptr;
	context_function reader = nullptr;
	context_function writer = nullptr;
public:
	template<class Context>
	auto push(T const& value, Context context)
	{
		assert(pvalue == nullptr);
		assert(!writer);
		pvalue = &value;
		writer = context;
		if (reader) goroutine::schedule(reader);
	
	
	}

	template<class Context>
	auto pull(Context context)
	{
		assert(!reader);

		if (writer) {
			auto result = *pvalue;
			pvalue = nullptr;
			auto wr = writer;
			writer = {};
			goroutine::schedule(wr);
			//wr();
			context(result);
		}
		else {
			reader = context_function{ [](void* v, void* d) {
				auto pthis = static_cast<channel<T>*>(d);
				auto context = Context::get_context(v);
				auto result = *pthis->pvalue;
				pthis->pvalue = nullptr;
				if (pthis->writer) {
					auto wr = pthis->writer;
					pthis->writer = nullptr;
					wr();
				}
				context(result);
			},&context.f().value(),this };

		}
	
	}

	void sync_push(T const& value)
	{
		assert(!pvalue);
		pvalue = &value;
		assert(reader);
		reader();
		assert(!pvalue);
		reader = nullptr;
	}

	auto sync_pull()
	{
		while (!pvalue) goroutine::run_one();
		auto result = *pvalue;
		pvalue = nullptr;
		if (writer)
		{
			auto wr = writer;
			writer = nullptr;
			wr();
		}
		return result;
	}
};
  struct variables {
    channel<int> *left;
    channel<int> *right;
    int value = 0;

    variables(channel<int> &left, channel<int> &right)
        : left{&left}, right{&right} {}
  };

template<class V>
auto get_pusher(V& v) {
  auto block =
      stackless_coroutine::make_block(stackless_coroutine::make_while_true(
          [](auto context, auto &variables) {
            variables.left->pull(context);
            return context.do_async();

          },
          [](auto context, auto &variables, int &value) {
            variables.value = value;
            ++variables.value;
            variables.right->push(variables.value, context);
            return context.do_async();

          },
          [](auto context, auto &variables) {

          }

          ));

  return stackless_coroutine::make_coroutine<V>(
      v,block, [](auto &&...) {});
}

//goroutine pusher(channel<int>& left, channel<int>& right)
//{
//	for (;;) {
//		auto val = co_await left.pull();
//		await right.push(val + 1);
//	}
//}

const int N = 100'000'000;
const int repeat = 10;

channel<int>* c = new channel<int>[N + 1];

//created in 0.064742 secs. pass thru in 0.032365 secs
// 1000 * 1000 in 32ms
// 1000 in 32 us
// 1 in 32 ns
#include <chrono>
#include <atomic>
#include <vector>
int main()
{
	printf("%zd\n", sizeof(channel<int>));

	//SetThreadAffinityMask(GetCurrentThread(), 1);
	using clock = std::chrono::high_resolution_clock;
	using dur = std::chrono::duration<float, std::ratio<1, 1>>;

	auto start = clock::now();
	using vars_t = stackless_coroutine::variables_t<variables>;
	std::vector<vars_t> vec;
	vec.reserve(N);
	for (int i = 0; i < N; ++i) {
		vec.emplace_back(c[i], c[i + 1]);

		get_pusher(vec.back())();
	}
	auto stop = clock::now();
	auto secs1 = std::chrono::duration_cast<dur>(stop - start).count();

	start = clock::now();
	int result = 0;
	for (int j = 0; j < repeat; ++j)
	{
		c[0].sync_push(0);
		result = c[N].sync_pull();
	}
	stop = clock::now();
	auto secs2 = std::chrono::duration_cast<dur>(stop - start).count();
	cancel = true;
	printf("created in %f secs. pass thru in %f secs, result %d\n", secs1, secs2  , result);
	return N - result;
}
