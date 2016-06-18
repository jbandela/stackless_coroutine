#include <boost/asio.hpp>
#include "stackless_coroutine.hpp"
#include <iostream>
#define NONIUS_RUNNER
#include "nonius/nonius.h++"

#include <boost/asio/yield.hpp>
struct session : boost::asio::coroutine
{
	int max = 0;
	int* counter = nullptr;
	boost::asio::io_service* pio;
	session(int m, boost::asio::io_service& io, int* c) :max{ m }, pio{ &io }, counter{ c } {}

	void operator()()
	{
		reenter(this)
		{
			for (;;)
			{
				if (*counter >= max) return;
				++*counter;
				yield pio->post(*this);
			}
		}
	}
};
#include <boost/asio/unyield.hpp>
	struct variables {
		int max = 0;
		int* counter = 0;
		boost::asio::io_service* pio;

		variables(int m, boost::asio::io_service& io, int* c) :max{ m }, pio{ &io }, counter{ c } {}
	};


auto get_coroutine(variables& v,int max, boost::asio::io_service& io, int* counter) {
	auto block = stackless_coroutine::make_block(
		stackless_coroutine::make_while_true(
			[](auto& context, variables& vars) {
		if (*vars.counter >= vars.max) {
			return context.do_async_return();
			}
		++*vars.counter;
		vars.pio->post(context);
		return context.do_async();


	}


	)
	);

	return stackless_coroutine::make_coroutine<variables>(v,block,[](auto&&...) {});


}


#include <chrono>
template<class F>
auto measure_time(F& f, boost::asio::io_service& io) {

	f();
	io.run();
} 



NONIUS_BENCHMARK("coroutine", [](nonius::chronometer meter) {

	boost::asio::io_service io;
	int max = 1000'000;

		int counter = 0;


		stackless_coroutine::variables_t<variables> v{max,io,&counter };
		auto co = get_coroutine(v,max, io,&counter);
	meter.measure([&] {

		measure_time(co, io);

	
		return 1; });
		if (counter != max) {
			std::cerr << "Error\n";
		}
	
//	std::cout << "Counter for coroutine = " << counter << "\n";
});

NONIUS_BENCHMARK("boost coroutine", [](nonius::chronometer meter) {

	boost::asio::io_service io;
	int max = 1000'000;

		int counter = 0;

		auto bco = session{ max,io,&counter };
	meter.measure([&] {

		measure_time(bco, io);
				return 1; });
if (counter != max) {
			std::cerr << "Error\n";
		}
	

//	std::cout << "Counter for boost coroutine = " << counter << "\n";
});


#if 0
int main() {


		std::cout << "Enter max\n";
		int max;
		std::cin >> max;


	boost::asio::io_service io;
	while (true) {


		std::cout << "c for co, b for bco\n";
		char c;
		std::cin >> c;


		if (c == 'c') {

			auto co = get_coroutine(max, io);
			std::cout << "co " << measure_time(co,io).count() << " microseconds\n";
		}
		else {

			auto bco = session{ max,io };

			std::cout << "bco " << measure_time(bco,io).count() << " microseconds\n";
		}







	}


}
#endif
