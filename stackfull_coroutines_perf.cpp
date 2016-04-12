#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/spawn.hpp>
#include <iostream>
#include <vector>
#include <chrono>
template<class Finished>
auto do_coroutine(boost::asio::io_service &io, std::string host,
	std::string port, int size, Finished f) {

	boost::asio::spawn(io, [&io, host, port,size, finished = f](boost::asio::yield_context yc)mutable {

		boost::asio::ip::tcp::resolver::query query{ host, port };
		boost::asio::ip::tcp::resolver resolver{ io };
		boost::asio::ip::tcp::socket socket_{ io };
		auto iterator = resolver.async_resolve(query, yc);

		boost::asio::async_connect(socket_, iterator, yc);

		auto start_time = std::chrono::steady_clock::now();
		std::uint64_t bytes = 0;

		std::vector<char> current;
		current.resize(size);

		while (true) {
			boost::system::error_code ec;
			auto n = socket_.async_read_some(
				boost::asio::buffer(current.data(),
					current.size()),
				yc[ec]);
			if (ec) {
				if (ec != boost::asio::error::eof)
					std::cerr << ec.message();
				break;
			};

			bytes += n;
		}
		finished(start_time, bytes);
	});


}

#include <future>
#include <memory>

int main(int argc, char** argv) {

	if (argc != 3) {
		std::cerr << "usage stackless_coroutine_perf <port> <read buffer size>\n";
		return 1;
	}
	boost::asio::io_service io;
	auto work = std::make_unique<boost::asio::io_service::work>(io);


	std::promise<void> done_promise;

	auto done_future = done_promise.get_future();

		do_coroutine(io, "127.0.0.1", argv[1],std::stoi(argv[2]), [&](auto start_time, auto bytes ) {
				auto end_time = std::chrono::steady_clock::now();

				auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
				std::cout << "Received " << bytes << " in " << duration.count() << " milliseconds\n";
				std::cout << "Transmit speed " << (bytes / duration.count()) * 1000 << " Bytes/Seconds \n";
				done_promise.set_value();
		});


	auto frun = std::async(std::launch::async, [&]() { io.run(); });

	done_future.wait();

	work.reset();

	frun.get();

	try {

		done_future.get();
	}
	catch (std::exception &e) {
		std::cerr << e.what() << "\n";
	}
};
