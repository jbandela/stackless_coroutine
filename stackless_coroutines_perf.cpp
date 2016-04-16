#include "stackless_coroutine.hpp"
#include <array>
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <chrono>
#include <iostream>

template <class Finished>
auto do_coroutine(boost::asio::io_service &io, std::string host,
                  std::string port, int size, Finished f) {
  struct val {
    boost::asio::io_service &io;

    boost::asio::ip::tcp::resolver resolver;
    boost::asio::ip::tcp::socket socket_;
    std::vector<char> current;
    std::chrono::steady_clock::time_point start_time;
    std::chrono::steady_clock::time_point end_time;
    std::uint64_t bytes = 0;

    val(boost::asio::io_service &io) : io{io}, resolver{io}, socket_{io} {}
  };

  auto tuple = stackless_coroutine::make_block(

      [](auto &context, auto &value, std::string host, std::string port,
         int size) {
        value.current.resize(size);
        boost::asio::ip::tcp::resolver::query query{host, port};
        value.resolver.async_resolve(query, context);
        return context.do_async();
      },
      // Connect to the host
      [](auto &context, auto &value, auto ec, auto iterator) {

        if (ec) {
          throw boost::system::system_error{ec};
        } else {
          boost::asio::async_connect(value.socket_, iterator, context);
          return context.do_async();
        }

      },
      // Check if connection successful
      [](auto &context, auto &value, auto &ec, auto iterator) {

        if (ec) {
          throw boost::system::system_error{ec};
        } else {
          // Set the start time
          value.start_time = std::chrono::steady_clock::now();
        }

      },
      // Read in a loop
      stackless_coroutine::make_while_true(

          [](auto &context, auto &value) {
            value.socket_.async_read_some(
                boost::asio::buffer(value.current.data(), value.current.size()),
                context);
            return context.do_async();

          },
          [](auto &context, auto &value, auto &ec, auto n) {
            if (ec) {
              if (ec != boost::asio::error::eof)
                std::cerr << ec.message();
              return context.do_break();
            }
            value.bytes += n;
            return context.do_next();
          }));

  auto co = stackless_coroutine::make_coroutine<val>(tuple, std::move(f), io);
  return co(host, port, size);
}

#include <future>
#include <memory>

int main(int argc, char **argv) {

  if (argc != 3) {
    std::cerr << "usage stackless_coroutine_perf <port> <read buffer size>\n";
    return 1;
  }

  boost::asio::io_service io;
  auto work = std::make_unique<boost::asio::io_service::work>(io);

  std::promise<void> done_promise;

  auto done_future = done_promise.get_future();

  auto func = [&]() {
    do_coroutine(io, "127.0.0.1", argv[1], std::stoi(argv[2]),
                 [&](auto &value, std::exception_ptr e, auto op) {
                   if (e) {
                     done_promise.set_exception(e);
                   } else {
                     auto end_time = std::chrono::steady_clock::now();

                     auto duration =
                         std::chrono::duration_cast<std::chrono::milliseconds>(
                             end_time - value.start_time);
                     std::cout << "Received " << value.bytes << " in "
                               << duration.count() << " milliseconds\n";
                     std::cout << "Transmit speed "
                               << (value.bytes / duration.count()) * 1000
                               << " Bytes/Seconds \n";
                     done_promise.set_value();
                   }
                 });
  };

  io.post(func);

  auto frun = std::async(std::launch::async, [&]() { io.run(); });

  done_future.wait();

  work.reset();

  frun.get();

  try {

    done_future.get();
  } catch (std::exception &e) {
    std::cerr << e.what() << "\n";
  }
};
