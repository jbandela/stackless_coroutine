// Copyright 2016 John R. Bandela
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "stackless_coroutine.hpp"
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <iostream>

template <class Finished>
auto do_coroutine(boost::asio::io_service &io, std::string host,
                  std::string service, std::string url, Finished f) {
  struct val {
    std::string host;
    std::string service;
    std::string url;
    boost::asio::io_service &io;

    boost::asio::ip::tcp::resolver resolver;
    boost::asio::ip::tcp::socket socket_;
    std::string request_string;
    std::string current;

    val(std::string host, std::string service, std::string url,
        boost::asio::io_service &io)
        : host{std::move(host)}, service{std::move(service)},
          url{std::move(url)}, io{io}, resolver{io}, socket_{io} {}
  };

  static auto tuple = stackless_coroutine::make_block(

      [](auto &context, auto &value) {
        boost::asio::ip::tcp::resolver::query query{value.host, value.service};
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
        }

      },
      // Send the GET message
      [](auto &context, auto &value) {

        std::stringstream request_stream;
        request_stream << "GET " << value.url << " HTTP/1.0\r\n";
        request_stream << "Host: " << value.host << "\r\n";
        request_stream << "Accept: */*\r\n";
        request_stream << "Connection: close\r\n\r\n";

        // Send the request.
        value.request_string = request_stream.str();
        auto &str = value.request_string;
        boost::asio::async_write(value.socket_,
                                 boost::asio::buffer(str.data(), str.size()),
                                 context);
        return context.do_async();
      },
      //  Check for errors
      [](auto &context, auto &value, auto &ec, auto n) {
        if (ec) {
          throw boost::system::system_error{ec};
        }

      },
      // Read in a loop
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) { value.current.resize(20); },

          [](auto &context, auto &value) {
            value.socket_.async_read_some(
                boost::asio::buffer(&value.current[0], value.current.size()),
                context);
            return context.do_async();

          },
          [](auto &context, val &value, auto &ec, auto n) {
            if (ec) {
              if (ec != boost::asio::error::eof)
                std::cerr << ec.message();
              return context.do_break();
            } else {

              value.current.resize(n);
              std::cout << value.current;
              return context.do_next();
            }

          }

          ));

  auto co = stackless_coroutine::make_coroutine<val>(
      tuple, std::move(f), std::move(host), std::move(service), std::move(url),
      io);
  return co();
}

#include <future>
#include <memory>

int main() {

  boost::asio::io_service io;
  auto work = std::make_unique<boost::asio::io_service::work>(io);

  std::promise<void> done_promise;

  auto done_future = done_promise.get_future();

  auto func = [&]() {
    do_coroutine(io, "www.httpbin.org", "http", "/",
                 [&](auto &a, std::exception_ptr e, auto op) {
                   if (e) {
                     done_promise.set_exception(e);
                   } else {
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
