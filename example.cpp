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
                  std::string url, Finished f) {

  // Holds values that must be across calls to the lambdas
  struct val {
    boost::asio::ip::tcp::resolver resolver;
    boost::asio::ip::tcp::socket socket;
    boost::asio::streambuf request;
    std::string current;
    explicit val(boost::asio::io_service &io) : resolver{io}, socket{io} {}
  };
  // Create the block for the lambda
  static auto block = stackless_coroutine::make_block(

      // For the first lamda of the block, in addition to context, and value,
      // can take other parameters
      [](auto &context, auto &value, const std::string &host,
         const std::string &path) {
        std::ostream request_stream(&value.request);

        request_stream << "GET " << path << " HTTP/1.0\r\n";
        request_stream << "Host: " << host << "\r\n";
        request_stream << "Accept: */*\r\n";
        request_stream << "Connection: close\r\n\r\n";

        boost::asio::ip::tcp::resolver::query query{host, "http"};

        // Pass context in to any function that requires a callback
        value.resolver.async_resolve(query, context);

        // Return do_async to signal that we have made an async call, and that
        // we should not go on the next
        // lamda upon return
        return context.do_async();
      },
      // In the next lambda after a do_async, in addition to context, and value,
      // also take the parameters
      // of the callback, in this case the error_code ec, and iterator
      [](auto &context, auto &value, auto ec, auto iterator) {

        // We can throw exceptions, and the exception will exit the entire block
        if (ec) {
          throw boost::system::system_error{ec};
        } else {

          // Pass context as the callback
          boost::asio::async_connect(value.socket, iterator, context);
          return context.do_async();
        }

      },
      // Take the parameters for the async_connect callback
      [](auto &context, auto &value, auto &ec, auto iterator) {

        if (ec) {
          throw boost::system::system_error{ec};
        }

        // Send the request.
        boost::asio::async_write(value.socket, value.request, context);
        return context.do_async();
      },
      [](auto &context, auto &value, auto &ec, auto n) {
        if (ec) {
          throw boost::system::system_error{ec};
        }

      },

      // make_while_true creates a loop, that always repeats, unless
      // context.do_break is called
      stackless_coroutine::make_while_true(
          [](auto &context, auto &value) {
            value.current.resize(20);
            value.socket.async_read_some(
                boost::asio::buffer(&value.current[0], value.current.size()),
                context);
            return context.do_async();

          },
          [](auto &context, val &value, auto &ec, auto n) {
            if (ec) {
              if (ec != boost::asio::error::eof)
                std::cerr << ec.message();
              // context.do_break breaks out of the loop
              return context.do_break();
            } else {

              value.current.resize(n);
              std::cout << value.current;

              // context.do_continue continues the loop out of the loop
              // We could also return do_next() which will continue to the next
              // lamda
              // which in this case would result in re-entering the loop at the
              // top
              return context.do_continue();
            }

          }

          ));

  // pass the block, and the callback, along with any arguments for our val
  // constructor,
  // which in this case is io to make_coroutine
  auto co = stackless_coroutine::make_coroutine<val>(block, std::move(f), io);

  // call co with arguments corresponding to the parameters of the first lambda
  // after context and value
  // which in this case is host, and url
  return co(host, url);
}

#include <future>
#include <memory>

int main() {

  boost::asio::io_service io;
  do_coroutine(io, "www.httpbin.org", "/",

               // The callback - which takes a reference to the val struct, an
               // exception pointer, and op which tells us how we exited the
               // coroutine
               [&](auto &a, std::exception_ptr e, auto op) {
                 if (e) {
                   std::cerr << "\nHad an exception\n";
                 } else {
                   std::cout << "\nFinished successfully\n";
                 }
               });

  io.run();
};
