// Adapted by John R. Bandela from
// http://www.boost.org/doc/libs/1_60_0/doc/html/boost_asio/example/cpp03/http/client/async_client.cpp
// to use stackless_coroutine
//
// async_client.cpp
// ~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2015 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "stackless_coroutine.hpp"
#include <boost/asio.hpp>
#include <iostream>
#include <istream>
#include <memory>
#include <ostream>
#include <string>

using boost::asio::ip::tcp;

struct coroutine_variables_t {
  tcp::resolver resolver_;
  tcp::socket socket_;
  boost::asio::streambuf request_;
  boost::asio::streambuf response_;
  coroutine_variables_t(boost::asio::io_service &io_service)
      : resolver_(io_service), socket_(io_service) {}
};

void get(boost::asio::io_service &io_service, const std::string &server,
         const std::string &path) {

  auto block = stackless_coroutine::make_block(

      [](auto &context, auto &coroutine_variables, const std::string &server,
         const std::string &path) {
        std::ostream request_stream(&coroutine_variables.request_);
        request_stream << "GET " << path << " HTTP/1.0\r\n";
        request_stream << "Host: " << server << "\r\n";
        request_stream << "Accept: */*\r\n";
        request_stream << "Connection: close\r\n\r\n";

        // Start an asynchronous resolve to translate the server and service
        // names
        // into a list of endpoints.
        tcp::resolver::query query(server, "http");
        coroutine_variables.resolver_.async_resolve(query, context);
        return context.do_async();
      },
      [](auto &context, auto &coroutine_variables, auto err,
         auto endpoint_iterator) {
        if (err) {
          std::cout << "Error: " << err.message() << "\n";
          return context.do_async_return();
        } else {
          boost::asio::async_connect(coroutine_variables.socket_,
                                     endpoint_iterator, context);
          return context.do_async();
        }
      },
      [](auto &context, auto &coroutine_variables, auto err, auto) {
        if (err) {
          std::cout << "Error: " << err.message() << "\n";
          return context.do_async_return();
        } else {
          boost::asio::async_write(coroutine_variables.socket_,
                                   coroutine_variables.request_, context);
          return context.do_async();
        }
      },
      [](auto &context, auto &coroutine_variables, auto err, std::size_t) {
        if (err) {
          std::cout << "Error: " << err.message() << "\n";
          return context.do_async_return();
        } else {
          boost::asio::async_read_until(coroutine_variables.socket_,
                                        coroutine_variables.response_, "\r\n",
                                        context);
          return context.do_async();
        }

      },

      [](auto &context, auto &coroutine_variables, auto err, std::size_t) {
        if (err) {
          std::cout << "Error: " << err.message() << "\n";
          return context.do_async_return();
        } else {
          // Check that response is OK.
          std::istream response_stream(&coroutine_variables.response_);
          std::string http_version;
          response_stream >> http_version;
          unsigned int status_code;
          response_stream >> status_code;
          std::string status_message;
          std::getline(response_stream, status_message);
          if (!response_stream || http_version.substr(0, 5) != "HTTP/") {
            std::cout << "Invalid response\n";
            return context.do_async_return();
          }
          if (status_code != 200) {
            std::cout << "Response returned with status code ";
            std::cout << status_code << "\n";
            return context.do_async_return();
          }

          // Read the response headers, which are terminated by a blank line.
          boost::asio::async_read_until(coroutine_variables.socket_,
                                        coroutine_variables.response_,
                                        "\r\n\r\n", context);
          return context.do_async();
        }

      },
      [](auto &context, auto &coroutine_variables, auto err, std::size_t) {
        if (err) {
          std::cout << "Error: " << err.message() << "\n";
          return context.do_return();
        } else {
          // Process the response headers.
          std::istream response_stream(&coroutine_variables.response_);
          std::string header;
          while (std::getline(response_stream, header) && header != "\r")
            std::cout << header << "\n";
          std::cout << "\n";

          // Write whatever content we already have to output.
          if (coroutine_variables.response_.size() > 0)
            std::cout << &coroutine_variables.response_;

          // Start reading remaining data until EOF.
          return context.do_next();
        }

      },
      stackless_coroutine::make_while_true(
          [](auto &context, auto &coroutine_variables) {
            boost::asio::async_read(coroutine_variables.socket_,
                                    coroutine_variables.response_,
                                    boost::asio::transfer_at_least(1), context);
            return context.do_async();
          },
          [](auto &context, auto &coroutine_variables, auto err, std::size_t) {
            if (err) {
              if (err != boost::asio::error::eof)
                std::cout << "Error: " << err.message() << "\n";
              return context.do_return();
            } else {
              std::cout << &coroutine_variables.response_;
              return context.do_next();
            }
          }

          )

          );

  auto co = stackless_coroutine::make_coroutine<coroutine_variables_t>(
      block,
      [](auto &value, std::exception_ptr ep, stackless_coroutine::operation) {
        std::cout << "\n**Finished**\n";
        if (ep)
          std::rethrow_exception(ep);
      },
      io_service);

  co(server, path);
}
int main(int argc, char *argv[]) {
  try {
    if (argc != 3) {
      std::cout << "Usage: async_client <server> <path>\n";
      std::cout << "Example:\n";
      std::cout << "  async_client www.boost.org /LICENSE_1_0.txt\n";
      return 1;
    }

    boost::asio::io_service io_service;
    get(io_service, argv[1], argv[2]);
    io_service.run();
  } catch (std::exception &e) {
    std::cout << "Exception: " << e.what() << "\n";
  }

  return 0;
}