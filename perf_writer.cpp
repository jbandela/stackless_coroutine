//
// transmit_file.cpp
// ~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2015 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <ctime>
#include <iostream>
#include <string>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/asio.hpp>

std::vector<char> big_buffer;

using namespace boost::asio::ip;

class connection
  : public boost::enable_shared_from_this<connection>
{
public:
  typedef boost::shared_ptr<connection> pointer;

  static pointer create(boost::asio::io_service& io_service)
  {
    return pointer(new connection(io_service));
  }

  tcp::socket& socket()
  {
    return socket_;
  }

  void start()
  {
    boost::system::error_code ec;
	boost::asio::write(socket_, boost::asio::buffer(big_buffer.data(), big_buffer.size()));
    boost::system::error_code ignored_ec;
    socket_.shutdown(tcp::socket::shutdown_both, ignored_ec);
  }

private:
  connection(boost::asio::io_service& io_service)
    : socket_(io_service)
  {
  }

  tcp::socket socket_;
};

class server
{
public:
  server(boost::asio::io_service& io_service,
      unsigned short port)
    : acceptor_(io_service, tcp::endpoint(tcp::v4(), port))
  {
    start_accept();
  }

private:
  void start_accept()
  {
    connection::pointer new_connection =
      connection::create(acceptor_.get_io_service());

    acceptor_.async_accept(new_connection->socket(),
        boost::bind(&server::handle_accept, this, new_connection,
          boost::asio::placeholders::error));
  }

  void handle_accept(connection::pointer new_connection,
      const boost::system::error_code& error)
  {
    if (!error)
    {
      new_connection->start();
    }

    start_accept();
  }

  tcp::acceptor acceptor_;
  std::string filename_;
};

#include <algorithm>
int main(int argc, char* argv[])
{
	if (argc != 3)
    {
      std::cerr << "Usage: transmit_file <port> <size>\n";
      return 1;
    }


	std::cout << "Initializing buffer\n";
	std::uint64_t size = std::stoll(argv[2]);
	big_buffer.resize(size);
	for (std::uint64_t i = 0; i < big_buffer.size(); ++i) {
		big_buffer[i] = i & 0xFF;
	}

	std::cout << "Done Initializing buffer\n";
  try
  {
    boost::asio::io_service io_service;

    using namespace std; // For atoi.
    server s(io_service, atoi(argv[1]));

    io_service.run();
  }
  catch (std::exception& e)
  {
    std::cerr << e.what() << std::endl;
  }

  return 0;
}

