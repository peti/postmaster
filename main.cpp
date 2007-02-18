/*
 * Copyright (c) 2001-2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include <csignal>
#include <boost/test/prg_exec_monitor.hpp>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/thread.hpp>

using boost::asio::ip::tcp;

class session
{
public:
  session(boost::asio::io_service& io_service)
    : socket_(io_service)
  {
    std::cout << __PRETTY_FUNCTION__ << std::endl;
  }

  ~session()
  {
    std::cout << __PRETTY_FUNCTION__ << std::endl;
  }

  tcp::socket& socket()
  {
    return socket_;
  }

  void start()
  {
    socket_.async_read_some(boost::asio::buffer(data_, max_length),
        boost::bind(&session::handle_read, this,
          boost::asio::placeholders::error,
          boost::asio::placeholders::bytes_transferred));
  }

  void handle_read(const boost::system::error_code& error,
      size_t bytes_transferred)
  {
    if (!error)
    {
      boost::asio::async_write(socket_,
          boost::asio::buffer(data_, bytes_transferred),
          boost::bind(&session::handle_write, this,
            boost::asio::placeholders::error));
    }
    else
    {
      delete this;
    }
  }

  void handle_write(const boost::system::error_code& error)
  {
    if (!error)
    {
      socket_.async_read_some(boost::asio::buffer(data_, max_length),
          boost::bind(&session::handle_read, this,
            boost::asio::placeholders::error,
            boost::asio::placeholders::bytes_transferred));
    }
    else
    {
      delete this;
    }
  }

private:
  tcp::socket socket_;
  enum { max_length = 1024 };
  char data_[max_length];
};

class server
{
public:
  server(boost::asio::io_service& io_service, short port)
    : io_service_(io_service),
      acceptor_(io_service, tcp::endpoint(tcp::v4(), port))
  {
    std::cout << __PRETTY_FUNCTION__ << std::endl;
    session* new_session = new session(io_service_);
    acceptor_.async_accept(new_session->socket(),
        boost::bind(&server::handle_accept, this, new_session,
          boost::asio::placeholders::error));
  }

  ~server()
  {
    std::cout << __PRETTY_FUNCTION__ << std::endl;
  }

  void handle_accept(session* new_session,
      const boost::system::error_code& error)
  {
    if (!error)
    {
      new_session->start();
      new_session = new session(io_service_);
      acceptor_.async_accept(new_session->socket(),
          boost::bind(&server::handle_accept, this, new_session,
            boost::asio::placeholders::error));
    }
    else
    {
      delete new_session;
    }
  }

private:
  boost::asio::io_service& io_service_;
  tcp::acceptor acceptor_;
};

static boost::asio::io_service & get_io_service()
{
  static boost::asio::io_service ios;
  return ios;
}

static void stop_io_service()
{
  get_io_service().stop();
}

static void run_io_service()
{
  get_io_service().run();
}

int cpp_main(int argc, char * argv[])
{
  using namespace std;

  // Make sure usage is correct.
  if (argc != 2)
  {
    std::cerr << "Usage: async_tcp_echo_server <port>\n";
    return 1;
  }

  // Setup the system.

  ios::sync_with_stdio(false);
  signal(SIGTERM, reinterpret_cast<sighandler_t>(&stop_io_service));
  signal(SIGINT,  reinterpret_cast<sighandler_t>(&stop_io_service));
  signal(SIGHUP,  reinterpret_cast<sighandler_t>(&stop_io_service));
  signal(SIGQUIT, reinterpret_cast<sighandler_t>(&stop_io_service));
  signal(SIGPIPE, SIG_IGN);

  // Start the server.

  server s(get_io_service(), atoi(argv[1]));
  boost::thread t1(&run_io_service);
  boost::thread t2(&run_io_service);
  boost::thread t3(&run_io_service);
  get_io_service().run();
  t1.join(); t2.join(); t3.join();

  // Shut down gracefully.

  return 0;
}
