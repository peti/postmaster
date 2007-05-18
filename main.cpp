/*
 * Copyright (c) 2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/compatibility/cpp_c_headers/csignal>
#include <boost/program_options.hpp>
#include <boost/test/included/prg_exec_monitor.hpp>
#include <sanity/system-error.hpp>
#include <iostream>

#define TRACE(msg) std::cerr << "trace: [" <<__func__ << "] " << msg << std::endl
#define INFO(msg)  std::cerr << "info: " << msg << std::endl

#ifndef PACKAGE_NAME
static char const PACKAGE_NAME[]    = "postmaster";
#endif
#ifndef PACKAGE_VERSION
static char const PACKAGE_VERSION[] = "2007-05-10";
#endif

typedef boost::asio::io_service io_service;
static boost::scoped_ptr<io_service> the_io_service;

static void start_service()
{
  the_io_service->run();
}

static void stop_service()
{
  the_io_service->stop();
}

int cpp_main(int argc, char ** argv)
{
  using namespace std;
  namespace po = boost::program_options;
  //
  // Ignore relevant signals to avoid race conditions.
  //
  signal(SIGTERM, SIG_IGN);
  signal(SIGINT,  SIG_IGN);
  signal(SIGHUP,  SIG_IGN);
  signal(SIGQUIT, SIG_IGN);
  signal(SIGPIPE, SIG_IGN);
  //
  // Seed the random number generator.
  //
  srand(time(0));
  //
  // stdout/stderr are accessed only through the C++ streams API.
  //
  ios::sync_with_stdio(false);
  //
  // Parse the command line: Generic Process and Daemon Options
  //
  vector<string>        listen_addrs;
  po::options_description meta_opts("Administrative Options");
  meta_opts.add_options()
    ( "help,h",                                                         "produce help message and exit" )
    ( "version,v",                                                      "show program version and exit" )
    ( "no-detach,D",                                                    "don't run in the background" )
    ( "listen",         po::value< vector<string> >(&listen_addrs),     "listen on address:port"  )
    ;
  po::positional_options_description pos_opts;
  pos_opts.add("listen", -1);
  //
  // Parse the command line: HTTP Page Delivery and Logging
  //
  string                hostname;
  po::options_description httpd_opts("SMTP Daemon Configuration");
  httpd_opts.add_options()
    ( "hostname",       po::value<string>(&hostname),                   "hostname to use in HELO response")
    ;
  //
  // Run command line parser. Obvious errors are thrown as exceptions.
  //
  po::options_description opts
    ( string("Commandline Interface ") + PACKAGE_NAME + " " + PACKAGE_VERSION)
    ;
  opts.add(meta_opts).add(httpd_opts);
  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv).options(opts).positional(pos_opts).run(), vm);
  po::notify(vm);
  //
  // Sanity checking ...
  //
  if (vm.count("help"))          { cout << opts << endl;                                     return 0; }
  if (vm.count("version"))       { cout << PACKAGE_NAME << " " << PACKAGE_VERSION << endl;   return 0; }
  if (listen_addrs.empty())      { cout << "no listen addresses configured" << endl;         return 1; }
  bool const detach( !vm.count("no-detach") );
  //
  // Setup the system and log configuration.
  //
  the_io_service.reset(new io_service);
  INFO(  PACKAGE_NAME << " version " << PACKAGE_VERSION
      << " running " << (detach ? "as daemon" : "on current tty")
      );
  //
  // Configure TCP listeners.
  //
  using namespace boost::asio::ip;
  typedef boost::shared_ptr<tcp::acceptor>      shared_acceptor;
  typedef vector<shared_acceptor>               acceptor_array;
  acceptor_array                                acceptors;
  for (vector<string>::iterator i( listen_addrs.begin() ); i != listen_addrs.end(); ++i)
  {
    tcp::endpoint addr;
    string::size_type k( i->rfind(':') );
    switch (k)
    {
      case string::npos:
        TRACE("parse TCP port '" << *i << "'");
        addr = tcp::endpoint(tcp::v6(), atoi(i->c_str()));
        break;

      case 0u:
        TRACE("parse any host, TCP port '" << *i << "'");
        addr = tcp::endpoint(tcp::v6(), atoi(i->c_str() + 1u));
        break;

      default:
        TRACE(  "parse host '" << i->substr(0, k)
             << "', port '"    << i->substr(k + 1u)
             << "'"
             );
        addr = tcp::endpoint( address::from_string(i->substr(0, k))
                            , atoi(i->substr(k + 1u).c_str())
                            );
    }
    INFO("listen on network address " << addr);
    shared_acceptor const acc( new tcp::acceptor(*the_io_service, addr) );
    acceptors.push_back(acc);
  }
  //
  // Detach from terminal.
  //
  if (detach)
  {
    switch (fork())
    {
      case -1:
        throw system_error("fork() failed");
      case 0:
        setsid();
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);
        break;
      default:
        return 0;
    }
  }
  //
  // Run the server.
  //
  signal(SIGTERM, reinterpret_cast<sighandler_t>(&stop_service));
  signal(SIGINT,  reinterpret_cast<sighandler_t>(&stop_service));
  signal(SIGHUP,  reinterpret_cast<sighandler_t>(&stop_service));
  signal(SIGQUIT, reinterpret_cast<sighandler_t>(&stop_service));
  start_service();
  //
  // Terminate gracefully.
  //
  INFO("shutting down");
  acceptors.clear();
  the_io_service.reset();
  return 0;
}
