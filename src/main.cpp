#include "io.hpp"
#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/tuple/tuple_io.hpp>

struct print
{
  void operator() (std::string const * str) const
  {
    std::cout << (str ? str->c_str() : "error") << std::endl;
  }

  void operator() (std::vector<std::string> const * vec) const
  {
    if (vec)
    {
      std::cout << "[ ";
      std::copy(vec->begin(), vec->end(), std::ostream_iterator<std::string>(std::cout, " "));
      std::cout << "]" << std::endl;
    }
    else
      std::cout << "error" << std::endl;
  }

  void operator() (std::vector< std::pair< std::string,std::vector<std::string> > > const * vec) const
  {
    using namespace postmaster::io;
    if (vec)
    {
      for (resolver::mxname_list::const_iterator i( vec->begin() ); i != vec->end(); ++i)
      {
        std::cout << "MX " << i->first << " [ ";
        std::copy(i->second.begin(), i->second.end(), std::ostream_iterator<std::string>(std::cout, " "));
        std::cout << "]" << std::endl;
      }
    }
    else
      std::cout << "error" << std::endl;
  }
};

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

void socket_timeout(postmaster::io::socket s1, postmaster::io::socket s2, postmaster::io::socket s)
{
  std::cout << "timeout on socket " << s << std::endl;
  BOOST_ASSERT(s == s1 || s == s2);
  postmaster::io::on_input(s1,  postmaster::io::system_socket::handler());
  postmaster::io::on_output(s1, postmaster::io::system_socket::handler());
  postmaster::io::on_input(s2,  postmaster::io::system_socket::handler());
  postmaster::io::on_output(s2, postmaster::io::system_socket::handler());
}

int main(int, char**)
{
  using namespace std;
  using namespace postmaster;
  using namespace postmaster::io;
  using boost::bind;

  resolver io;
#if 0
  {
    io::socket sin( create_socket(io, STDIN_FILENO) );   close_on_destruction(sin, false);
    io::socket sout( create_socket(io, STDOUT_FILENO) ); close_on_destruction(sout, false);
    cout << "standard input  = socket " << sin << endl
         << "standard output = socket " << sout << endl;
    on_input(sin, bind(print_id, 10u), 3u, bind(socket_timeout, sin, sout, sin));
    on_output(sout, bind(print_id, 20u), 5u, bind(socket_timeout, sin, sout, sout));
  }

  io.schedule(bind(print_id, 3u), 1u);
  io.schedule(bind(print_id, 0u));
  io.schedule(bind(print_id, 4u), 3u);
  io.schedule(bind(print_id, 1u));
  io.schedule(bind(print_id, 2u));
#endif
  io.query_a("peti-ip.localhost", print());
  io.query_a_no_cname("peti-mx.localhost", print());
  io.query_mx("peti-ip.localhost", print());
  io.query_ptr("1.0.0.127.in-addr.arpa", print());

  for (;;)
  {
    try
    {
      io.run();
      break;
    }
    catch(exception const & e)
    {
      std::cout << "*** I/O main loop caught: " << e.what() << endl;
    }
  }

  cout << "postmaster shut down" << endl;
  return 0;
}
