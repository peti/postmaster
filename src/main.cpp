#include "io.hpp"
#include <iostream>

void print_id(unsigned int id) { std::cout << "task id " << id << std::endl; }

void socket_timeout(postmaster::io::socket s1, postmaster::io::socket s2, postmaster::io::socket s)
{
  BOOST_ASSERT(s == s1 || s == s2);
  std::cout << "timeout on socket " << s << std::endl;
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

  static scheduler io;
  {
    socket sin( create_socket(io, STDIN_FILENO) ); sin->close_on_destruction(false);
    socket sout( create_socket(io, STDOUT_FILENO) ); sout->close_on_destruction(false);
    cout << "standard input  = socket " << sin << endl
         << "standard output = socket " << sout << endl;
    on_input(sin, bind(print_id, 10u), 3u, bind(socket_timeout, sin, sout, _1));
    on_output(sout, bind(print_id, 20u), 5u, bind(socket_timeout, sin, sout, _1));
  }

  io.schedule(bind(print_id, 3u), 1u);
  io.schedule(bind(print_id, 0u));
  io.schedule(bind(print_id, 4u), 3u);
  io.schedule(bind(print_id, 1u));
  io.schedule(bind(print_id, 2u));

  int pipe_fds[2];
  if (pipe(pipe_fds) != 0)
    throw system_error(errno, errno_ecat, "pipe(2) failed");

  for(;;)
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
