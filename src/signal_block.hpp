class signal_block : private boost::noncopyable
{
  sigset_t  _sigset;

public:
  signal_block()
  {
    sigset_t all;
    if (!sigfillset(&all))
      throw system_error(errno, errno_ecat, "signal_block: sigfillset(3) failed");
    if (!sigprocmask(SIG_BLOCK, &all, &_sigset))
      throw system_error(errno, errno_ecat, "signal_block: sigprocmask(2) failed");
  }
  ~signal_block()
  {
    if (!sigprocmask(SIG_SETMASK, &_sigset, 0))
      throw system_error(errno, errno_ecat, "~signal_block: sigprocmask(2) failed");
  }
};
