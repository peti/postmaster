class signal_block : private boost::noncopyable
{
  sigset_t  _orig_set;

public:
  signal_block()
  {
    sigset_t all;
    if (!sigfillset(&all))
      throw system_error(errno, errno_ecat, "signal_block: sigfillset(3) failed");
    if (!sigprocmask(SIG_BLOCK, &all, &_orig_set))
      throw system_error(errno, errno_ecat, "signal_block: sigprocmask(2) failed");
  }
  ~signal_block()
  {
    if (!sigprocmask(SIG_SETMASK, &_orig_set, 0))
      throw system_error(errno, errno_ecat, "~signal_block: sigprocmask(2) failed");
  }
};
