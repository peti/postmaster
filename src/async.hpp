// An asynchronously retrieved value t is the result of some outer world
// side-effect. You cannot rely on t to actually appear because the outer
// world has a tendency to not deliver the result you expected. The general
// soundness of the operation is expressed by the error code. If that error
// code compares to true, then t actually is available and can be retrieved
// using the value() method. If a non-existent result t is accessed by
// value() although it doesn't exist, the appropriate error code is thrown.

template <class T>
class async
{
public:
  typedef T                              value_type;
  typedef boost::function1<void, async>  task;

  explicit async(value_type const & val) : _val(val) { }
  explicit async(error_code const & ec, value_type const & val = value_type())  : _ec(ec), _val(val) { }

  operator error_code const & () const { return _ec; }

  void throw_if_error() const { if (!_ec) throw _ec; }

  T &       value()       { throw_if_error(); return _val; }
  T const & value() const { throw_if_error(); return _val; }

private:
  error_code        _ec;
  T                 _val;
};
