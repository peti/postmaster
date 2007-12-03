#include "../src/parser.hpp"
#include <map>
#include <iostream>
#include <iterator>
#include <fstream>
#include <boost/range.hpp>
#include <boost/bind.hpp>

#define BOOST_AUTO_TEST_MAIN
#include <boost/test/unit_test.hpp>

namespace phoenix
{
  template <class Value>
  inline std::vector<Value> & operator+= (std::vector<Value> & v, Value const & val)
  {
    v.push_back(val);
    return v;
  }
}

PP_SPIRIT_DEFINE_TRIVIAL_GRAMMAR(atom,          lexeme_d[ +(~(range_p('\x00', '\x1F') | chset_p(" \\\"#=[]\x7F"))) ]);
PP_SPIRIT_DEFINE_TRIVIAL_GRAMMAR(comment,       lexeme_d[ ch_p('#') >> *(~chset_p("\r\n")) ]);
PP_SPIRIT_DEFINE_TRIVIAL_GRAMMAR(quoted_pair,   lexeme_d[ ch_p('\\') >> anychar_p ]);

PP_SPIRIT_DEFINE_TRIVIAL_GRAMMAR( quoted_string
                                , lexeme_d [ ch_p('"') >> *( quoted_pair_p | +(~chset_p("\\\"\r\n")) ) >> '"' ]
                                );

PP_SPIRIT_DEFINE_TRIVIAL_GRAMMAR(word, atom_p | quoted_string_p);

typedef std::vector<std::string>                word_list;
typedef std::pair<std::string,word_list>        assignment;
typedef std::vector<assignment>                 variables;

inline std::ostream & operator<< (std::ostream & os, assignment const & a)
{
  os << a.first << " = ";
  std::copy(a.second.begin(), a.second.end(), std::ostream_iterator<std::string>(os, " "));
  return os;
}

inline std::ostream & operator<< (std::ostream & os, variables const & vars)
{
  for (variables::const_iterator i( vars.begin() ); i != vars.end(); ++i)
    os << *i << std::endl;
  return os;
}

PP_SPIRIT_DEFINE_PARSER( assignment
                       ,  atom_p [bind(&assignment::first)(self.val) = construct_<std::string>(arg1,arg2)]
                       >> !ch_p('=')
                       >> +word_p [bind(&assignment::second)(self.val) += construct_<std::string>(arg1, arg2)]
                       );

struct variable_closure : public spirit::closure<variable_closure, variables, std::string, std::string>
{
  member1 vars;
  member2 section;
  member3 name;
};

void add_section(variable_closure const & ctx)
{
  word_list val;
  val.push_back(ctx.name());
  ctx.vars().push_back(assignment(ctx.section() + ".name", val));
}

void add_variable(variable_closure const & ctx, assignment const & a)
{
  ctx.vars().push_back(assignment(ctx.section() + '.' + ctx.name() + '.' + a.first, a.second));
}

PP_SPIRIT_DEFINE_GRAMMAR( cf_file, variable_closure
                        ,  eps_p [self.section = "route"]
                        >> eps_p [self.name    = "\"\""]
                        >> *( !(assignment_p [bind(&add_variable)(boost::cref(self), arg1)]) >> eol_p )
                        >> *(  ch_p('[')
                            >> atom_p             [self.section = construct_<std::string>(arg1,arg2)]
                            >> ( quoted_string_p  [self.name    = construct_<std::string>(arg1,arg2)]
                               | eps_p            [self.name    = "\"\""]
                               )
                            >> ch_p(']')          [bind(&add_section)(boost::cref(self))]
                            >> *( !(assignment_p  [bind(&add_variable)(boost::cref(self), arg1)]) >> eol_p )
                            )
                        );

struct print
{
  typedef void result_type;

  print(std::string const & ctx) : context(ctx) { }

  std::string context;

  template <class Iterator>
  void operator() (Iterator begin, Iterator end) const
  {
    std::cout << context << "(" << std::string(begin, end) << ") ";
  }

  void operator() (assignment const & a) const
  {
    std::cout << a << std::endl;
  }

  void operator() (variables const & vars) const
  {
    std::cout << vars;
  }
};

void print_eol(char const *, char const *)
{
  std::cout << std::endl;
}

BOOST_AUTO_TEST_CASE( test_cf_file_parser )
{
  using namespace std;
  using namespace spirit;
  using namespace phoenix;

  std::vector<char> buf;
  for (ifstream fh("postmaster.cf"); fh; /**/)
  {
    string line;
    getline(fh, line);
    buf.insert(buf.end(), line.begin(), line.end());
    if (fh) buf.push_back('\n');
  }

  string current_section = "route";
  parse_info<> const r( parse( &buf[0], &buf[buf.size()]
                             , cf_file_p [print("postmaster.cf")]
                             , blank_p | ::comment_p
                             )
                      );

  cout << "read " << buf.size() << " characters." << endl
       << r << endl;
}
