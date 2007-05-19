/*
 * Copyright (c) 2006-2007 Peter Simons <simons@cryp.to>
 *
 * This software is provided 'as-is', without any express or
 * implied warranty. In no event will the authors be held liable
 * for any damages arising from the use of this software.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty
 * provided the copyright notice and this notice are preserved.
 */

#include "parser.hpp"

struct target_id : public string
{
  template <class Iterator>
  target_id(Iterator b, Iterator e)
  {
    string::push_back('<');
    string::insert(string::end(), b, e);
    string::push_back('>');
  };
};

struct target_parameter : public string
{
  template <class Iterator>
  target_parameter(Iterator b, Iterator e) : string(++b, --e) { }
};

struct target_parser : public spirit::grammar<target_parser, address_closure::context_t>
{
  target_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> target;

    definition(target_parser const & self)
    {
      using namespace spirit;
      using namespace phoenix;

      target
        = word_p                            [ bind(&address::first)(self.val)  = construct_<target_id>(arg1, arg2) ]
          >> *wsp_p
          >> confix_p('(', *anychar_p, ')') [ bind(&address::second)(self.val) = construct_<target_parameter>(arg1, arg2) ]
        ;
      BOOST_SPIRIT_DEBUG_NODE(target);
    }

    spirit::rule<scannerT> const & start() const { return target; }
  };
};

target_parser const  target_p;

bool parse_route(char const * begin, char const * end, route & result)
{
  using namespace spirit;
  route  rt;

  rule<> route_p
    =  ( address_pattern_p              [ assign_a(rt.first) ]
       | ch_p('@')
       )
    >> wsp_p
    >> list_p
       ( *wsp_p >> ( target_p           [ push_back_a(rt.second) ]
                   | address_pattern_p  [ push_back_a(rt.second) ]
                   )
       , ','
       )
    ;
  bool const full_hit( parse(begin, end, route_p).full );
  std::swap(result, rt);
  return full_hit;
}
