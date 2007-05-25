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

#ifndef POSTMASTER_PARSER_HPP_INCLUDED
#define POSTMASTER_PARSER_HPP_INCLUDED

#if 0                           // enable spirit debugging
#  define BOOST_SPIRIT_DEBUG
#  define BOOST_SPIRIT_DEBUG_OUT std::cerr
#endif

#include "postmaster.hpp"
#include "rfc2822/address.hpp"
#include <boost/spirit/phoenix/binders.hpp>

namespace spirit = boost::spirit ;

// ----- Helper Macros

#define PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(NAME) \
  PP_PHOENIX_DEFINE_TRIVIAL_RECORD_ACCESSOR(NAME, typename Record::NAME ## _type &)

#define PP_PHOENIX_DEFINE_METHOD_ACTOR(NAME, TYPE, METHOD)      \
  struct NAME ## _impl                                          \
  {                                                             \
    template <typename Container, typename Item>                \
    struct result                                               \
    {                                                           \
      typedef TYPE type;                                        \
    };                                                          \
    template <typename Container, typename Item>                \
    TYPE operator()(Container & c, Item const & item) const     \
    {                                                           \
      c.METHOD ( item );                                        \
    }                                                           \
  };                                                            \
  phoenix::function<NAME ## _impl> const NAME = NAME ## _impl()

#define PP_PHOENIX_DEFINE_TRIVIAL_METHOD_ACTOR(NAME) \
   PP_PHOENIX_DEFINE_METHOD_ACTOR(NAME, void, NAME)

#define PP_SPIRIT_DEFINE_CLOSURE(NAME)                                          \
  struct NAME ## _closure : public spirit::closure<NAME ## _closure, NAME>      \
  {                                                                             \
    member1 val;                                                                \
  }

#define PP_SPIRIT_DEFINE_GRAMMAR(NAME, CLOSURE, GRAMMAR)                        \
  struct NAME ## _parser                                                        \
  : public spirit::grammar<NAME ## _parser, CLOSURE::context_t>                 \
  {                                                                             \
    NAME ## _parser() { }                                                       \
                                                                                \
    template<typename scannerT>                                                 \
    struct definition                                                           \
    {                                                                           \
      spirit::rule<scannerT>    NAME;                                           \
                                                                                \
      definition(NAME ## _parser const & self)                                  \
      {                                                                         \
        using namespace spirit;                                                 \
        using namespace phoenix;                                                \
        using namespace rfc2822;                                                \
        NAME = GRAMMAR;                                                         \
        BOOST_SPIRIT_DEBUG_NODE(NAME);                                          \
      }                                                                         \
                                                                                \
      spirit::rule<scannerT> const & start() const { return NAME; }             \
    };                                                                          \
  };                                                                            \
  NAME ## _parser const NAME ## _p

#define PP_SPIRIT_DEFINE_PARSER(NAME, GRAMMAR)                                  \
  PP_SPIRIT_DEFINE_CLOSURE(NAME);                                               \
  PP_SPIRIT_DEFINE_GRAMMAR(NAME, NAME ## _closure, GRAMMAR)

// ----- Lazy helper functions for use in actions

PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(first);             // lazy access to pairs
PP_PHOENIX_DEFINE_TUPLE_ACCESSOR(second);
PP_PHOENIX_DEFINE_TRIVIAL_METHOD_ACTOR(push_back);   // lazy access to std::vector

// ----- Parsers

PP_SPIRIT_DEFINE_PARSER
  ( address
  ,   local_part_p              [first(self.val) = arg1]
  >>  ch_p('@')
  >>  domain_p                  [second(self.val) = arg1]
  );

PP_SPIRIT_DEFINE_PARSER
  ( pattern
  , lexeme_d
  [   ch_p('@') >> domain_p     [ second(self.val) = arg1 ]
  |   address_p                 [ self.val = arg1 ]
  |   local_part_p              [ first(self.val)  = arg1 ] >> !ch_p('@')
  ]);

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

PP_SPIRIT_DEFINE_PARSER
  ( target
  ,   word_p                         [ first(self.val) = construct_<target_id>(arg1, arg2) ]
  >>  confix_p('(', *anychar_p, ')') [ second(self.val) = construct_<target_parameter>(arg1, arg2) ]
  );

PP_SPIRIT_DEFINE_CLOSURE(route);

struct route_parser : public spirit::grammar<route_parser, route_closure::context_t>
{
  route_parser() { }

  template<typename scannerT>
  struct definition
  {
    spirit::rule<scannerT> route_p;
    spirit::subrule<0>     lhs_p;
    spirit::subrule<1>     rhs_p;

    definition(route_parser const & self)
    {
      using namespace boost::spirit;
      using namespace phoenix;

      route_p =
        (
          lhs_p
            =  ( pattern_p [ first(self.val) = arg1 ]  |  ch_p('@') )
            >> list_p(rhs_p, ',')

        , rhs_p
            =   target_p       [ push_back(second(self.val), arg1) ]
            |   pattern_p      [ push_back(second(self.val), arg1) ]
        );

      BOOST_SPIRIT_DEBUG_NODE(route_p);
    }

    spirit::rule<scannerT> const & start() const { return route_p; }
  };
};

route_parser const route_p;

#endif // POSTMASTER_PARSER_HPP_INCLUDED
