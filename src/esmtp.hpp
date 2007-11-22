#ifndef POSTMASTER_ESMTP_2007_11_18
#define POSTMASTER_ESMTP_2007_11_18

#include "parser.hpp"
#include <boost/bind.hpp>
#include <boost/assert.hpp>

namespace postmaster
{
  namespace esmtp
  {
    typedef char const                                  char_type;
    typedef char_type *                                 iterator;
    typedef std::pair<iterator,iterator>                range;

    typedef std::pair<char const *, char const *>       range;
    PP_SPIRIT_DEFINE_CLOSURE(range);

    spirit::chlit<char_type> const                      sp_p           = ' ';
    spirit::strlit<iterator> const                      crlf_p         = "\r\n";

    PP_SPIRIT_DEFINE_GRAMMAR( line, range_closure, lexeme_d[*(~chset_p("\r\n"))] [self.val = construct_<range>(arg1, arg2)] );
    PP_SPIRIT_DEFINE_GRAMMAR( partial_line, range_closure
                            , lexeme_d[eps_p(line_p >> !ch_p('\r') >> end_p) >> epsilon_p] [self.val = construct_<range>(arg1, arg2)]
                            );

    PP_SPIRIT_DEFINE_GRAMMAR(hostname, range_closure, lexeme_d[+chset_p("a-zA-Z0-9.-")] [self.val = construct_<range>(arg1, arg2)]);

    class server
    {
    public:
      typedef boost::spirit::parse_info<iterator>       parse_info_t;

      iterator operator() (iterator begin, iterator end)
      {
        using namespace spirit;
        using namespace phoenix;
        using boost::bind;
        using boost::cref;
        range str_arg;
        parse_info_t const r( parse( begin, end
                                   , +( as_lower_d["ehlo"] >> hostname_p[assign_a(str_arg)] >> crlf_p [ bind(&server::ehlo, this, cref(str_arg)) ]
                                      | as_lower_d["helo"] >> hostname_p[assign_a(str_arg)] >> crlf_p [ bind(&server::helo, this, cref(str_arg)) ]
                                      | as_lower_d["noop"] >> line_p[assign_a(str_arg)]     >> crlf_p [ bind(&server::noop, this, cref(str_arg)) ]
                                      | line_p[assign_a(str_arg)]                           >> crlf_p [ bind(&server::unrecognized, this, cref(str_arg)) ]
                                      )
                                   | partial_line_p
                                   , sp_p
                                   ));
        BOOST_ASSERT(!r.hit || r.stop <= end);
        BOOST_ASSERT(!r.hit || r.stop >= begin);
        BOOST_ASSERT(!r.hit || r.stop >= begin + r.length);
        return (r.hit ? r.stop : 0);
      }

      void ehlo(range const & name)             { std::cout << "EHLO \"" << std::string(name.first, name.second) << '\"' << std::endl;  }
      void helo(range const & name)             { std::cout << "HELO \"" << std::string(name.first, name.second) << '\"' << std::endl;  }
      void noop(range const & str)              { std::cout << "NOOP \"" << std::string(str.first,  str.second)  << '\"' << std::endl;  }
      void unrecognized(range const & line)     { std::cout << "unrecognized \"" << std::string(line.first, line.second) << '\"' << std::endl;  }
    };


    // 4.1.1.1  Extended HELLO (EHLO) or HELLO (HELO)
    //
    // Command:
    //
    //       ehlo            = "EHLO" SP Domain CRLF
    //       helo            = "HELO" SP Domain CRLF

    // Response:
    //
    //       ehlo-ok-rsp  =    ( "250"    domain [ SP ehlo-greet ] CRLF )
    //                    / (    "250-"   domain [ SP ehlo-greet ] CRLF
    //                        *( "250-"   ehlo-line                CRLF )
    //                           "250"    SP ehlo-line             CRLF  )
    //
    //       ehlo-greet   = 1*(%d0-9 / %d11-12 / %d14-127)
    //                    ; string of any characters other than CR or LF
    //
    //       ehlo-line    = ehlo-keyword *( SP ehlo-param )
    //
    //       ehlo-keyword = (ALPHA / DIGIT) *(ALPHA / DIGIT / "-")
    //                    ; additional syntax of ehlo-params depends on
    //                    ; ehlo-keyword
    //
    //       ehlo-param   = 1*(%d33-127)
    //                    ; any CHAR excluding <SP> and all
    //                    ; control characters (US-ASCII 0-31 inclusive)

    // 4.1.1.2 MAIL (MAIL)
    //
    //       "MAIL FROM:" ("<>" / Reverse-Path)
    //                        [SP Mail-parameters] CRLF

    // 4.1.1.3 RECIPIENT (RCPT)
    //
    //    "RCPT TO:" ("<Postmaster@" domain ">" / "<Postmaster>" / Forward-Path)
    //                     [SP Rcpt-parameters] CRLF

    // 4.1.1.4 DATA (DATA)
    //
    //       "DATA" CRLF

    // 4.1.1.5 RESET (RSET)
    //
    //       "RSET" CRLF

    // 4.1.1.6 VERIFY (VRFY)
    //
    //       "VRFY" SP String CRLF

    // 4.1.1.7 EXPAND (EXPN)
    //
    //       "EXPN" SP String CRLF

    // 4.1.1.8 HELP (HELP)
    //
    //       "HELP" [ SP String ] CRLF

    // 4.1.1.9 NOOP (NOOP)
    //
    //       "NOOP" [ SP String ] CRLF

    // 4.1.1.10 QUIT (QUIT)
    //
    //       "QUIT" CRLF

    // 4.1.2 Command Argument Syntax
    //
    //       Reverse-path = Path
    //       Forward-path = Path
    //       Path = "<" [ A-d-l ":" ] Mailbox ">"
    //       A-d-l = At-domain *( "," A-d-l )
    //             ; Note that this form, the so-called "source route",
    //             ; MUST BE accepted, SHOULD NOT be generated, and SHOULD be
    //             ; ignored.
    //       At-domain = "@" domain
    //       Mail-parameters = esmtp-param *(SP esmtp-param)
    //       Rcpt-parameters = esmtp-param *(SP esmtp-param)
    //
    //       esmtp-param     = esmtp-keyword ["=" esmtp-value]
    //       esmtp-keyword   = (ALPHA / DIGIT) *(ALPHA / DIGIT / "-")
    //       esmtp-value     = 1*(%d33-60 / %d62-127)
    //             ; any CHAR excluding "=", SP, and control characters
    //       Keyword  = Ldh-str
    //       Argument = Atom
    //       Domain = (sub-domain 1*("." sub-domain)) / address-literal
    //       sub-domain = Let-dig [Ldh-str]
    //
    //       address-literal = "[" IPv4-address-literal /
    //                             IPv6-address-literal /
    //                             General-address-literal "]"
    //             ; See section 4.1.3
    //
    //       Mailbox = Local-part "@" Domain
    //
    //       Local-part = Dot-string / Quoted-string
    //             ; MAY be case-sensitive
    //
    //       Dot-string = Atom *("." Atom)
    //
    //       Atom = 1*atext
    //
    //       Quoted-string = DQUOTE *qcontent DQUOTE
    //
    //       String = Atom / Quoted-string

    // 4.1.3 Address Literals
    //
    //       IPv4-address-literal = Snum 3("." Snum)
    //       IPv6-address-literal = "IPv6:" IPv6-addr
    //       General-address-literal = Standardized-tag ":" 1*dcontent
    //       Standardized-tag = Ldh-str
    //             ; MUST be specified in a standards-track RFC
    //             ; and registered with IANA
    //
    //       Snum = 1*3DIGIT  ; representing a decimal integer
    //             ; value in the range 0 through 255
    //       Let-dig = ALPHA / DIGIT
    //       Ldh-str = *( ALPHA / DIGIT / "-" ) Let-dig
    //
    //       IPv6-addr = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
    //       IPv6-hex  = 1*4HEXDIG
    //       IPv6-full = IPv6-hex 7(":" IPv6-hex)
    //       IPv6-comp = [IPv6-hex *5(":" IPv6-hex)] "::" [IPv6-hex *5(":"
    //                  IPv6-hex)]
    //             ; The "::" represents at least 2 16-bit groups of zeros
    //             ; No more than 6 groups in addition to the "::" may be
    //             ; present
    //       IPv6v4-full = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
    //       IPv6v4-comp = [IPv6-hex *3(":" IPv6-hex)] "::"
    //                    [IPv6-hex *3(":" IPv6-hex) ":"] IPv4-address-literal
    //             ; The "::" represents at least 2 16-bit groups of zeros
    //             ; No more than 4 groups in addition to the "::" and
    //             ; IPv4-address-literal may be present

    // 4.2 SMTP Replies
    //
    //       Greeting = "220 " Domain [ SP text ] CRLF
    //       Reply-line = Reply-code [ SP text ] CRLF

    // 4.2.1 Reply Code Severities and Theory
    //
    //    1yz   Positive Preliminary reply
    //    2yz   Positive Completion reply
    //    3yz   Positive Intermediate reply
    //    4yz   Transient Negative Completion reply
    //    5yz   Permanent Negative Completion reply
    //
    //    x0z   Syntax: These replies refer to syntax errors, syntactically
    //       correct commands that do not fit any functional category, and
    //       unimplemented or superfluous commands.
    //
    //    x1z   Information:  These are replies to requests for information,
    //       such as status or help.
    //
    //    x2z   Connections: These are replies referring to the transmission
    //       channel.
    //
    //    x3z   Unspecified.
    //
    //    x4z   Unspecified.
    //
    //    x5z   Mail system: These replies indicate the status of the receiver
    //       mail system vis-a-vis the requested transfer or other mail system
    //       action.

    // 4.4 Trace Information
    //
    // Return-path-line = "Return-Path:" FWS Reverse-path <CRLF>
    //
    // Time-stamp-line = "Received:" FWS Stamp <CRLF>
    //
    // Stamp = From-domain By-domain Opt-info ";"  FWS date-time
    //
    //       ; where "date-time" is as defined in [32]
    //       ; but the "obs-" forms, especially two-digit
    //       ; years, are prohibited in SMTP and MUST NOT be used.
    //
    // From-domain = "FROM" FWS Extended-Domain CFWS
    //
    // By-domain = "BY" FWS Extended-Domain CFWS
    //
    // Extended-Domain = Domain /
    //            ( Domain FWS "(" TCP-info ")" ) /
    //            ( Address-literal FWS "(" TCP-info ")" )
    //
    // TCP-info = Address-literal / ( Domain FWS Address-literal )
    //       ; Information derived by server from TCP connection
    //       ; not client EHLO.
    //
    // Opt-info = [Via] [With] [ID] [For]
    //
    // Via = "VIA" FWS Link CFWS
    //
    // With = "WITH" FWS Protocol CFWS
    //
    // ID = "ID" FWS String / msg-id CFWS
    //
    // For = "FOR" FWS 1*( Path / Mailbox ) CFWS
    //
    // Link = "TCP" / Addtl-Link
    // Addtl-Link = Atom
    //       ; Additional standard names for links are registered with the
    //          ; Internet Assigned Numbers Authority (IANA).  "Via" is
    //          ; primarily of value with non-Internet transports.  SMTP
    //          ; servers SHOULD NOT use unregistered names.
    // Protocol = "ESMTP" / "SMTP" / Attdl-Protocol
    // Attdl-Protocol = Atom
    //       ; Additional standard names for protocols are registered with the
    //          ; Internet Assigned Numbers Authority (IANA).  SMTP servers
    //          ; SHOULD NOT use unregistered names.
  }
}

#endif // POSTMASTER_ESMTP_2007_11_18
