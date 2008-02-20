> {-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

A Walk Through "Config.hs"
==========================

:Author: Peter Simons <simons@cryp.to>
:Date:   2008-02-20
:Note:   This text is *nowhere* near being complete.

.. contents::

Welcome To The Real World
-------------------------

The purpose of this document is to provide a hands-on
introduction to the Postmaster ESMTP server. Naturally, it
is written as a literate Haskell `source code`_, so you can
load it into ``ghci`` and run any of the examples in the
interactive development environment while reading the text.
Note that Postmaster must be linked to the system libraries
``-ladns -lcrypto``, so you'll have to start the interpreter
with those flags given on the command-line. (If you run
``ghci`` from Emacs this should be configured
automatically.)

I have decided against explaining the internals of the
daemon. I'll write this text treating the functions
Postmaster provides just like any other Haskell library. I
think it is better to do it this way because you, as the
user, probably don't care how Postmaster works. You only
care how to configure a real bad-ass MTA. So I'll do just
that and refer you to the `reference documentation`_ for
the details.

::

> module Main where
>
> import System.IO
> import System.Time
> import System.Posix.User
> import Network.Socket ( SockAddr(..) )
> import Data.Char
> import Data.List
> import Postmaster hiding ( main )

> ioBufferSize :: Capacity
> ioBufferSize = 1024

> port :: PortID
> port = PortNumber 2525

> run :: EventT -> IO ()
> run f = main' ioBufferSize port f

You have a working SMTP daemon now. Just start it with ``run
id`` and ``telnet`` to the server::

  $ telnet localhost 2525
  Trying 127.0.0.1...
  Connected to localhost.
  Escape character is '^]'.
  220 peti.cryp.to Postmaster ESMTP Server
  NOOP
  250 Massive system failure. Just kidding ... OK.
  QUIT
  221 peti.cryp.to closing connection
  Connection closed by foreign host.

The default configuration will ...

- accept every HELO and EHLO command,
- accept every MAIL command,
- refuse every RCPT command;
- thus, refuse DATA commands for lack of recipients.

Writing Event Handlers
''''''''''''''''''''''

::

> debugEH :: String -> EventT
> debugEH name f e = do
>   yell (StartEventHandler name e)
>   r@(Reply rc _) <- f e
>   yell (EventHandlerResult name e rc)
>   return r

The constructors ``StartEventHandler`` etc. are defined by
Postmaster; it's one of many ``LogEvents`` you may trigger
whenever you think it's appropriate. By wrapping any event
handler with this combinator, we can trace its input and
output values. With two little helper functions, we can try
it out right away::

> mainDebug :: IO ()
> mainDebug = run (debugEH "default")

If you speak with Postmaster now, you should notice the new
log messages that show up on your ``ghci`` terminal (and in
the system log file you've configured for ``syslog(3)``)::

  SID 1: StartEventHandler "default" Greeting
  SID 1: EventHandlerResult "default" Greeting 220


Standard Unix Configuration
'''''''''''''''''''''''''''

Let's configure a real MTA that actually does something. As
a simple example, let us reimplement the way MTAs have
worked traditionally under Unix: (1) The MTA has a list of
"local hostnames". Any recipient which is not in one of
these domains is refused. (2) Recipients *in* the local
domains are delivered to the Unix user with the same name as
the local part. (3) All system users are valid e-mail
recipients. (4) Everything else needs an manual entry a.k.a.
"alias".

One straightforward way to implement this scheme is by
splitting these requirements into the three functions: The
first one checks whether the address of the ``AddRcptTo``
event is a local hostname; the second checks the system
users database, and the third one unconditionally delivers
to the local-part. By wrapping these functions around each
other in the right order, we get exactly the behavior
described above.

The implementation is trivial::

> localHosts :: [HostName] -> EventT
> localHosts lhosts _ (AddRcptTo (Mailbox _ _ host)) = do
>   if (map toLower host) `elem` lhosts
>      then say 2 5 0 "TODO: We accept everything right now"
>      else say 5 5 3 "unknown recipient"
> localHosts _ f e = f e

For it to work, the list you give the function must contain
the local hostnames in all lower-case, obviously. Note that
this combinator works differently than the earlier ones: It
doesn't use ``f`` as a fallback but *guards* access to ``f``!

Our database lookup isn't complicated either: [5]_ ::

> exposePasswd :: EventT
> exposePasswd f e@(AddRcptTo (Mailbox _ lpart _)) = do
>   pwdentry <- liftIO (getUserEntryForName lpart)
>   if userName pwdentry == lpart   -- kludge for ghc
>      then f e
>      else say 5 5 3 "unknown recipient"
> exposePasswd f e = f e

Another function that guards access to ``f`` on the
``AddRcptTo`` event. And our local mailer is::

 TODO: broken with spooler

 | localProcmail :: EventT
 | localProcmail _ (AddRcptTo mbox@(Mailbox _ lpart _)) =
 |   procmail [mbox] lpart []
 | localProcmail f e = f e

Done. ::

> stdConfig :: EventT
> stdConfig =
>   localHosts myHostNames . exposePasswd -- TODO: . localProcmail

The code is point-free, so it must be good. Now edit the
list of local hostnames to suit your system's setup ... ::

> myHostNames :: [HostName]
> myHostNames = [ "localhost"
>               , "change-me.example.org"
>               ]

and run your MTA::

> stdMTA :: IO ()
> stdMTA = run stdConfig

 | runStdMTA :: [String] -> IO ()
 | runStdMTA = runTest stdConfig

A good test session should be::

> stdTest :: [String]
> stdTest =
>   [ "EHLO [127.0.0.1]"
>   , "MAIL FROM:<\"foo\\\".bar\"@example.net>"
>   , "RCPT TO:<non.existent@localhost>"
>   , "RCPT TO:<root@example.com>"
>   , "RCPT TO:<root@localhost>"
>   , "DATA"
>   , "From: simons@cryp.to (Peter Simons)"
>   , "Subject: Testing Postmaster"
>   , ""
>   , "Won't work anyway."
>   , "."
>   ]

If you run ``testStdMTA stdTest``, you'll most likely find
the e-mail in the file ``/var/mail/root`` now. Procmail,
which we used for local delivery, doesn't care about any
``/etc/mail/aliases`` you might have. Which is good, because
we want to determine the aliases in Postmaster, not
somewhere else.

Aliases and Exploders
'''''''''''''''''''''

Aliases ... phew. That ought to be difficult? ::

> alias :: [(Mailbox, Mailbox)] -> EventT
> alias theDB f e
>   | AddRcptTo mbox <- e
>   , Just mbox' <- lookup mbox theDB
>       = trigger (AddRcptTo mbox')
>   | otherwise  = f e

Why do we need ``trigger``? Instead of that definition, we
could equally well have used::

  alias theDB f e
    | ...
        = alias theDB f (AddRcptTo mbox')

The semantics differ insofar as that this variant will
properly recurse, but it will bypass the access checks that
might have run *before* ``alias`` was even called. We don't
know how deeply nested we are in the event transformer
chain! The function ``triggers`` allows us to call the
entire chain from the beginning.

By-passing the checks might be what you want in some cases,
actually. But I'd rather define an explicit handler for
addresses like that. ``alias`` rewrites addresses; nothing
more, nothing less. Here is a short demo function::

 | runAliasTest :: IO ()
 | runAliasTest = runTest (myalias . stdConfig) stdTest
 |   where
 |   lhs     = read "non.existent@localhost"
 |   rhs     = read "root@localhost"
 |   myalias = alias [(lhs,rhs)]

You will have noticed that the mechanism doesn't look like
the usual aliases file. It maps addresses one-to-one, not
one-to-many. In fact, it is more similar to Sendmail's
``virtusertable``. because our left handside of the rewrite
is a full e-mail address, not just a local part. If we want
to have one-to-many mappings, this a simple way to do it::

> explode :: Mailbox -> Smtpd SmtpReply -> EventT
> explode lhs mkRhs f e
>   | AddRcptTo mbox <- e,  lhs == mbox
>                = mkRhs
>   | otherwise  = f e

 | runExploderTest :: IO ()
 | runExploderTest = runTest (expl . stdConfig) stdTest
 |   where
 |   expl = explode (read "non.existent@localhost")
 |            (do shell [] "cat >/dev/null"
 |                shell [] "cat >/dev/null"
 |                -- add more
 |                say 2 5 0 "great")

Cooler Event Handlers
---------------------

The Generic Environment
'''''''''''''''''''''''

Which brings us to the question of how we write a stateful
handler then? What if we want to keep transient information
for a session -- or beyond the life-time of a session?

For that purpose Postmaster features two finite-map
environments: a global one, and a per-TCP-session one. These
environments work almost exactly the like Shell variables
under Unix do. ::

  local  :: EnvT a -> Smtpd a
  global :: EnvT a -> Smtpd a

Disallow Routing Addresses
''''''''''''''''''''''''''

::

> noRouteAddr :: EventT
> noRouteAddr _ (SetMailFrom (Mailbox (_:_) _ _))
>   = say 5 0 4 "You are kidding, right?"
> noRouteAddr _ (AddRcptTo   (Mailbox (_:_) _ _))
>   = say 5 0 4 "no source routing"
> noRouteAddr f e = f e

Dynamic Blacklisting
''''''''''''''''''''

::

> data (Typeable a) => TimeStamped a = TS ClockTime a
>     deriving (Typeable, Show)
>
> type Blacklist = [TimeStamped HostAddress]
>
> blacklist :: TimeDiff -> EventT
> blacklist ttl f e = do
>   r <- f e
>   if e /= Greeting || isFailure r then return r else do
>     peer <- getPeerAddr
>     case peer of
>       Nothing                       -> return r
>       Just (SockAddrUnix _)         -> return r
>       Just sa@(SockAddrInet _ addr) -> do
>         now <- liftIO getClockTime
>         let delta  = addToClockTime ttl
>             stale  = \(TS ts _) -> delta ts < now
>             clean  = reverse . dropWhile stale . reverse
>             expire = (\bl -> (bl,bl)) . maybe [] clean
>         blackl <- global (modifyVar (mkVar "blacklist") expire)
>         if all (\(TS _ a) -> a /= addr) blackl
>             then return r
>             else do yell (Msg (msg sa))
>                     say 5 5 4 "no SMTP service here"
>       Just (SockAddrInet6 _ _ _ _) -> return r
>   where
>   msg = showString "blacklist: refuse peer " . show

Now we need a function to add a peer to the blacklist
whenever we feel like it::

> ban :: Smtpd ()
> ban = do
>   peer <- getPeerAddr
>   case peer of
>     Nothing                      -> return ()
>     Just (SockAddrUnix _)        -> return ()
>     Just (SockAddrInet6 _ _ _ _) -> return ()
>     Just sa@(SockAddrInet _ a) -> do
>       yell (Msg (msg sa))
>       now <- liftIO getClockTime
>       let a'     = TS now a
>           append = maybe [a'] (\as -> a' : as)
>       global (modifyVar_ (mkVar "blacklist") append)
>       return ()
>   where
>   msg = showString "black-listing peer: " . show

An SMTP reply code of 221 or 421 from the event handler
causes Postmaster to drop the connection after the reply::

> bye :: Smtpd SmtpReply
> bye = do
>   whoami <- myHeloName
>   say 4 2 1 (showString whoami " Hasta la vista, baby.")

::

> impatient :: Int -> EventT
> impatient permFailBound f e = do
>   r@(Reply (Code rc _ _) _) <- f e
>   case rc of
>     PermanentFailure -> do
>       c <- local (tick (mkVar "permFailures"))
>       if c >= permFailBound
>          then ban >> bye
>          else return r
>     _ -> return r

> badass :: EventT
> badass = blacklist ttl . impatient maxPF . noRouteAddr
>   where
>   ttl   = noTimeDiff { tdMin = 30 }
>   maxPF = 3

The Rules Of RFC2821
--------------------

In all of the text I assume you are familiar with
[RFC2821]_. So I'll just explain a few minor details
concerning `how the RFC is implemented <Rfc2821.html>`_ in
Postmaster.

Mailboxes
'''''''''

The data type ``Mailbox`` is of a certain importance in this
text. It is defined like this::

  data Mailbox = Mailbox [String] String String

The most general e-mail address defined in the RFC has the
form ``<[@route,...:]user@domain>``, and ``Mailbox`` mirrors
that exactly. You'll find that the first field, the optional
routing information, is rather unpopular these days. But
what can I do? It is part of an e-mail address.

``Mailbox`` is an instance of ``Read`` and ``Shown``, so you
can use the text-representation to create mailboxes in a
comfortable way. Just use ``read "user@domain.tld"`` and
that's it. In case of mailboxes, ``read . show = id`` holds,
but ``show . read = id`` does *not*, because a mailbox
returned by ``show`` will always be enclosed in angular
brackets. Mailbox is also in class ``Eq``, and ``(mb ==
mb')`` will treat the hostname as case-insensitive, as the
RFC requires.

There are two special mailboxes  defined for the SMTP dialogue::

  nullPath, postmaster :: Mailbox
  nullPath   = Mailbox [] [] []
  postmaster = Mailbox [] "postmaster" []

Don't forget to do something with those. ``MAIL FROM:<>``
and ``RCPT TO:<postmaster>`` must always be valid commands.

SMTP Reply Codes by Function Groups
''''''''''''''''''''''''''''''''''''

These reply codes are suggested in the RFC. You ultimately,
you can do what you want because nobody cares for more than
the first digit anyway.

``500``
  Syntax error, command unrecognized
``501``
  Syntax error in parameters or arguments
``502``
  Command not implemented
``503``
  Bad sequence of commands
``504``
  Command parameter not implemented
``211``
  System status, or system help reply
``214``
  Help message
  (Information on how to use the receiver or the meaning of a
  particular non-standard command; this reply is useful only
  to the human user)
``220``
  <domain> Service ready
``221``
  <domain> Service closing transmission channel. See 421.
``421``
  <domain> Service not available. This may be a reply to any
  command if the service knows it must shut down. When the
  event handler returns this code (or 221), Postmaster will
  drop the connection after handling it.
``250``
  Requested mail action okay, completed
``251``
  User not local; will forward to <forward-path>
``252``
  Cannot VRFY user, but will accept message and attempt
  delivery
``450``
  Requested mail action not taken: mailbox unavailable
``550``
  Requested action not taken: mailbox unavailable
``451``
  Requested action aborted: error in processing
``551``
  User not local; please try <forward-path>
``452``
  Requested action not taken: insufficient system storage
``552``
  Requested mail action aborted: exceeded storage allocation
``553``
  Requested action not taken: mailbox name not allowed
  (e.g., mailbox syntax incorrect)
``554``
  Transaction failed (Or, in the case of a connection-opening
  response, "No SMTP service here")

Notes
-----

.. [1] The port-number argument doesn't have enough
       granularity. I'll soon change that API to expect a
       socket, so that you can specify on which IP address
       to listen, too.

.. [2] I wonder whether I should change that to ``Doc``, to
       allow pretty-printing. Opinions are welcome.


.. [3] In fact, the ``relay`` target is implemented on top
       of ``pipe`` at the moment. Postmaster doesn't have a
       mail queue yet, so it can't relay itself. (That will
       change.) ``relay`` uses the field ``sendmailPath``
       from the configuration and just pipes the message
       into Sendmail with appropriate arguments.

.. [4] Yes, the call to ``sed`` in ``shell`` is not nice.
       That will change. Postmaster does support re-writing
       of the data section already, I just wanted to keep
       the internal structure as simple as possible for the
       time being.

.. [5] GHC seems to have a bug in ``getUserEntryForName``
       which causes it to return an incorrect entry when the
       requested one doesn't exist. Until that's fixed, we
       use the comparison for equality of the user names to
       determine success, rather than catching the exception
       we were supposed to get in case of failure.

.. [6] Mostly because I'll replace it with
     ``Data.Dynamic.Dynamic`` soon anyway.

Change me::

> main :: IO ()
> main = run (badass . stdConfig)

References
----------

.. [RFC2821] Simple Mail Transfer Protocol: http://www.faqs.org/rfcs/rfc2821.html

.. [Postmaster] Homepage: http://postmaster.cryp.to/

.. [Haskell] The Haskell Homepage: http://www.haskell.org/

.. [GHC] The Glorious Haskell Compiler: http://www.haskell.org/ghc/

.. [Sendmail] Sendmail Homepage: http://sendmail.org/

.. [Procmail] Procmail Homepage: http://www.procmail.org/

.. _source code: http://postmaster.cryp.to/tutorial.lhs

.. _reference documentation: index.html

.. _events: Rfc2821.html#t%3AEvent


.. ----- Configure Emacs -----
..
.. Local Variables: ***
.. haskell-program-name: "ghci -ladns -lcrypto" ***
.. End: ***
