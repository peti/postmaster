# GNUmakefile -- build Postmaster

##### paths and programs

GHC	 := ghc
OBJDIR	 := .objs
HFLAGS	 := -threaded -O2 -funbox-strict-fields -Wall   \
	    -odir $(OBJDIR) -hidir $(OBJDIR)		\
	    -ihsdns	-ignore-package hsdns		\
		'-\#include <adns.h>'			\
		'-\#include <errno.h>'			\
	    -ihsemail	-ignore-package hsemail		\
	    -ihopenssl	-ignore-package hopenssl	\
		'-\#include <openssl/evp.h>'		\
	    -ihsyslog	-ignore-package hsyslog		\

DOCDIR	 := docs
HADDOCK	 := haddock
HSC2HS	 := hsc2hs
HDI_PATH := http://haskell.org/ghc/docs/latest/html/libraries
HDI_FILE := /usr/local/ghc-current/share/ghc-6.7/html/libraries
HDIFILES :=							\
  -i $(HDI_PATH)/base,$(HDI_FILE)/base/base.haddock		\
  -i $(HDI_PATH)/network,$(HDI_FILE)/network/network.haddock	\
  -i $(HDI_PATH)/mtl,$(HDI_FILE)/mtl/mtl.haddock		\
  -i $(HDI_PATH)/unix,$(HDI_FILE)/unix/unix.haddock		\
  -i $(HDI_PATH)/parsec,$(HDI_FILE)/parsec/parsec.haddock

##### build postmaster binary

.PHONY: all

SRCS := Postmaster.hs					\
	Postmaster/Base.hs				\
	Postmaster/FSM.hs				\
	Postmaster/FSM/Announce.hs			\
	Postmaster/FSM/DNSResolver.hs			\
	Postmaster/FSM/DataHandler.hs			\
	Postmaster/FSM/EhloPeer.hs			\
	Postmaster/FSM/EventHandler.hs			\
	Postmaster/FSM/HeloName.hs			\
	Postmaster/FSM/MailFrom.hs			\
	Postmaster/FSM/MailID.hs			\
	Postmaster/FSM/PeerAddr.hs			\
	Postmaster/FSM/PeerHelo.hs			\
	Postmaster/FSM/SessionState.hs			\
	Postmaster/FSM/Spooler.hs			\
	Postmaster/IO.hs				\
	Postmaster/Main.hs				\
	hsdns/ADNS.hs					\
	hsdns/ADNS/Base.hs				\
	hsdns/ADNS/Endian.hs				\
	hsdns/ADNS/Resolver.hs				\
	hsemail/Text/ParserCombinators/Parsec/Rfc2234.hs \
	hsemail/Text/ParserCombinators/Parsec/Rfc2821.hs \
	hopenssl/OpenSSL/Digest.hs			\
	hsyslog/System/Posix/Syslog.hs

all::	postmaster

postmaster:	$(SRCS) tutorial.lhs
	@if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	@rm -f $(OBJDIR)/Main.o $(OBJDIR)/Main.hi
	$(GHC) --make $(HFLAGS) -o $@ tutorial.lhs -ladns -lcrypto

%.hs : %.hsc
	$(HSC2HS) $<

##### documentation

.PHONY: docs

docs::		$(DOCDIR)/tutorial.html
	@rm -f $(DOCDIR)/index.html
	@$(MAKE) $(DOCDIR)/index.html

$(DOCDIR)/index.html:	$(SRCS)
	@echo "Build documentation ..."
	@if [ ! -d $(DOCDIR) ]; then mkdir $(DOCDIR); fi
	@$(HADDOCK) $(HDIFILES) -s ../%F -t Postmaster -h \
		-o $(DOCDIR) $(SRCS)

$(DOCDIR)/tutorial.html:	tutorial.lhs
	@lhs2html $<
	@mv tutorial.html $@

NOTES:		.todo
	@devtodo -T -f all --date-format '%Y-%m-%d'
	@mv TODO $@

##### distribution

.PHONY: dist mkdist

dist:		clean index.html $(DOCDIR)/index.html $(DOCDIR)/tutorial.html

mkdist:
	@darcs dist --dist-name postmaster-`date --iso-8601`

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

##### administrative targets

.PHONY: clean distclean redate init-src

clean::
	@rm -rf $(OBJDIR)
	@rm -f postmaster

distclean::	clean
	@rm -rf $(DOCDIR) index.html
	@rm -f postmaster-*.tar.gz

redate::
	redate README
