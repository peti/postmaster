# GNUmakefile -- build Postmaster

##### paths and programs

GHC	 := ghc
OBJDIR	 := .objs
HFLAGS	 := -threaded -O0 -Wall \
	    -iblockio -ihopenssl -imonadenv -isyslog -ichild \
	    '-\#include <openssl/evp.h>' \
	    -odir $(OBJDIR) -hidir $(OBJDIR)
DOCDIR	 := docs
HADDOCK	 := haddock
HSC2HS	 := hsc2hs
HDI_PATH := http://haskell.org/ghc/docs/latest/html/libraries
HDI_FILE := /usr/local/ghc-current/share/ghc-6.5/html/libraries
HDIFILES := \
  -i $(HDI_PATH)/base,$(HDI_FILE)/base/base.haddock \
  -i $(HDI_PATH)/network,$(HDI_FILE)/network/network.haddock \
  -i $(HDI_PATH)/mtl,$(HDI_FILE)/mtl/mtl.haddock \
  -i $(HDI_PATH)/unix,$(HDI_FILE)/unix/unix.haddock \
  -i $(HDI_PATH)/parsec,$(HDI_FILE)/parsec/parsec.haddock

MONODIRS := blockio child hopenssl monadenv syslog

##### build postmaster binary

.PHONY: all

SRCS := Postmaster.hs				\
	Postmaster/Base.hs			\
	Postmaster/FSM.hs			\
	Postmaster/FSM/Announce.hs		\
	Postmaster/FSM/DNSResolver.hs		\
	Postmaster/FSM/DataHandler.hs		\
	Postmaster/FSM/EhloPeer.hs		\
	Postmaster/FSM/EventHandler.hs		\
	Postmaster/FSM/HeloName.hs		\
	Postmaster/FSM/MailFrom.hs		\
	Postmaster/FSM/MailID.hs		\
	Postmaster/FSM/PeerHelo.hs		\
	Postmaster/FSM/SessionState.hs		\
	Postmaster/FSM/Spooler.hs		\
	Postmaster/IO.hs			\
	Postmaster/Main.hs			\
	blockio/BlockIO.hs			\
	child/Child.hs				\
	hopenssl/Digest.hs			\
	monadenv/MonadEnv.hs			\
	syslog/Syslog.hs

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
	@$(HADDOCK) $(HDIFILES) -s .. -t Postmaster -h \
		-o $(DOCDIR) $(SRCS)

$(DOCDIR)/tutorial.html:	tutorial.lhs
	@lhs2html $<
	@mv tutorial.html $@

NOTES:		.todo
	@devtodo -T -f all --date-format '%Y-%m-%d'
	@mv TODO $@

##### TAGS file

.PHONY: tags

tags::
	@rm -f TAGS
	@$(MAKE) TAGS

TAGS:		$(SRCS)
	@rm -f tags TAGS
	@echo "Updating TAGS ... "
	@(find . -name "*.hs*" && \
	  find /usr/local/src/ghc-current/libraries -name "*.hs*") \
	   | xargs hasktags
	@rm -f tags

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
	@rm -f postmaster TODO TAGS tags

distclean::	clean
	@rm -rf $(DOCDIR) index.html
	@rm -f postmaster-*.tar.gz NOTES
	@rm -rf $(MONODIRS)

redate::
	redate Postmaster.hs Postmaster/*.hs* tutorial.lhs README

init-src::	$(MONODIRS) $(SRCS)
	@-mkdir $(DOCDIR)
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc

$(MONODIRS):
	monotone --db=/home/monodbs/simons.db --branch=to.cryp.hs.$@ co $@
