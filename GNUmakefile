TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS =	       \
    ADNS	       \

PACKAGE		:= postmaster
RELEASEDAY	:= 0.1
VERSION		:= $(RELEASEDAY)
PACKAGE_DEPS	:= base network

SRC_HADDOCK_OPTS += -t "Postmaster ESMTP Server"

# ---------------------------------------------------------------

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk

haddock::	docs/tutorial.html

docs/tutorial.html:	tutorial.lhs
	@lhs2html $<
	@mv tutorial.html $@
