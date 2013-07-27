# Toplevel Makefile for GreenCard
# 
# Typical usage:
#
# Linux:   sudo make install
#          sudo make prefix=$HOME install
# Windows: make prefix=c:/greencard install
#

# === User serviceable parts:

# Path to Haskell compiler
HC=ghc

# Path to C compiler
CC      = gcc

# When building for GHC, we can build for profiling, etc.
WAYS=p

# You only need to adjust these if you
# should need to change the parser
HAPPY = happy

# Set to 1 if you're compiling for Win32
PURE_WIN32=0
# [ The effect of this option is to control whether
#   ';' (PURE_WIN32=1) or ':' (PURE_WIN32=0) is used
#   as path separator when GreenCard processes the -i
#   option.
# ]

# installation directories - override when invoking make
prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib/greencard-3.01

# === End of user-serviceable parts

HC_NAME = $(notdir $(shell which $(HC)))

# What system you're building for.
#  Legal values: GHC, NHC or HUGS
FOR_SYSTEM:=$(shell bash -c "case ${HC} in (*ghc*) echo GHC ;; (*hugs*) echo HUGS ;; (*nhc*) echo NHC ;; esac")

SUBDIRS = src lib

ifneq "$(FOR_SYSTEM)" "GHC"
WAYS=
endif

all clean install install-pkg uninstall uninstall-pkg ::
	@for way in "" $(WAYS) ; do \
	  for i in $(SUBDIRS) ; do \
	    $(MAKE) -C $$i -f Makefile $(MFLAGS) $@ way=$${way} bindir='$(bindir)' libdir='$(libdir)' FOR_SYSTEM='$(FOR_SYSTEM)' HC='$(HC)' CC='$(CC)' HAPPY='$(HAPPY)' PURE_WIN32=$(PURE_WIN32); \
	  done \
	done

show-install :
	@echo "bindir = $(bindir)"
	@echo "libdir = $(libdir)"
	@echo "FOR_SYSTEM = $(FOR_SYSTEM)" 
	@echo "HC = $(HC)"                 
	@echo "HC_NAME = $(HC_NAME)"                 
	@echo "WAYS = $(WAYS)"                 
	@echo "CC = $(CC)"                 
	@echo "HAPPY = $(HAPPY)"                 
	@echo "PURE_WIN32 = $(PURE_WIN32)"                 

