#	Copyright (c) 1998, 2003 Canon Research Centre Europe (CRE).
#
#	You may redistribute and modify this module, provided you duplicate
#	all of the original copyright notices and associated disclaimers.
#	If you have modified the module in any way, you must document the
#	changes, and include a reference to the original distribution.
#
#	THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
#	WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES
#	OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
# Makefile for gdbm interface
# To use it, import Gdbm.hs, and link with Gdbm.o and StdDIS.o
# Can make a profiling version as well.

#GC=/path/to/greencard
TOP=../..

GC=$(TOP)/dist/build/greencard/greencard

# If in a non-standard-place
# GDBMDIR=/usr/gdbm-1.7.3/
GDBMDIR=/usr/lib

PROF=
# PROF=-prof
HC      = ghc

HC_OPTS += $(PROF)
HC_OPTS += -I. -I$(GDBMDIR)
HC_OPTS += -package greencard

LD_OPTS = -L$(GDBMDIR)

all:: Main

test: Main

clean: rm -f Main *.o *.hi Gdbm.hs

%.o : %.hs
	$(HC) $(HC_OPTS) -c $< -o $@

%.o : %.c
	$(HC) $(CC_OPTS) -c $< -o $@

%.hs %_stub_ffi.c %_stub_ffi.h: %.gc
	$(GC) $<

Main: Main.o Gdbm.o Gdbm_stub_ffi.o
	$(HC) $(HC_OPTS) -o Main $^ $(LD_OPTS) -lgdbm

Main.o : Gdbm.o
