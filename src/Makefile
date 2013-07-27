# (GNU) Makefile for Green Card (GHC)

### User serviceable parts  ###

# Options to compile the sources with.

# For GHC:
SRC_HC_OPTS    += -recomp -O -fglasgow-exts 


# Uncomment this if you're using a 'happy -g' parser.
# (the one shipped with the distribution is not.)
#Parse_HC_OPTS  += -cpp -fglasgow-exts -DBEGIN_GHC_ONLY='-}' -DEND_GHC_ONLY='{-'
Parse_HC_OPTS   += -Onot
GreenCard_HC_OPTS += -cpp -DBEGIN_GHC_ONLY='-}' -DEND_GHC_ONLY='{-' -DBEGIN_NOT_FOR_GHC='{-' -DEND_NOT_FOR_GHC='-}' -DPURE_WIN32=$(PURE_WIN32)

# For NHC:
#SRC_HC_OPTS = ???

##### End of user serviceable parts  #####

PROGRAM=greencard

SRCS = \
Casm.lhs DIS.lhs Decl.lhs ErrMonad.lhs FillIn.lhs \
FillInMonad.lhs GCToken.lhs \
GreenCard.lhs Lex.lhs LexM.lhs ListUtils.lhs MarshallMonad.lhs \
Name.lhs NameSupply.lhs Package.lhs \
PrettyUtils.lhs Proc.lhs Process.lhs Target.lhs Type.lhs

BOOT_SRCS = Parse.hs
ALL_SRCS  = $(SRCS) $(BOOT_SRCS)

# nullops in this directory
install-pkg uninstall-pkg ::


ifeq "x$(FOR_SYSTEM)" "xHUGS"

all :: $(PROGRAM)

$(PROGRAM) : greencard.hugs.in
	sed < $^ > $@ "s|@libdir@|${libdir}|g"
	chmod a+x $@

clean ::
	rm -f $(PROGRAM)

install :: $(PROGRAM)
	install -D $(PROGRAM) $(bindir)/$(PROGRAM)

uninstall :: 
	rm -f $(bindir)/$$i

all :: $(ALL_SRCS)

install :: all
	for i in $(ALL_SRCS); do \
		case $$i in \
		  (*.hs | *.lhs) \
		    install -D $$i $(libdir)/src/$$i; \
		esac; \
	done

uninstall ::
	@for i in $(ALL_SRCS) "" ; do			\
		rm -f $(libdir)/$$i;		\
	done

endif # HUGS

ifeq "x$(FOR_SYSTEM)" "xGHC"

ifneq "$(way)" "" 

all ::
clean ::
install ::
uninstall ::

else

ALL_SRCS += ErrorHook.c
ALL_OBJS  = $(patsubst %, %.o,$(basename $(ALL_SRCS)))

HC_OPTS = $(SRC_HC_OPTS) $(WAY$(_way)_HC_OPTS) $($*_HC_OPTS) $(EXTRA_HC_OPTS)
CC_OPTS = $(SRC_CC_OPTS) $(WAY$(_way)_CC_OPTS) $($*_CC_OPTS) $(EXTRA_CC_OPTS)
HAPPY_OPTS =

# Generating dependcies
MKDEPEND      = $(HC) -M
MKDEPEND_OPTS = -optdep-f -optdep.depend -optdep-xFiniteMap -optdep-xPretty
MKDEPEND_SRCS = $(SRCS) $(BOOT_SRCS)

# Targets
all     :: .depend
all	:: greencard-bin
all     :: $(PROGRAM)

install :: all
	install -D greencard-bin $(libdir)/greencard-bin
	install -D $(PROGRAM) $(bindir)/$(PROGRAM)

uninstall ::
	$(RM) $(libdir)/greencard-bin
	$(RM) $(bindir)/$(PROGRAM)

$(PROGRAM) : greencard.ghc.in
	sed < $^ > $@ "s|@libdir@|${libdir}|g"
	chmod a+x $@

.depend :: Parse.hs

show :
	@echo $(ALL_OBJS)

.depend :: $(MKDEPEND_SRCS)
	$(RM) .depend
	touch .depend
	$(MKDEPEND) $(MKDEPEND_OPTS) $(HC_OPTS) $(MKDEPEND_SRCS)

# No need to build this target explicitly, but in case somebody wants to...
depend : .depend

clean ::
	$(RM) *.o *.hi $(PROGRAM) .depend

greencard-bin :: $(ALL_OBJS)
	$(HC) $(HC_OPTS) -o $@ $^

# Suffix rules (GNU make.)
%.o : %.lhs	 
	$(HC) $(HC_OPTS) -c $< -o $@

%.o : %.hs
	$(HC) $(HC_OPTS) -c $< -o $@

%.hi : %.o
	@:

%.hs : %.ly
	$(HAPPY) $(HAPPY_OPTS) $<

%.hs : %.lx
	$(LX) $(LX_OPTS) $<

%.o : %.c
	$(CC) $(CC_OPTS) -c $< -o $@

ErrorHook.o : ErrorHook.c
	$(HC) $(CC_OPTS) -c $< -o $@

# Dependencies
-include .depend

endif # way == ""

endif # GHC