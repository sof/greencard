# (GNU) Makefile for Green Card (GHC)

### User serviceable parts  ###

# Options to compile the sources with.
GHC_OPTS = -cpp -fglasgow-exts -fno-prune-tydecls 

PACKAGE=greencard

WAY_p_GHC_OPTS = -prof

##### End of user serviceable parts  #####

ifneq "$(way)" ""
way_ := $(way)_
endif

install-pkg uninstall-pkg ::

ifeq "x$(FOR_SYSTEM)" "xGHC"
GREENCARD = ../src/greencard-bin --libdir=.
endif
ifeq "x$(FOR_SYSTEM)" "xHUGS"
GREENCARD = runhugs -c100 -h2m -P../src: ../src/GreenCard.lhs --libdir=$(libdir)
endif

%.hs %_stub_ffi.c %_stub_ffi.h : %.gc
	$(GREENCARD) $(GREENCARD_OPTS) $<

ifeq "x$(FOR_SYSTEM)" "xGHC"
all :: libHSgreencard.$(way_)a  

PKGCONF_CPP_OPTS += -DLIBDIR=\"${libdir}\"
PKGCONF_CPP_OPTS += -DPACKAGE=\"${PACKAGE}\"
PKGCONF_CPP_OPTS += -DPACKAGE_DEPS='$(patsubst %,"%"$(comma),$(PACKAGE_DEPS)) "base"'
PKGCONF_CPP_OPTS += -DLIBRARY=\"HS$(PACKAGE)\"


%.$(way_)o %.$(way_)hi : %.hs
	$(HC) -package-name $(PACKAGE) $(GHC_OPTS) $(WAY_$(way)_GHC_OPTS) -c $< -o $@ -hisuf $(way_)hi

%.o : %.c
	$(HC) $(CC_OPTS) -c $< -o $@

SRC_HC_OPTS 	+= -package-name $(PACKAGE)

libHSgreencard.$(way_)a : Foreign/GreenCard.$(way_)o Foreign/GreenCard_stub_ffi.o
	ar clqs $@ $^

clean ::
	rm -f Foreign/GreenCard.$(way_)o Foreign/GreenCard_stub_ffi.o libHSgreencard.$(way_)a

install :: libHSgreencard.$(way_)a Foreign/GreenCard.$(way_)hi
	install -D Foreign/GreenCard.$(way_)hi $(libdir)/imports/Foreign/GreenCard.$(way_)hi
	install -D libHSgreencard.$(way_)a $(libdir)/libHSgreencard.$(way_)a

uninstall ::
	rm -f $(libdir)/imports/Foreign/GreenCard.$(way_)hi
	rm -f $(libdir)/libHSgreencard.$(way_)a

ifeq "$(way)" ""

install ::
	install -D Foreign/GreenCard.gc $(libdir)/Foreign/GreenCard.gc
uninstall ::
	rm -f $(libdir)/Foreign/GreenCard.gc 

install-pkg :: package.conf.in
	cpp -x c -DINSTALLING ${PKGCONF_CPP_OPTS} $< \
	| sed -e 's/^#.*$$//g' -e 's/""//g' -e 's/, ]/ ]/g' \
	| ghc-pkg --force --update-package 

uninstall-pkg ::
	ghc-pkg --remove-package=$(PACKAGE)

endif

endif # GHC
ifeq "x$(FOR_SYSTEM)" "xHUGS"

all :: Foreign/GreenCard.hs
all :: Foreign/GreenCard.so

%.so : %.hs %.gc %_stub_ffi.c %_stub_ffi.h
	ffihugs +G -98 $<

install :: all

install :: Foreign/GreenCard.hs
	install -D Foreign/GreenCard.gc $(libdir)/Foreign/GreenCard.gc
	install -D Foreign/GreenCard.hs $(libdir)/Foreign/GreenCard.hs

uninstall :: 
	rm -f $(libdir)/Foreign/GreenCard.gc
	rm -f $(libdir)/Foreign/GreenCard.hs

# No actual ffi in Foreign.GreenCard so nothing to install
# install :: Foreign/GreenCard.so
#         install -D Foreign/GreenCard.so $(libdir)/Foreign/GreenCard.so
# uninstall :: 
# 	rm -f $(libdir)/Foreign/GreenCard.so

endif # HUGS

