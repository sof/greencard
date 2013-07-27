SUBDIRS = Gdbm

all ::
	@for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i -f Makefile $(MFLAGS) HC=$(HC) FOR_SYSTEM=$(FOR_SYSTEM) $@; \
	done
