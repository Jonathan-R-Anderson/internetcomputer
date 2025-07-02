# Set DFLAGS
UDFLAGS:=-conf= -Isrc -Iimport -w -de -preview=dip1000 -preview=fieldwise $(MODEL_FLAG) $(PIC) $(OPTIONAL_COVERAGE) -preview=dtorfields
D_UDFLAGS:=$(UDFLAGS)
ifeq ($(BUILD),debug)
	D_UDFLAGS += -g -debug
	DFLAGS:=$(D_UDFLAGS)
else
	D_UDFLAGS += -O -release
	DFLAGS:=$(D_UDFLAGS) -inline # unittests don't compile with -inline
endif

$(PHOBOS) : $(OBJS) $(COBJS)
	rm -f $@
	$(AR) -r $@ $^

$(OBJDIR)/%$(DOTOBJ) : src/%.d $(DMD)
	$(DMD) -c $(D_UDFLAGS) -I. -P=-I. $< -of$@

$(ROOT)/%$(DOTOBJ) : src/%.d $(DMD)
	$(DMD) -c $(D_UDFLAGS) $(SHAREDFLAGS) -I. -P=-I. $< -of$@

$(ROOT)/%_c$(DOTOBJ) : src/%.c
	$(CC) -c $(CFLAGS) -I. -o $@ $< 