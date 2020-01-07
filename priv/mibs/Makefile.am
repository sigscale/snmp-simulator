## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

emib_DATA =  ALARM-MIB.bin \
		IANA-ITU-ALARM-TC-MIB.bin \
		ITU-ALARM-MIB.bin \
		ITU-ALARM-TC-MIB.bin

%.bin:$(top_srcdir)/mibs/%.mib
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

CLEANFILES = $(emib_DATA)

