## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

emib_DATA =  

%.bin:$(top_srcdir)/mibs/%.mib
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

CLEANFILES = $(emib_DATA)

