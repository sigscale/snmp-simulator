## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

ERLCFLAGS +=  +'{group_check, false}' +'{db, mnesia}'

emib_DATA = ITU-ALARM-TC-MIB.bin \
		IANA-ITU-ALARM-TC-MIB.bin \
		IANAifType-MIB.bin \
		IF-MIB.bin \
		ENTITY-MIB.bin \
		RMON-MIB.bin \
		RMON2-MIB.bin \
		ALARM-MIB.bin \
		ITU-ALARM-MIB.bin

ALARM-MIB.bin:$(top_srcdir)/mibs/ALARM-MIB.mib $(top_srcdir)/mibs/ALARM-MIB.funcs
	$(ERLC) $(ERLCFLAGS) -I $(ERLANG_LIB_DIR_sigscale_mibs)/priv/mibs -I $(ERLANG_LIB_DIR_snmp)/priv/mib $<

%.bin:$(top_srcdir)/mibs/%.mib
	$(ERLC) $(ERLCFLAGS) -I $(ERLANG_LIB_DIR_sigscale_mibs)/priv/mibs -I $(ERLANG_LIB_DIR_snmp)/priv/mib $<

CLEANFILES = $(emib_DATA)

