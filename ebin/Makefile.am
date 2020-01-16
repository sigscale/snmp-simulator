## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = snmp_simulator.app
EXTRA_DIST = snmp_simulator.app

beam_DATA = snmp_simulator_app.beam \
		snmp_simulator_sup.beam \
		snmp_simulator_server.beam \
		snmp_simulator.beam \
		snmp_simulator_mib.beam \
		snmp_simulator_import.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/snmp_simulator.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

