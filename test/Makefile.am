## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

testmods = snmp_simulator_test_lib \
		snmp_simulator_snmpm_cb \
		snmp_simulator_api_SUITE \
		snmp_simulator_snmp_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/snmp_simulator.hrl
	$(ERLC) -pa $(top_builddir)/ebin $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -noshell -sname $(LOGNAME)-ct -config sys

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e 's#%APP_PRIV%#$(abs_top_srcdir)/priv#' \
			-e 's#%SERVER_ROOT%#$(abs_top_builddir)/test#' < $< > $@

clean-local:
	-rm -r *.beam log/*

