## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

mibs_DATA = ALARM-MIB.mib \
		IANA-ITU-ALARM-TC-MIB.mib \
		ITU-ALARM-MIB.mib \
		ITU-ALARM-TC-MIB.mib

check_DATA = .tree_ok

.tree_ok:
	@if snmptranslate -V > /dev/null 2>&1; then \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +ALARM-MIB:IANA-ITU-ALARM-TC-MIB:ITU-ALARM-MIB:ITU-ALARM-TC-MIB \
			-Lo -Tp -IR sigscale; \
	fi

