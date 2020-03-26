## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

mibs_DATA = ITU-ALARM-TC-MIB.mib \
		IANA-ITU-ALARM-TC-MIB.mib \
		IANAifType-MIB.mib \
		IF-MIB.mib \
		ENTITY-MIB.mib \
		RMON-MIB.mib \
		RMON2-MIB.mib \
		ALARM-MIB.mib \
		ITU-ALARM-MIB.mib

check_DATA = .tree_ok

.tree_ok:
	@if snmptranslate -V > /dev/null 2>&1; then \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +ITU-ALARM-TC-MIB -Lo -Tp -IR ituAlarmTc; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +IANA-ITU-ALARM-TC-MIB -Lo -Tp -IR ianaItuAlarmNumbers; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +IANAifType-MIB  -Lo -Tp -IR ianaifType; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +IF-MIB -Lo -Tp -IR ifMIB; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +ENTITY-MIB -Lo -Tp -IR ifMIB; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +RMON-MIB -Lo -Tp -IR rmonMibModule; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m RMON2-MIB -Lo -Tp -IR rmon; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m ALARM-MIB -Lo -Tp -IR alarmMIB; \
		snmptranslate -M +$(top_srcdir)/mibs \
			-m +ITU-ALARM-MIB -Lo -Tp -IR ituAlarmMIB; \
	fi

