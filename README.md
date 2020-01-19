# [SigScale](http://www.sigscale.org) SNMP Agent Simulator

SigScale SNMP Agent Simulator is used to benchmark SNMP managers
acting as alarm notification collectors
(e.g. [SigScale SNMP Collector](https://github.com/sigscale/snmp-collector)).
A traffic target in TPS (transactions per second) may be defined and
statistics are kept for each session.

The SNMP agent simulator implements the Alarm Management Information
Base (MIB) ([RFC3877](https://tools.ietf.org/html/rfc3877)). An alarm model
is created to define the alarms which may be sent to the manager. Alarms
are then added to the active alarm table which causes an SNMP notification
to be sent to the manager(s). Active alarms may be cleared which also causes
a notification.

