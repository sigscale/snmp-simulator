-module(snmp_simulator_import).

-export([file/1]).

-include("snmp_simulator.hrl").

-spec file(Filename) -> Result
	when
		Filename :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import alarm models.
file(Filename) when is_list(Filename) ->
	case file:consult(Filename) of
		{ok, Lines} ->
			import(Lines, 0);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
import([H | T], N) when is_tuple(H) ->
	{#alarmModelTable{key = GenKey1} = GenericModel1,
			#ituAlarmTable{key = ItuKey1} = ItuModel1} = parse_model(H),
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	{value, ItuAlarmEventType} = snmpa:name_to_oid(ituAlarmEventType),
	Index = N + 1,
	GenKey2 = setelement(2, GenKey1, Index),
	{ListName, Index, State} = GenKey2,
	GenRowIndex = [length(ListName)] ++ ListName ++ [Index, State],
	ItuKey2 = setelement(2, ItuKey1, Index),
	{ListName, Index, PerceivedSeverity} = ItuKey2,
	ItuRowIndex = [length(ListName)] ++ ListName ++ [Index, PerceivedSeverity],
	GenericModel2 = case GenericModel1#alarmModelTable.alarmModelSpecificPointer of
		[0, 0] ->
			GenericModel1#alarmModelTable{key = GenKey2,
					alarmModelSpecificPointer = ItuAlarmEventType ++ ItuRowIndex,
					alarmModelRowStatus = 1};
		_ ->
			GenericModel1#alarmModelTable{key = GenKey2,
					alarmModelRowStatus = 1}
	end,
	ItuModel2 = ItuModel1#ituAlarmTable{key = ItuKey2,
			ituAlarmGenericModel = AlarmModelNotificationId ++ GenRowIndex},
	F = fun() ->
				ok = mnesia:write(GenericModel2),
				ok = mnesia:write(ItuModel2)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			import(T, Index);
		{aborted, Reason} ->
			{error, Reason}
	end;
import([], _) ->
	ok.

%% @hidden
parse_model(Line) when is_tuple(Line) ->
	parse_notifification_id(Line, #alarmModelTable{}).

%% @hidden
parse_notifification_id(Line, Acc)
		when is_atom(element(1, Line)) ->
	{value, OID} = snmpa:name_to_oid(element(1, Line)),
	parse_varbind_index(Line,
			Acc#alarmModelTable{alarmModelNotificationId = OID});
parse_notifification_id(Line, Acc)
		when is_integer(hd(element(1, Line))) ->
	parse_varbind_index(Line,
			Acc#alarmModelTable{alarmModelNotificationId = element(1, Line)}).

%% @hidden
parse_varbind_index(Line, Acc)
		when is_integer(element(2, Line)),
		element(2, Line) > 0 ->
	parse_varbind_value(Line,
			Acc#alarmModelTable{alarmModelVarbindIndex = element(2, Line)});
parse_varbind_index(Line, Acc)
		when element(2, Line) == undefined ->
	parse_description(Line, Acc).

%% @hidden
parse_varbind_value(Line, Acc)
		when is_integer(element(3, Line)),
		element(3, Line) > 0 ->
	parse_description(Line,
			Acc#alarmModelTable{alarmModelVarbindValue = element(3, Line)}).

%% @hidden
parse_description(Line, Acc)
		when is_list(element(4, Line)) ->
	parse_specific_pointer(Line,
			Acc#alarmModelTable{alarmModelDescription = element(4, Line)});
parse_description(Line, Acc)
		when element(4, Line) == undefined ->
	parse_specific_pointer(Line, Acc).

%% @hidden
parse_specific_pointer(Line, Acc)
		when is_atom(hd(element(5, Line))) ->
	{value, Prefix} = snmpa:name_to_oid(hd(element(5, Line))),
	OID = Prefix ++ tl(element(5, Line)),
	parse_varbind_subtree(Line,
			Acc#alarmModelTable{alarmModelSpecificPointer = OID});
parse_specific_pointer(Line, Acc)
		when is_integer(hd(element(5, Line))) ->
	parse_varbind_subtree(Line,
			Acc#alarmModelTable{alarmModelSpecificPointer = element(5, Line)});
parse_specific_pointer(Line, Acc)
		when element(5, Line) == undefined ->
	parse_varbind_subtree(Line, Acc).

%% @hidden
parse_varbind_subtree(Line, Acc)
		when is_atom(element(6, Line)),
		element(6, Line) /= undefined ->
	{value, OID} = snmpa:name_to_oid(element(6, Line)),
	parse_resource_prefix(Line,
			Acc#alarmModelTable{alarmModelVarbindSubtree = OID});
parse_varbind_subtree(Line, Acc)
		when is_integer(hd(element(6, Line))) ->
	parse_resource_prefix(Line,
			Acc#alarmModelTable{alarmModelVarbindSubtree = element(6, Line)});
parse_varbind_subtree(Line, Acc)
		when element(6, Line) == undefined ->
	parse_resource_prefix(Line, Acc).

%% @hidden
parse_resource_prefix(Line, Acc)
		when is_atom(element(7, Line)),
		element(7, Line) /= undefined ->
	{value, OID} = snmpa:name_to_oid(element(7, Line)),
	parse_perceived_severity(Line,
			Acc#alarmModelTable{alarmModelResourcePrefix = OID});
parse_resource_prefix(Line, Acc)
		when is_integer(hd(element(7, Line))) ->
	parse_perceived_severity(Line,
			Acc#alarmModelTable{alarmModelResourcePrefix = element(7, Line)});
parse_resource_prefix(Line, Acc)
		when element(7, Line) == undefined ->
	parse_perceived_severity(Line, Acc).

%% @hidden
parse_perceived_severity(Line, Acc)
		when element(8, Line) == clear;
		element(8, Line) =:=  1 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 1}},
			#ituAlarmTable{key = {[], 0, 1}});
parse_perceived_severity(Line, Acc)
		when element(8, Line) == indeterminate;
		element(8, Line) =:=  2 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 2}},
			#ituAlarmTable{key = {[], 0, 2}});
parse_perceived_severity(Line, Acc)
		when element(8, Line) == warning;
		element(8, Line) =:=  6 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 3}},
			#ituAlarmTable{key = {[], 0, 6}});
parse_perceived_severity(Line, Acc)
		when element(8, Line) == minor;
		element(8, Line) =:=  5 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 4}},
			#ituAlarmTable{key = {[], 0, 5}});
parse_perceived_severity(Line, Acc)
		when element(8, Line) == major;
		element(8, Line) =:=  4 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 5}},
			#ituAlarmTable{key = {[], 0, 4}});
parse_perceived_severity(Line, Acc)
		when element(8, Line) == critical;
		element(8, Line) =:=  3 ->
	parse_event_type(Line, Acc#alarmModelTable{key = {[], 0, 6}},
			#ituAlarmTable{key = {[], 0, 3}}).

%% @hidden
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == other;
		element(9, Line) =:= 1 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 1});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == communicationsAlarm;
		element(9, Line) =:= 2 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 2});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == qualityOfServiceAlarm;
		element(9, Line) =:= 3 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 3});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == processingErrorAlarm;
		element(9, Line) =:= 4 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 4});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == equipmentAlarm;
		element(9, Line) =:= 5 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 5});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == environmentalAlarm;
		element(9, Line) =:= 6 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 6});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == integrityViolation;
		element(9, Line) =:= 7 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 7});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == operationalViolation;
		element(9, Line) =:= 8 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 8});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == physicalViolation;
		element(9, Line) =:= 9 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 9});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == securityServiceOrMechanismViolation;
		element(9, Line) =:= 10 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 10});
parse_event_type(Line, Acc1, Acc2)
		when element(9, Line) == timeDomainViolation;
		element(9, Line) =:= 11 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 11}).

%% @hidden
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == aIS;
		element(10, Line) =:= 1 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 1});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == callSetUpFailure ;
		element(10, Line) =:= 2 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 2});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == degradedSignal ;
		element(10, Line) =:= 3 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 3});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == farEndReceiverFailure ;
		element(10, Line) =:= 4 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 4});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == framingError ;
		element(10, Line) =:= 5 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 5});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfFrame;
		element(10, Line) =:= 6 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 6});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfPointer ;
		element(10, Line) =:= 7 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 7});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfSignal ;
		element(10, Line) =:= 8 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 8});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == payloadTypeMismatch;
		element(10, Line) =:= 9 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 9});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == transmissionError;
		element(10, Line) =:= 10 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 10});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == remoteAlarmInterface;
		element(10, Line) =:= 11 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 11});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == excessiveBER ;
		element(10, Line) =:= 12 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 12});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == pathTraceMismatch ;
		element(10, Line) =:= 13 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 13});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == unavailable ;
		element(10, Line) =:= 14 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 14});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == signalLabelMismatch;
		element(10, Line) =:= 15 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 15});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfMultiFrame;
		element(10, Line) =:= 16 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 16});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == receiveFailure;
		element(10, Line) =:= 17 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 17});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == transmitFailure;
		element(10, Line) =:= 18 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 18});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == modulationFailure;
		element(10, Line) =:= 19 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 19});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == demodulationFailure;
		element(10, Line) =:= 20 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 20});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == broadcastChannelFailure;
		element(10, Line) =:= 21 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 21});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == connectionEstablishmentError;
		element(10, Line) =:= 22 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 22});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == invalidMessageReceived;
		element(10, Line) =:= 23 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 23});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == localNodeTransmissionError;
		element(10, Line) =:= 24 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 24});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == remoteNodeTransmissionError;
		element(10, Line) =:= 25 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 25});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == routingFailure;
		element(10, Line) =:= 26 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 26});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == backplaneFailure;
		element(10, Line) =:= 51 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 51});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == dataSetProblem ;
		element(10, Line) =:= 52 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 52});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == equipmentIdentifierDuplication ;
		element(10, Line) =:= 53 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 53});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == externalIFDeviceProblem ;
		element(10, Line) =:= 54 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 54});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lineCardProblem;
		element(10, Line) =:= 55 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 55});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == multiplexerProblem ;
		element(10, Line) =:= 56 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 56});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == nEIdentifierDuplication ;
		element(10, Line) =:= 57 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 57});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == powerProblem ;
		element(10, Line) =:= 58 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 58});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == processorProblem ;
		element(10, Line) =:= 59 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 59});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == protectionPathFailure ;
		element(10, Line) =:= 60 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 60});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == receiverFailure ;
		element(10, Line) =:= 61 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 61});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == replaceableUnitMissing ;
		element(10, Line) =:= 62 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 62});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == replaceableUnitTypeMismatch;
		element(10, Line) =:= 63 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 63});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == synchronizationSourceMismatch ;
		element(10, Line) =:= 64 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 64});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == terminalProblem  ;
		element(10, Line) =:= 65 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 65});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == timingProblem  ;
		element(10, Line) =:= 66 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 66});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == transmitterFailure ;
		element(10, Line) =:= 67 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 67});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == trunkCardProblem ;
		element(10, Line) =:= 68 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 68});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == replaceableUnitProblem ;
		element(10, Line) =:= 69 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 69});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == realTimeClockFailure;
		element(10, Line) =:= 70 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 70});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == antennaFailure;
		element(10, Line) =:= 71 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 71});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == batteryChargingFailure;
		element(10, Line) =:= 72 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 72});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == diskFailure;
		element(10, Line) =:= 73 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 73});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == frequencyHoppingFailure;
		element(10, Line) =:= 74 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 74});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == iODeviceError;
		element(10, Line) =:= 75 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 75});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfSynchronisation;
		element(10, Line) =:= 76 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 76});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfRedundancy;
		element(10, Line) =:= 77 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 77});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == powerSupplyFailure;
		element(10, Line) =:= 78 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 78});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == signalQualityEvaluationFailure;
		element(10, Line) =:= 79 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 79});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == tranceiverFailure;
		element(10, Line) =:= 80 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 80});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == protectionMechanismFailure;
		element(10, Line) =:= 81 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 81});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == protectingResourceFailure;
		element(10, Line) =:= 82 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 82});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == airCompressorFailure ;
		element(10, Line) =:= 101 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 101});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == airConditioningFailure ;
		element(10, Line) =:= 102 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 102});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == airDryerFailure  ;
		element(10, Line) =:= 103 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 103});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == batteryDischarging ;
		element(10, Line) =:= 104 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 104});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == batteryFailure  ;
		element(10, Line) =:= 105 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 105});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == commercialPowerFailure ;
		element(10, Line) =:= 106 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 106});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == coolingFanFailure ;
		element(10, Line) =:= 107 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 107});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == engineFailure ;
		element(10, Line) =:= 108 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 108});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fireDetectorFailure ;
		element(10, Line) =:= 109 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 109});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fuseFailure ;
		element(10, Line) =:= 110 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 110});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == generatorFailure ;
		element(10, Line) =:= 111 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 111});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowBatteryThreshold;
		element(10, Line) =:= 112 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 112});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == pumpFailure ;
		element(10, Line) =:= 113 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 113});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == rectifierFailure ;
		element(10, Line) =:= 114 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 114});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == rectifierHighVoltage ;
		element(10, Line) =:= 115 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 115});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == rectifierLowFVoltage ;
		element(10, Line) =:= 116 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 116});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == ventilationsSystemFailure ;
		element(10, Line) =:= 117 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 117});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == enclosureDoorOpen ;
		element(10, Line) =:= 118 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 118});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == explosiveGas ;
		element(10, Line) =:= 119 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 119});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fire;
		element(10, Line) =:= 120 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 120});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == flood  ;
		element(10, Line) =:= 121 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 121});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == highHumidity ;
		element(10, Line) =:= 122 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 122});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == highTemperature ;
		element(10, Line) =:= 123 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 123});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == highWind ;
		element(10, Line) =:= 124 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 124});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == iceBuildUp ;
		element(10, Line) =:= 125 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 125});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == intrusionDetection ;
		element(10, Line) =:= 126 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 126});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowFuel ;
		element(10, Line) =:= 127 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 127});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowHumidity ;
		element(10, Line) =:= 128 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 128});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowCablePressure ;
		element(10, Line) =:= 129 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 129});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowTemperature ;
		element(10, Line) =:= 130 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 130});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lowWater ;
		element(10, Line) =:= 131 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 131});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == smoke ;
		element(10, Line) =:= 132 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 132});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == toxicGas ;
		element(10, Line) =:= 133 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 133});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == coolingSystemFailure;
		element(10, Line) =:= 134 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 134});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == externalEquipmentFailure;
		element(10, Line) =:= 135 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 135});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == externalPointFailure;
		element(10, Line) =:= 136 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 136});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == storageCapacityProblem;
		element(10, Line) =:= 151 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 151});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == memoryMismatch ;
		element(10, Line) =:= 152 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 152});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == corruptData ;
		element(10, Line) =:= 153 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 153});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == outOfCPUCycles  ;
		element(10, Line) =:= 154 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 154});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == sfwrEnvironmentProblem ;
		element(10, Line) =:= 155 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 155});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == sfwrDownloadFailure ;
		element(10, Line) =:= 156 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 156});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfRealTime;
		element(10, Line) =:= 157 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 157});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == reinitialized;
		element(10, Line) =:= 158 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 158});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == applicationSubsystemFailure;
		element(10, Line) =:= 159 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 159});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == configurationOrCustomisationError;
		element(10, Line) =:= 160 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 160});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == databaseInconsistency;
		element(10, Line) =:= 161 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 161});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fileError;
		element(10, Line) =:= 162 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 162});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == outOfMemory;
		element(10, Line) =:= 163 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 163});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == softwareError;
		element(10, Line) =:= 164 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 164});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == timeoutExpired;
		element(10, Line) =:= 165 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 165});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == underlayingResourceUnavailable;
		element(10, Line) =:= 166 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 166});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == versionMismatch;
		element(10, Line) =:= 167 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 167});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == bandwidthReduced;
		element(10, Line) =:= 201 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 201});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == congestion;
		element(10, Line) =:= 202 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 202});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == excessiveErrorRate;
		element(10, Line) =:= 203 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 203});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == excessiveResponseTime;
		element(10, Line) =:= 204 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 204});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == excessiveRetransmissionRate;
		element(10, Line) =:= 205 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 205});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == reducedLoggingCapability;
		element(10, Line) =:= 206 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 206});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == systemResourcesOverload;
		element(10, Line) =:= 207 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 207});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == adapterError;
		element(10, Line) =:= 500 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 500});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == applicationSubsystemFailture;
		element(10, Line) =:= 501 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 501});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == bandwidthReducedX733;
		element(10, Line) =:= 502 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 502});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == callEstablishmentError;
		element(10, Line) =:= 503 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 503});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == communicationsProtocolError;
		element(10, Line) =:= 504 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 504});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == communicationsSubsystemFailure;
		element(10, Line) =:= 505 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 505});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == configurationOrCustomizationError;
		element(10, Line) =:= 506 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 506});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == congestionX733;
		element(10, Line) =:= 507 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 507});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == coruptData;
		element(10, Line) =:= 508 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 508});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == cpuCyclesLimitExceeded;
		element(10, Line) =:= 509 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 509});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == dataSetOrModemError;
		element(10, Line) =:= 510 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 510});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == degradedSignalX733;
		element(10, Line) =:= 511 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 511});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == dteDceInterfaceError;
		element(10, Line) =:= 512 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 512});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == enclosureDoorOpenX733;
		element(10, Line) =:= 513 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 513});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == equipmentMalfunction;
		element(10, Line) =:= 514 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 514});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == excessiveVibration;
		element(10, Line) =:= 515 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 515});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fileErrorX733;
		element(10, Line) =:= 516 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 516});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == fireDetected;
		element(10, Line) =:= 517 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 517});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == framingErrorX733;
		element(10, Line) =:= 518 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 518});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == heatingVentCoolingSystemProblem;
		element(10, Line) =:= 519 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 519});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == humidityUnacceptable;
		element(10, Line) =:= 520 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 520});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == inputOutputDeviceError;
		element(10, Line) =:= 521 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 521});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == inputDeviceError;
		element(10, Line) =:= 522 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 522});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lanError;
		element(10, Line) =:= 523 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 523});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == leakDetected;
		element(10, Line) =:= 524 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 524});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == localNodeTransmissionErrorX733;
		element(10, Line) =:= 525 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 525});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfFrameX733;
		element(10, Line) =:= 526 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 526});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == lossOfSignalX733;
		element(10, Line) =:= 527 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 527});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == materialSupplyExhausted;
		element(10, Line) =:= 528 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 528});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == multiplexerProblemX733;
		element(10, Line) =:= 529 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 529});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == outOfMemoryX733;
		element(10, Line) =:= 530 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 530});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == ouputDeviceError;
		element(10, Line) =:= 531 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 531});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == performanceDegraded;
		element(10, Line) =:= 532 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 532});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == powerProblems;
		element(10, Line) =:= 533 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 533});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == pressureUnacceptable;
		element(10, Line) =:= 534 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 534});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == processorProblems;
		element(10, Line) =:= 535 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 535});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == pumpFailureX733;
		element(10, Line) =:= 536 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 536});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == queueSizeExceeded;
		element(10, Line) =:= 537 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 537});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == receiveFailureX733;
		element(10, Line) =:= 538 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 538});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == receiverFailureX733;
		element(10, Line) =:= 539 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 539});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == remoteNodeTransmissionErrorX733;
		element(10, Line) =:= 540 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 540});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == resourceAtOrNearingCapacity;
		element(10, Line) =:= 541 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 541});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == responseTimeExecessive;
		element(10, Line) =:= 542 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 542});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == retransmissionRateExcessive;
		element(10, Line) =:= 543 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 543});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == softwareErrorX733;
		element(10, Line) =:= 544 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 544});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == softwareProgramAbnormallyTerminated;
		element(10, Line) =:= 545 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 545});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == softwareProgramError;
		element(10, Line) =:= 546 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 546});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == storageCapacityProblemX733;
		element(10, Line) =:= 547 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 547});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == temperatureUnacceptable;
		element(10, Line) =:= 548 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 548});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == thresholdCrossed;
		element(10, Line) =:= 549 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 549});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == timingProblemX733;
		element(10, Line) =:= 550 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 550});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == toxicLeakDetected;
		element(10, Line) =:= 551 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 551});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == transmitFailureX733;
		element(10, Line) =:= 552 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 552});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == transmiterFailure;
		element(10, Line) =:= 553 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 553});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == underlyingResourceUnavailable;
		element(10, Line) =:= 554 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 554});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == versionMismatchX733;
		element(10, Line) =:= 555 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 555});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == authenticationFailure;
		element(10, Line) =:= 600 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 600});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == breachOfConfidentiality;
		element(10, Line) =:= 601 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 601});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == cableTamper;
		element(10, Line) =:= 602 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 602});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == delayedInformation;
		element(10, Line) =:= 603 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 603});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == denialOfService;
		element(10, Line) =:= 604 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 604});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == duplicateInformation;
		element(10, Line) =:= 605 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 605});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == informationMissing;
		element(10, Line) =:= 606 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 606});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == informationModificationDetected;
		element(10, Line) =:= 607 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 607});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == informationOutOfSequence;
		element(10, Line) =:= 608 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 608});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == keyExpired;
		element(10, Line) =:= 609 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 609});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == nonRepudiationFailure;
		element(10, Line) =:= 610 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 610});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == outOfHoursActivity;
		element(10, Line) =:= 611 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 611});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == outOfService;
		element(10, Line) =:= 612 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 612});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == proceduralError;
		element(10, Line) =:= 613 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 613});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == unauthorizedAccessAttempt;
		element(10, Line) =:= 614 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 614});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == unexpectedInformation;
		element(10, Line) =:= 615 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 615});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == other;
		element(10, Line) =:= 1024 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 1024});
parse_probable_cause(Line, Acc1, Acc2)
		when element(10, Line) == undefined ->
	parse_additional_text(Line, Acc1, Acc2).

%% @hidden
parse_additional_text(Line, Acc1, Acc2)
		when is_list(element(11, Line)) ->
	{Acc1, Acc2#ituAlarmTable{ituAlarmAdditionalText = element(11, Line)}};
parse_additional_text(Line, Acc1, Acc2)
		when element(11, Line) == undefined ->
	{Acc1, Acc2}.

