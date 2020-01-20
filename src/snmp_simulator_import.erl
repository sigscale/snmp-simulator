-module(snmp_simulator_import).

-export([file/1]).

-include("snmp_simulator.hrl").

-spec file(Filename) -> Result
	when
		Filename :: file:filename(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Import alarm model.
file(Filename) when is_list(Filename) ->
	case file:consult(Filename) of
		{ok, Lines} ->
			import(Lines);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
import([H | T]) when is_tuple(H) ->
	{#alarmModelTable{key = {ListName, Index, State}} = GenericModel1,
			#ituAlarmTable{key = {ListName, Index, Severity}} = ItuModel1} = parse_model(H),
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	{value, ItuAlarmEventType} = snmpa:name_to_oid(ituAlarmEventType),
	GenRowIndex = [length(ListName)] ++ ListName ++ [Index, State],
	ItuRowIndex = [length(ListName)] ++ ListName ++ [Index, Severity],
	GenericModel2 = case GenericModel1#alarmModelTable.alarmModelSpecificPointer of
		[0, 0] ->
			Pointer = ItuAlarmEventType ++ ItuRowIndex,
			GenericModel1#alarmModelTable{alarmModelSpecificPointer = Pointer,
					alarmModelRowStatus = 1};
		_ ->
			GenericModel1#alarmModelTable{alarmModelRowStatus = 1}
	end,
	GenericModelPointer = AlarmModelNotificationId ++ GenRowIndex,
	ItuModel2 = ItuModel1#ituAlarmTable{ituAlarmGenericModel = GenericModelPointer},
	F = fun() ->
				mnesia:write(GenericModel2),
				mnesia:write(ItuModel2),
				mnesia:write({snmp_variables, alarmModelIndex, Index})
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			import(T);
		{aborted, Reason} ->
			{error, Reason}
	end;
import([]) ->
	ok.

%% @hidden
parse_model(Line)
		when is_tuple(Line), is_integer(element(1, Line)),
		element(1, Line) > 0 ->
	parse_notifification_id(Line,
			#alarmModelTable{key = {[], element(1, Line), 0}}).

%% @hidden
parse_notifification_id(Line, Acc)
		when is_atom(element(2, Line)) ->
	{value, OID} = snmpa:name_to_oid(element(2, Line)),
	parse_varbind_index(Line,
			Acc#alarmModelTable{alarmModelNotificationId = OID});
parse_notifification_id(Line, Acc)
		when is_integer(hd(element(2, Line))) ->
	parse_varbind_index(Line,
			Acc#alarmModelTable{alarmModelNotificationId = element(2, Line)}).

%% @hidden
parse_varbind_index(Line, Acc)
		when is_integer(element(3, Line)),
		element(3, Line) > 0 ->
	parse_varbind_value(Line,
			Acc#alarmModelTable{alarmModelVarbindIndex = element(3, Line)});
parse_varbind_index(Line, Acc)
		when element(3, Line) == undefined ->
	parse_description(Line, Acc).

%% @hidden
parse_varbind_value(Line, Acc)
		when is_integer(element(4, Line)),
		element(4, Line) > 0 ->
	parse_description(Line,
			Acc#alarmModelTable{alarmModelVarbindValue = element(4, Line)}).

%% @hidden
parse_description(Line, Acc)
		when is_list(element(5, Line)) ->
	parse_specific_pointer(Line,
			Acc#alarmModelTable{alarmModelDescription = element(5, Line)});
parse_description(Line, Acc)
		when element(5, Line) == undefined ->
	parse_specific_pointer(Line, Acc).

%% @hidden
parse_specific_pointer(Line, Acc)
		when is_atom(hd(element(6, Line))) ->
	{value, Prefix} = snmpa:name_to_oid(hd(element(6, Line))),
	OID = Prefix ++ tl(element(6, Line)),
	parse_varbind_subtree(Line,
			Acc#alarmModelTable{alarmModelSpecificPointer = OID});
parse_specific_pointer(Line, Acc)
		when is_integer(hd(element(6, Line))) ->
	parse_varbind_subtree(Line,
			Acc#alarmModelTable{alarmModelSpecificPointer = element(6, Line)});
parse_specific_pointer(Line, Acc)
		when element(6, Line) == undefined ->
	parse_varbind_subtree(Line, Acc).

%% @hidden
parse_varbind_subtree(Line, Acc)
		when is_atom(element(7, Line)),
		element(7, Line) /= undefined ->
	{value, OID} = snmpa:name_to_oid(element(7, Line)),
	parse_resource_prefix(Line,
			Acc#alarmModelTable{alarmModelVarbindSubtree = OID});
parse_varbind_subtree(Line, Acc)
		when is_integer(hd(element(7, Line))) ->
	parse_resource_prefix(Line,
			Acc#alarmModelTable{alarmModelVarbindSubtree = element(7, Line)});
parse_varbind_subtree(Line, Acc)
		when element(7, Line) == undefined ->
	parse_resource_prefix(Line, Acc).

%% @hidden
parse_resource_prefix(Line, Acc)
		when is_atom(element(8, Line)),
		element(8, Line) /= undefined ->
	{value, OID} = snmpa:name_to_oid(element(8, Line)),
	parse_perceived_severity(Line,
			Acc#alarmModelTable{alarmModelResourcePrefix = OID});
parse_resource_prefix(Line, Acc)
		when is_integer(hd(element(8, Line))) ->
	parse_perceived_severity(Line,
			Acc#alarmModelTable{alarmModelResourcePrefix = element(8, Line)});
parse_resource_prefix(Line, Acc)
		when element(8, Line) == undefined ->
	parse_perceived_severity(Line, Acc).

%% @hidden
parse_perceived_severity(Line,
		#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == clear;
		element(9, Line) =:= 1 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 1}},
			#ituAlarmTable{key = {ListName, Index, 1}});
parse_perceived_severity(Line,
		#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == indeterminate;
		element(9, Line) =:= 2 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 2}},
			#ituAlarmTable{key = {ListName, Index, 2}});
parse_perceived_severity(Line,
			#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == critical;
		element(9, Line) =:= 3 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 6}},
			#ituAlarmTable{key = {ListName, Index, 3}});
parse_perceived_severity(Line,
			#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == major;
		element(9, Line) =:= 4 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 5}},
			#ituAlarmTable{key = {ListName, Index, 4}});
parse_perceived_severity(Line,
			#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == minor;
		element(9, Line) =:= 5 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 4}},
			#ituAlarmTable{key = {ListName, Index, 5}});
parse_perceived_severity(Line,
			#alarmModelTable{key = {ListName, Index, _}} = Acc)
		when element(9, Line) == warning;
		element(9, Line) =:= 6 ->
	parse_event_type(Line,
			Acc#alarmModelTable{key = {ListName, Index, 3}},
			#ituAlarmTable{key = {ListName, Index, 6}}).

%% @hidden
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == other;
		element(10, Line) =:= 1 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 1});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == communicationsAlarm;
		element(10, Line) =:= 2 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 2});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == qualityOfServiceAlarm;
		element(10, Line) =:= 3 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 3});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == processingErrorAlarm;
		element(10, Line) =:= 4 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 4});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == equipmentAlarm;
		element(10, Line) =:= 5 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 5});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == environmentalAlarm;
		element(10, Line) =:= 6 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 6});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == integrityViolation;
		element(10, Line) =:= 7 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 7});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == operationalViolation;
		element(10, Line) =:= 8 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 8});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == physicalViolation;
		element(10, Line) =:= 9 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 9});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == securityServiceOrMechanismViolation;
		element(10, Line) =:= 10 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 10});
parse_event_type(Line, Acc1, Acc2)
		when element(10, Line) == timeDomainViolation;
		element(10, Line) =:= 11 ->
	parse_probable_cause(Line, Acc1,
			Acc2#ituAlarmTable{ituAlarmEventType = 11}).

%% @hidden
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == aIS;
		element(11, Line) =:= 1 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 1});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == callSetUpFailure ;
		element(11, Line) =:= 2 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 2});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == degradedSignal ;
		element(11, Line) =:= 3 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 3});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == farEndReceiverFailure ;
		element(11, Line) =:= 4 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 4});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == framingError ;
		element(11, Line) =:= 5 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 5});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfFrame;
		element(11, Line) =:= 6 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 6});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfPointer ;
		element(11, Line) =:= 7 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 7});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfSignal ;
		element(11, Line) =:= 8 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 8});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == payloadTypeMismatch;
		element(11, Line) =:= 9 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 9});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == transmissionError;
		element(11, Line) =:= 10 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 10});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == remoteAlarmInterface;
		element(11, Line) =:= 11 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 11});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == excessiveBER ;
		element(11, Line) =:= 12 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 12});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == pathTraceMismatch ;
		element(11, Line) =:= 13 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 13});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == unavailable ;
		element(11, Line) =:= 14 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 14});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == signalLabelMismatch;
		element(11, Line) =:= 15 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 15});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfMultiFrame;
		element(11, Line) =:= 16 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 16});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == receiveFailure;
		element(11, Line) =:= 17 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 17});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == transmitFailure;
		element(11, Line) =:= 18 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 18});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == modulationFailure;
		element(11, Line) =:= 19 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 19});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == demodulationFailure;
		element(11, Line) =:= 20 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 20});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == broadcastChannelFailure;
		element(11, Line) =:= 21 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 21});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == connectionEstablishmentError;
		element(11, Line) =:= 22 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 22});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == invalidMessageReceived;
		element(11, Line) =:= 23 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 23});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == localNodeTransmissionError;
		element(11, Line) =:= 24 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 24});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == remoteNodeTransmissionError;
		element(11, Line) =:= 25 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 25});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == routingFailure;
		element(11, Line) =:= 26 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 26});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == backplaneFailure;
		element(11, Line) =:= 51 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 51});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == dataSetProblem ;
		element(11, Line) =:= 52 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 52});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == equipmentIdentifierDuplication ;
		element(11, Line) =:= 53 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 53});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == externalIFDeviceProblem ;
		element(11, Line) =:= 54 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 54});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lineCardProblem;
		element(11, Line) =:= 55 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 55});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == multiplexerProblem ;
		element(11, Line) =:= 56 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 56});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == nEIdentifierDuplication ;
		element(11, Line) =:= 57 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 57});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == powerProblem ;
		element(11, Line) =:= 58 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 58});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == processorProblem ;
		element(11, Line) =:= 59 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 59});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == protectionPathFailure ;
		element(11, Line) =:= 60 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 60});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == receiverFailure ;
		element(11, Line) =:= 61 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 61});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == replaceableUnitMissing ;
		element(11, Line) =:= 62 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 62});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == replaceableUnitTypeMismatch;
		element(11, Line) =:= 63 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 63});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == synchronizationSourceMismatch ;
		element(11, Line) =:= 64 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 64});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == terminalProblem  ;
		element(11, Line) =:= 65 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 65});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == timingProblem  ;
		element(11, Line) =:= 66 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 66});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == transmitterFailure ;
		element(11, Line) =:= 67 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 67});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == trunkCardProblem ;
		element(11, Line) =:= 68 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 68});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == replaceableUnitProblem ;
		element(11, Line) =:= 69 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 69});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == realTimeClockFailure;
		element(11, Line) =:= 70 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 70});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == antennaFailure;
		element(11, Line) =:= 71 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 71});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == batteryChargingFailure;
		element(11, Line) =:= 72 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 72});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == diskFailure;
		element(11, Line) =:= 73 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 73});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == frequencyHoppingFailure;
		element(11, Line) =:= 74 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 74});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == iODeviceError;
		element(11, Line) =:= 75 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 75});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfSynchronisation;
		element(11, Line) =:= 76 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 76});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfRedundancy;
		element(11, Line) =:= 77 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 77});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == powerSupplyFailure;
		element(11, Line) =:= 78 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 78});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == signalQualityEvaluationFailure;
		element(11, Line) =:= 79 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 79});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == tranceiverFailure;
		element(11, Line) =:= 80 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 80});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == protectionMechanismFailure;
		element(11, Line) =:= 81 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 81});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == protectingResourceFailure;
		element(11, Line) =:= 82 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 82});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == airCompressorFailure ;
		element(11, Line) =:= 101 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 101});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == airConditioningFailure ;
		element(11, Line) =:= 102 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 102});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == airDryerFailure  ;
		element(11, Line) =:= 103 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 103});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == batteryDischarging ;
		element(11, Line) =:= 104 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 104});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == batteryFailure  ;
		element(11, Line) =:= 105 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 105});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == commercialPowerFailure ;
		element(11, Line) =:= 106 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 106});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == coolingFanFailure ;
		element(11, Line) =:= 107 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 107});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == engineFailure ;
		element(11, Line) =:= 108 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 108});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fireDetectorFailure ;
		element(11, Line) =:= 109 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 109});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fuseFailure ;
		element(11, Line) =:= 110 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 110});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == generatorFailure ;
		element(11, Line) =:= 111 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 111});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowBatteryThreshold;
		element(11, Line) =:= 112 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 112});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == pumpFailure ;
		element(11, Line) =:= 113 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 113});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == rectifierFailure ;
		element(11, Line) =:= 114 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 114});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == rectifierHighVoltage ;
		element(11, Line) =:= 115 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 115});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == rectifierLowFVoltage ;
		element(11, Line) =:= 116 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 116});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == ventilationsSystemFailure ;
		element(11, Line) =:= 117 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 117});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == enclosureDoorOpen ;
		element(11, Line) =:= 118 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 118});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == explosiveGas ;
		element(11, Line) =:= 119 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 119});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fire;
		element(11, Line) =:= 120 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 120});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == flood  ;
		element(11, Line) =:= 121 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 121});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == highHumidity ;
		element(11, Line) =:= 122 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 122});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == highTemperature ;
		element(11, Line) =:= 123 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 123});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == highWind ;
		element(11, Line) =:= 124 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 124});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == iceBuildUp ;
		element(11, Line) =:= 125 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 125});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == intrusionDetection ;
		element(11, Line) =:= 126 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 126});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowFuel ;
		element(11, Line) =:= 127 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 127});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowHumidity ;
		element(11, Line) =:= 128 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 128});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowCablePressure ;
		element(11, Line) =:= 129 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 129});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowTemperature ;
		element(11, Line) =:= 130 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 130});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lowWater ;
		element(11, Line) =:= 131 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 131});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == smoke ;
		element(11, Line) =:= 132 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 132});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == toxicGas ;
		element(11, Line) =:= 133 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 133});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == coolingSystemFailure;
		element(11, Line) =:= 134 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 134});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == externalEquipmentFailure;
		element(11, Line) =:= 135 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 135});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == externalPointFailure;
		element(11, Line) =:= 136 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 136});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == storageCapacityProblem;
		element(11, Line) =:= 151 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 151});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == memoryMismatch ;
		element(11, Line) =:= 152 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 152});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == corruptData ;
		element(11, Line) =:= 153 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 153});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == outOfCPUCycles  ;
		element(11, Line) =:= 154 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 154});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == sfwrEnvironmentProblem ;
		element(11, Line) =:= 155 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 155});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == sfwrDownloadFailure ;
		element(11, Line) =:= 156 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 156});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfRealTime;
		element(11, Line) =:= 157 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 157});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == reinitialized;
		element(11, Line) =:= 158 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 158});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == applicationSubsystemFailure;
		element(11, Line) =:= 159 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 159});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == configurationOrCustomisationError;
		element(11, Line) =:= 160 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 160});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == databaseInconsistency;
		element(11, Line) =:= 161 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 161});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fileError;
		element(11, Line) =:= 162 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 162});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == outOfMemory;
		element(11, Line) =:= 163 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 163});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == softwareError;
		element(11, Line) =:= 164 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 164});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == timeoutExpired;
		element(11, Line) =:= 165 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 165});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == underlayingResourceUnavailable;
		element(11, Line) =:= 166 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 166});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == versionMismatch;
		element(11, Line) =:= 167 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 167});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == bandwidthReduced;
		element(11, Line) =:= 201 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 201});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == congestion;
		element(11, Line) =:= 202 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 202});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == excessiveErrorRate;
		element(11, Line) =:= 203 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 203});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == excessiveResponseTime;
		element(11, Line) =:= 204 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 204});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == excessiveRetransmissionRate;
		element(11, Line) =:= 205 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 205});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == reducedLoggingCapability;
		element(11, Line) =:= 206 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 206});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == systemResourcesOverload;
		element(11, Line) =:= 207 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 207});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == adapterError;
		element(11, Line) =:= 500 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 500});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == applicationSubsystemFailture;
		element(11, Line) =:= 501 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 501});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == bandwidthReducedX733;
		element(11, Line) =:= 502 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 502});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == callEstablishmentError;
		element(11, Line) =:= 503 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 503});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == communicationsProtocolError;
		element(11, Line) =:= 504 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 504});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == communicationsSubsystemFailure;
		element(11, Line) =:= 505 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 505});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == configurationOrCustomizationError;
		element(11, Line) =:= 506 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 506});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == congestionX733;
		element(11, Line) =:= 507 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 507});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == coruptData;
		element(11, Line) =:= 508 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 508});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == cpuCyclesLimitExceeded;
		element(11, Line) =:= 509 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 509});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == dataSetOrModemError;
		element(11, Line) =:= 510 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 510});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == degradedSignalX733;
		element(11, Line) =:= 511 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 511});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == dteDceInterfaceError;
		element(11, Line) =:= 512 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 512});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == enclosureDoorOpenX733;
		element(11, Line) =:= 513 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 513});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == equipmentMalfunction;
		element(11, Line) =:= 514 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 514});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == excessiveVibration;
		element(11, Line) =:= 515 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 515});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fileErrorX733;
		element(11, Line) =:= 516 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 516});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == fireDetected;
		element(11, Line) =:= 517 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 517});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == framingErrorX733;
		element(11, Line) =:= 518 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 518});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == heatingVentCoolingSystemProblem;
		element(11, Line) =:= 519 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 519});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == humidityUnacceptable;
		element(11, Line) =:= 520 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 520});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == inputOutputDeviceError;
		element(11, Line) =:= 521 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 521});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == inputDeviceError;
		element(11, Line) =:= 522 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 522});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lanError;
		element(11, Line) =:= 523 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 523});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == leakDetected;
		element(11, Line) =:= 524 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 524});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == localNodeTransmissionErrorX733;
		element(11, Line) =:= 525 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 525});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfFrameX733;
		element(11, Line) =:= 526 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 526});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == lossOfSignalX733;
		element(11, Line) =:= 527 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 527});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == materialSupplyExhausted;
		element(11, Line) =:= 528 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 528});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == multiplexerProblemX733;
		element(11, Line) =:= 529 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 529});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == outOfMemoryX733;
		element(11, Line) =:= 530 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 530});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == ouputDeviceError;
		element(11, Line) =:= 531 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 531});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == performanceDegraded;
		element(11, Line) =:= 532 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 532});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == powerProblems;
		element(11, Line) =:= 533 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 533});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == pressureUnacceptable;
		element(11, Line) =:= 534 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 534});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == processorProblems;
		element(11, Line) =:= 535 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 535});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == pumpFailureX733;
		element(11, Line) =:= 536 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 536});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == queueSizeExceeded;
		element(11, Line) =:= 537 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 537});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == receiveFailureX733;
		element(11, Line) =:= 538 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 538});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == receiverFailureX733;
		element(11, Line) =:= 539 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 539});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == remoteNodeTransmissionErrorX733;
		element(11, Line) =:= 540 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 540});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == resourceAtOrNearingCapacity;
		element(11, Line) =:= 541 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 541});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == responseTimeExecessive;
		element(11, Line) =:= 542 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 542});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == retransmissionRateExcessive;
		element(11, Line) =:= 543 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 543});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == softwareErrorX733;
		element(11, Line) =:= 544 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 544});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == softwareProgramAbnormallyTerminated;
		element(11, Line) =:= 545 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 545});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == softwareProgramError;
		element(11, Line) =:= 546 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 546});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == storageCapacityProblemX733;
		element(11, Line) =:= 547 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 547});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == temperatureUnacceptable;
		element(11, Line) =:= 548 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 548});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == thresholdCrossed;
		element(11, Line) =:= 549 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 549});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == timingProblemX733;
		element(11, Line) =:= 550 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 550});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == toxicLeakDetected;
		element(11, Line) =:= 551 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 551});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == transmitFailureX733;
		element(11, Line) =:= 552 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 552});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == transmiterFailure;
		element(11, Line) =:= 553 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 553});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == underlyingResourceUnavailable;
		element(11, Line) =:= 554 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 554});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == versionMismatchX733;
		element(11, Line) =:= 555 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 555});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == authenticationFailure;
		element(11, Line) =:= 600 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 600});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == breachOfConfidentiality;
		element(11, Line) =:= 601 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 601});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == cableTamper;
		element(11, Line) =:= 602 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 602});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == delayedInformation;
		element(11, Line) =:= 603 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 603});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == denialOfService;
		element(11, Line) =:= 604 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 604});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == duplicateInformation;
		element(11, Line) =:= 605 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 605});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == informationMissing;
		element(11, Line) =:= 606 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 606});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == informationModificationDetected;
		element(11, Line) =:= 607 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 607});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == informationOutOfSequence;
		element(11, Line) =:= 608 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 608});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == keyExpired;
		element(11, Line) =:= 609 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 609});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == nonRepudiationFailure;
		element(11, Line) =:= 610 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 610});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == outOfHoursActivity;
		element(11, Line) =:= 611 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 611});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == outOfService;
		element(11, Line) =:= 612 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 612});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == proceduralError;
		element(11, Line) =:= 613 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 613});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == unauthorizedAccessAttempt;
		element(11, Line) =:= 614 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 614});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == unexpectedInformation;
		element(11, Line) =:= 615 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 615});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == other;
		element(11, Line) =:= 1024 ->
	parse_additional_text(Line, Acc1, Acc2#ituAlarmTable{ituAlarmProbableCause = 1024});
parse_probable_cause(Line, Acc1, Acc2)
		when element(11, Line) == undefined ->
	parse_additional_text(Line, Acc1, Acc2).

%% @hidden
parse_additional_text(Line, Acc1, Acc2)
		when is_list(element(12, Line)) ->
	{Acc1, Acc2#ituAlarmTable{ituAlarmAdditionalText = element(12, Line)}};
parse_additional_text(Line, Acc1, Acc2)
		when element(12, Line) == undefined ->
	{Acc1, Acc2}.

