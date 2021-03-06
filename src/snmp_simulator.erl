%%% snmp_simulator.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2018-2020 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the public API for the
%%%   {@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% export the snmp_simulator  public API
-export([add_alarm/2, add_alarm/3, clear_alarm/1]).
-export([add_worker/1]).
-export([datetime/0, iso8601/1]).

-include("snmp_simulator.hrl").

-define(EPOCH, 62167219200).

%%----------------------------------------------------------------------
%%  The snmp_simulator public API
%%----------------------------------------------------------------------

-spec add_alarm(Model, Resource) -> Result
	when
		Model :: {ListName, Index, State},
		ListName :: string(),
		Index :: pos_integer(),
		State :: 1..6,
		Resource :: snmpa:oid(),
		Result :: {ok, AlarmIndex} | {error, Reason},
		AlarmIndex :: {ListName, DateAndTime, ActiveIndex},
		ActiveIndex :: pos_integer(),
		DateAndTime :: string(),
		Reason :: term().
%% @equiv add_alarm(Model, Resource, [])
add_alarm(Model, Resource) ->
	add_alarm(Model, Resource, []).

-spec add_alarm(Model, Resource, Varbinds) -> Result
	when
		Model :: {ListName, Index, State} | RowIndex,
		ListName :: string(),
		Index :: pos_integer(),
		State :: 1..6,
		RowIndex :: [byte()],
		Resource :: snmpa:oid(),
		Varbinds :: [Varbind],
		Varbind :: {Variable, Value} | {Column, RowIndex, Value} | {OID, Value},
		Variable :: atom(),
		Column :: atom(),
		OID :: snmpa:oid(),
		Value :: term(),
		Result :: {ok, AlarmIndex} | {error, Reason},
		AlarmIndex :: {ListName, DateAndTime, ActiveIndex},
		ActiveIndex :: pos_integer(),
		DateAndTime :: string(),
		Reason :: not_found | term().
%% @doc Add an active alarm instance.
add_alarm(RowIndex = _Model, Resource, Varbinds)
		when is_list(RowIndex) ->
	case mnesia:snmp_get_mnesia_key(alarmModelTable, RowIndex) of
		{ok, Key} ->
			add_alarm(Key, Resource, Varbinds);
		undefined ->
			{error, not_found}
	end;
add_alarm({ListName, Index, State} = Model, Resource, Varbinds)
		when is_list(ListName), is_integer(Index), is_integer(State),
		State > 1, is_list(Resource), is_list(Varbinds) ->
	[{_, EngineID}] = dets:lookup(snmpa_local_db1, snmpEngineID),
	{EngineAddressType, EngineAddress} = case dets:lookup(snmpa_local_db1,
			intAgentIpAddress) of
		[{_, Address}] when size(Address) =:= 4 ->
			{1, tuple_to_list(Address)};
		[{_, Address}] when size(Address) =:= 8 ->
			{2, tuple_to_list(Address)}
	end,
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	DateTime = datetime(),
	F = fun() ->
			[#alarmModelTable{alarmModelNotificationId = NotificationId,
					alarmModelSpecificPointer = SpecificPointer,
					alarmModelDescription = Description}]
					= mnesia:read(alarmModelTable, Model, read),
			PerceivedSeverity = case State of
				2 -> 2;
				3 -> 6;
				4 -> 5;
				5 -> 4;
				6 -> 3
			end,
			ItuAlarmIndex = {ListName, Index, PerceivedSeverity},
			[#ituAlarmTable{ituAlarmEventType = EventType,
					ituAlarmProbableCause = ProbableCause,
					ituAlarmAdditionalText = AdditionalText}]
					= mnesia:read(ituAlarmTable, ItuAlarmIndex, read),
			[{_, _, N}] = mnesia:read(snmp_variables,
					alarmActiveIndex, write),
			ActiveIndex = N + 1,
			mnesia:write({snmp_variables, alarmActiveIndex, ActiveIndex}),
			ModelPointer = AlarmModelNotificationId
					++ [length(ListName)] ++ ListName ++ [Index, State],
			Key = {ListName, DateTime, ActiveIndex},
			mnesia:write(#alarmActiveTable{key = Key,
					alarmActiveEngineID = EngineID,
					alarmActiveEngineAddressType = EngineAddressType,
					alarmActiveEngineAddress = EngineAddress,
					alarmActiveContextName = "",
					alarmActiveVariables = length(Varbinds),
					alarmActiveNotificationID = NotificationId,
					alarmActiveResourceId = Resource,
					alarmActiveDescription = Description,
					alarmActiveModelPointer = ModelPointer,
					alarmActiveSpecificPointer = SpecificPointer}),
			mnesia:write({snmp_variables,
					alarmActiveLastChanged, snmp_standard_mib:sys_up_time()}),
			[#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Current1,
					alarmActiveStatsActives = Total1} = S1]
					= mnesia:read(alarmActiveStatsTable, ListName, write),
			mnesia:write(S1#alarmActiveStatsTable{alarmActiveStatsActiveCurrent
					= Current1 + 1, alarmActiveStatsActives = Total1 + 1,
					alarmActiveStatsLastRaise = snmp_standard_mib:sys_up_time()}),
			case PerceivedSeverity of
				2 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsIndeterminateCurrent = Current2,
							ituAlarmActiveStatsIndeterminates = Total2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsIndeterminateCurrent = Current2 + 1,
							ituAlarmActiveStatsIndeterminates = Total2 + 1});
				3 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current2,
							ituAlarmActiveStatsCriticals = Total2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current2 + 1,
							ituAlarmActiveStatsCriticals = Total2 + 1});
				4 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsMajorCurrent = Current2,
							ituAlarmActiveStatsMajors = Total2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsMajorCurrent = Current2 + 1,
							ituAlarmActiveStatsMajors = Total2 + 1});
				5 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsMinorCurrent = Current2,
							ituAlarmActiveStatsMinors = Total2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsMinorCurrent = Current2 + 1,
							ituAlarmActiveStatsMinors = Total2 + 1});
				6 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsWarningCurrent = Current2,
							ituAlarmActiveStatsWarnings = Total2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsWarningCurrent = Current2 + 1,
							ituAlarmActiveStatsWarnings = Total2 + 1})
			end,
			{NotificationId, Key, ModelPointer, Description, PerceivedSeverity,
					EventType, ProbableCause, AdditionalText}
	end,
	case mnesia:transaction(F) of
		{atomic, {NotificationId, AlarmIndex, ModelPointer, Description,
				PerceivedSeverity, EventType, ProbableCause, AdditionalText}} ->
			{_, Notification} = snmpa:oid_to_name(NotificationId),
			Varbinds1 = [{alarmActiveModelPointer, ModelPointer},
					{alarmActiveResourceId, Resource},
					{alarmActiveDescription, Description},
					{ituAlarmPerceivedSeverity, PerceivedSeverity},
					{ituAlarmEventType, EventType}],
			Varbinds2 = case ProbableCause of
				ProbableCause when is_integer(ProbableCause) ->
					Varbinds1 ++ [{ituAlarmProbableCause, ProbableCause}];
				undefined ->
					Varbinds1
			end,
			Varbinds3 = case AdditionalText of
				AdditionalText when is_list(AdditionalText) ->
					Varbinds2 ++ [{ituAlarmAdditionalText, AdditionalText}];
				undefined ->
					Varbinds2
			end,
			snmpa:send_notification(snmp_master_agent,
					Notification, no_receiver, Varbinds3),
			{ok, AlarmIndex};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec clear_alarm(AlarmIndex) -> Result
	when
		AlarmIndex :: {ListName, DateAndTime, Index} | RowIndex,
		ListName :: string(),
		DateAndTime :: string(),
		Index :: pos_integer(),
		RowIndex :: [byte()],
		Result :: ok | {error, Reason},
		Reason :: not_found | term().
%% @doc Clear an active alarm instance.
clear_alarm(RowIndex = _AlarmIndex) when is_list(RowIndex) ->
	case mnesia:snmp_get_mnesia_key(alarmActiveTable, RowIndex) of
		{ok, Key} ->
			clear_alarm(Key);
		undefined ->
			{error, not_found}
	end;
clear_alarm({ListName, DateAndTime, Index} = AlarmIndex)
		when is_list(ListName), is_list(DateAndTime), is_integer(Index) ->
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	F = fun() ->
			[#alarmActiveTable{alarmActiveModelPointer = ModelPointer,
					alarmActiveResourceId = Resource}]
					= mnesia:read(alarmActiveTable, AlarmIndex, write),
			mnesia:delete(alarmActiveTable, AlarmIndex, write),
			[#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Current1} = S1]
				 	= mnesia:read(alarmActiveStatsTable, ListName, write),
			mnesia:write(S1#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Current1 - 1,
					alarmActiveStatsLastClear = snmp_standard_mib:sys_up_time()}),
			RowIndex = lists:sublist(ModelPointer,
					length(AlarmModelNotificationId) + 1, length(ModelPointer)),
			{ok, {_, _, State} = Key1} = mnesia:snmp_get_mnesia_key(alarmModelTable, RowIndex),
			case State of
				2 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsIndeterminateCurrent = Current2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsIndeterminateCurrent = Current2 - 1});
				3 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsWarningCurrent = Current2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsWarningCurrent = Current2 - 1});
				4 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsMinorCurrent = Current2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsMinorCurrent = Current2 - 1});
				5 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsMajorCurrent = Current2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsMajorCurrent = Current2 - 1});
				6 ->
					[#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current2} = S2]
							= mnesia:read(ituAlarmActiveStatsTable, ListName, write),
					mnesia:write(S2#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current2 - 1})
			end,
			Key2 = setelement(3, Key1, 1),
			case mnesia:read(alarmModelTable, Key2, read) of
				[#alarmModelTable{alarmModelDescription = Description,
						alarmModelNotificationId = NotificationId}] ->
					[#ituAlarmTable{ituAlarmEventType = EventType,
							ituAlarmProbableCause = ProbableCause,
							ituAlarmAdditionalText = AdditionalText}]
							= mnesia:read(ituAlarmTable, Key2, read),
					{NotificationId, ModelPointer, Resource,
							Description, EventType, ProbableCause, AdditionalText};
				[] ->
					ok
			end
	end,
	case mnesia:transaction(F) of
		{atomic, {NotificationId, ModelPointer, Resource, Description,
				EventType, ProbableCause, AdditionalText}} ->
			{_, Notification} = snmpa:oid_to_name(NotificationId),
			Varbinds1 = [{alarmActiveModelPointer, ModelPointer},
					{alarmActiveResourceId, Resource},
					{alarmActiveDescription, Description},
					{ituAlarmPerceivedSeverity, 1},
					{ituAlarmEventType, EventType}],
			Varbinds2 = case ProbableCause of
				ProbableCause when is_integer(ProbableCause) ->
					Varbinds1 ++ [{ituAlarmProbableCause, ProbableCause}];
				undefined ->
					Varbinds1
			end,
			Varbinds3 = case AdditionalText of
				AdditionalText when is_list(AdditionalText) ->
					Varbinds2 ++ [{ituAlarmAdditionalText, AdditionalText}];
				undefined ->
					Varbinds2
			end,
			snmpa:send_notification(snmp_master_agent,
					Notification, no_receiver, Varbinds3),
			ok;
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec add_worker(N) -> Result
	when
		N :: pos_integer(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Add alarm generation workers.
%%
%% 	Each worker adds and clears active alarms at the
%% 	rate determined by application environment variables
%% 	`mean' and `deviation' so that the total traffic
%% 	generated is multiplied by the number of workers.
%%
add_worker(N) when is_integer(N), N > 0 ->
	case supervisor:start_child(snmp_simulator_fsm_sup, [[], []]) of
		{ok, _Child} ->
			add_worker(N - 1);
		{error, Reason} ->
			{error, Reason}
	end;
add_worker(0) ->
	ok.

-spec datetime() -> DateTime
	when
		DateTime :: string().
%% @doc Returns the current time in ISO 8601 format.
datetime() ->
	iso8601(erlang:system_time(millisecond)).

-spec iso8601(DateTime) -> DateTime
	when
		DateTime :: pos_integer() | string().
%% @doc Convert between ISO 8601 and Unix epoch milliseconds.
%% 	Parsing is not strict to allow prefix matching.
iso8601(DateTime) when is_integer(DateTime) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = date(DateTime),
	DateFormat = "~4.10.0b-~2.10.0b-~2.10.0b",
	TimeFormat = "T~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
	Chars = io_lib:fwrite(DateFormat ++ TimeFormat,
			[Year, Month, Day, Hour, Minute, Second, DateTime rem 1000]),
	lists:flatten(Chars);
iso8601([Y1, Y2, Y3, Y4 | T])
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
		Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	iso8601month(list_to_integer([Y1, Y2, Y3, Y4]), T).
%% @hidden
iso8601month(Year, []) ->
	DateTime = {{Year, 1, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601month(Year, [$-]) ->
	iso8601month(Year, []);
iso8601month(Year, [$-, $0]) ->
	iso8601month(Year, [$-, $0, $1]);
iso8601month(Year, [$-, $1]) ->
	iso8601month(Year, [$-, $1, $0]);
iso8601month(Year, [$-, M1, M2 | T])
		when M1 >= $0, M1 =< $1, M2 >= $0, M2 =< $9 ->
	iso8601day(Year, list_to_integer([M1, M2]), T).
%% @hidden
iso8601day(Year, Month, []) ->
	DateTime = {{Year, Month, 1}, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601day(Year, Month, [$-]) ->
	iso8601day(Year, Month, []);
iso8601day(Year, Month, [$-, $0]) ->
	iso8601day(Year, Month, [$-, $1, $0]);
iso8601day(Year, Month, [$-, D1])
		when D1 >= $1, D1 =< $3 ->
	iso8601day(Year, Month, [$-, D1, $0]);
iso8601day(Year, Month, [$-, D1, D2 | T])
		when D1 >= $0, D1 =< $3, D2 >= $0, D2 =< $9 ->
	Day = list_to_integer([D1, D2]),
	iso8601hour({Year, Month, Day}, T).
%% @hidden
iso8601hour(Date, []) ->
	DateTime = {Date, {0, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601hour(Date, [$T]) ->
	iso8601hour(Date, []);
iso8601hour(Date, [$T, H1])
		when H1 >= $0, H1 =< $2 ->
	iso8601hour(Date, [$T, H1, $0]);
iso8601hour(Date, [$T, H1, H2 | T])
		when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9 ->
	Hour = list_to_integer([H1, H2]),
	iso8601minute(Date, Hour, T).
%% @hidden
iso8601minute(Date, Hour, []) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601minute(Date, Hour, [$:]) ->
	iso8601minute(Date, Hour, []);
iso8601minute(Date, Hour, [$:, M1])
		when M1 >= $0, M1 =< $5 ->
	iso8601minute(Date, Hour, [$:, M1, $0]);
iso8601minute(Date, Hour, [$:, M1, M2 | T])
		when M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9 ->
	Minute = list_to_integer([M1, M2]),
	iso8601second(Date, Hour, Minute, T);
iso8601minute(Date, Hour, _) ->
	DateTime = {Date, {Hour, 0, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601second(Date, Hour, Minute, []) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000;
iso8601second(Date, Hour, Minute, [$:]) ->
	iso8601second(Date, Hour, Minute, []);
iso8601second(Date, Hour, Minute, [$:, S1])
		when S1 >= $0, S1 =< $5 ->
	iso8601second(Date, Hour, Minute, [$:, S1, $0]);
iso8601second(Date, Hour, Minute, [$:, S1, S2 | T])
		when S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
	Second = list_to_integer([S1, S2]),
	DateTime = {Date, {Hour, Minute, Second}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	EpocMilliseconds = (GS - ?EPOCH) * 1000,
	iso8601millisecond(EpocMilliseconds, T);
iso8601second(Date, Hour, Minute, _) ->
	DateTime = {Date, {Hour, Minute, 0}},
	GS = calendar:datetime_to_gregorian_seconds(DateTime),
	(GS - ?EPOCH) * 1000.
%% @hidden
iso8601millisecond(EpocMilliseconds, []) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$.]) ->
	EpocMilliseconds;
iso8601millisecond(EpocMilliseconds, [$., N1, N2, N3 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9,
		N3 >= $0, N3 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2, N3]);
iso8601millisecond(EpocMilliseconds, [$., N1, N2 | _])
		when N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9 ->
	EpocMilliseconds + list_to_integer([N1, N2]) * 10;
iso8601millisecond(EpocMilliseconds, [$., N | _])
		when N >= $0, N =< $9 ->
	EpocMilliseconds + list_to_integer([N]) * 100;
iso8601millisecond(EpocMilliseconds, _) ->
	EpocMilliseconds.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec date(MilliSeconds) -> Result
	when
		MilliSeconds :: pos_integer(),
		Result :: calendar:datetime().
%% @doc Convert timestamp to date and time.
date(MilliSeconds) when is_integer(MilliSeconds) ->
	Seconds = ?EPOCH + (MilliSeconds div 1000),
	calendar:gregorian_seconds_to_datetime(Seconds).

