%%% snmp_simulator_api_SUITE.erl
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
%%% Test suite for the public API of the
%%% {@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator_api_SUITE).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("snmp_simulator.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("common_test/include/ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	AgentPort = rand:uniform(64511) + 1024,
	AgentEngineId = sigscale_snmp_lib:engine_id(),
	[{userdata, [{doc, "Test suite for public API of SigScale SNMP Simulator"}]},
	{timetrap, {seconds, 60}},
	{require, snmp_mgr_agent, snmp},
	{default_config, snmp,
			[{start_agent, true},
			{agent_engine_id, AgentEngineId},
			{agent_udp, AgentPort},
			{agent_vsns, [v1, v2]},
			{agent_community, [{"private", "private", "all-rights", "", ""}]},
			{start_manager, false}]},
	{require, snmp_app},
	{default_config, snmp_app,
			[{agent,
					[{config, [{verbosity, silence}]},
					{agent_verbosity, silence},
					{net_if, [{verbosity, silence}]}]}]}].

-spec init_per_suite(Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before the whole suite.
%%
init_per_suite(Config) ->
	ok = snmp_simulator_test_lib:initialize_db(Config),
	ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
	ok = snmp_simulator_test_lib:start(Config),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	ok = snmp_simulator_test_lib:stop(),
	ok = ct_snmp:stop(Config).

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> any().
%% Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

-spec sequences() -> Sequences :: [{SeqName :: atom(), Testcases :: [atom()]}].
%% Group test cases into a test sequence.
%%
sequences() ->
	[].

-spec all() -> TestCases :: [Case :: atom()].
%% Returns a list of all test cases in this test suite.
%%
all() ->
	[import, add_alarm, clear_alarm].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

import() ->
	[{userdata, [{doc, "Import alarm model."}]}].

import(Config) ->
	PrivDir = ?config(priv_dir, Config),
	Chars = ["{alarmClearState, 2, 1, \"Temperature Normal\", undefined, "
			"alarmActiveResourceId, undefined, clear, environmentalAlarm, "
			"undefined, undefined}.", $\n,
			"{alarmActiveState, 2, 4, \"Temperature Low\", undefined, "
			"alarmActiveResourceId, undefined, minor, environmentalAlarm, "
			"lowTemperature, undefined}.", $\n,
			"{alarmActiveState, 2, 5, \"Temperature Low\", undefined, "
			"alarmActiveResourceId, undefined, major, environmentalAlarm, "
			"lowTemperature, undefined}.", $\n,
			"{alarmActiveState, 2, 6, \"Temperature Low\", undefined, "
			"alarmActiveResourceId, undefined, critical, environmentalAlarm, "
			"lowTemperature, undefined}.", $\n,
			"{alarmActiveState, 2, 4, \"Temperature High\", undefined, "
			"alarmActiveResourceId, undefined, minor, environmentalAlarm, "
			"highTemperature, undefined}.", $\n,
			"{alarmActiveState, 2, 5, \"Temperature High\", undefined, "
			"alarmActiveResourceId, undefined, major, environmentalAlarm, "
			"highTemperature, undefined}.", $\n,
			"{alarmActiveState, 2, 6, \"Temperature High\", undefined, "
			"alarmActiveResourceId, undefined, critical, environmentalAlarm, "
			"highTemperature, undefined}.", $\n],
	ok = file:write_file(PrivDir ++ "/example-alarm-model", Chars),
	ok = snmp_simulator_import:file(PrivDir ++ "/example-alarm-model"),
	AlarmModelTable = {alarmModelTable, mnesia},
	ItuAlarmTable = {ituAlarmTable, mnesia},
	{value, AlarmClearState} = snmpa:name_to_oid(alarmClearState),
	{value, AlarmActiveState} = snmpa:name_to_oid(alarmActiveState),
	{value, ItuAlarmEventType} = snmpa:name_to_oid(ituAlarmEventType),
	{value, AlarmActiveResourceId} = snmpa:name_to_oid(alarmActiveResourceId),
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	Index1 = snmp_generic:table_next(AlarmModelTable, []),
	AlarmModelSpecificPointer1 = ItuAlarmEventType ++ itu_index(Index1),
	[1, 1, AlarmClearState, 2, 1, "Temperature Normal",
			AlarmModelSpecificPointer1, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel1 = AlarmModelNotificationId ++ Index1,
	[1, 6, undefined, undefined,
			ItuAlarmGenericModel1] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index1), [1, 2, 3, 4, 5]),
	Index2 = snmp_generic:table_next(AlarmModelTable, Index1),
	AlarmModelSpecificPointer2 = ItuAlarmEventType ++ itu_index(Index2),
	[2, 4, AlarmActiveState, 2, 4, "Temperature Low",
			AlarmModelSpecificPointer2, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index2, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel2 = AlarmModelNotificationId ++ Index2,
	[5, 6, 130, undefined,
			ItuAlarmGenericModel2] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index2), [1, 2, 3, 4, 5]),
	Index3 = snmp_generic:table_next(AlarmModelTable, Index2),
	AlarmModelSpecificPointer3 = ItuAlarmEventType ++ itu_index(Index3),
	[3, 5, AlarmActiveState, 2, 5, "Temperature Low",
			AlarmModelSpecificPointer3, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index3, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel3 = AlarmModelNotificationId ++ Index3,
	[4, 6, 130, undefined,
			ItuAlarmGenericModel3] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index3), [1, 2, 3, 4, 5]),
	Index4 = snmp_generic:table_next(AlarmModelTable, Index3),
	AlarmModelSpecificPointer4 = ItuAlarmEventType ++ itu_index(Index4),
	[4, 6, AlarmActiveState, 2, 6, "Temperature Low",
			AlarmModelSpecificPointer4, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index4, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel4 = AlarmModelNotificationId ++ Index4,
	[3, 6, 130, undefined,
			ItuAlarmGenericModel4] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index4), [1, 2, 3, 4, 5]),
	Index5 = snmp_generic:table_next(AlarmModelTable, Index4),
	AlarmModelSpecificPointer5 = ItuAlarmEventType ++ itu_index(Index5),
	[5, 4, AlarmActiveState, 2, 4, "Temperature High",
			AlarmModelSpecificPointer5, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index5, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel5 = AlarmModelNotificationId ++ Index5,
	[5, 6, 123, undefined,
			ItuAlarmGenericModel5] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index5), [1, 2, 3, 4, 5]),
	Index6 = snmp_generic:table_next(AlarmModelTable, Index5),
	AlarmModelSpecificPointer6 = ItuAlarmEventType ++ itu_index(Index6),
	[6, 5, AlarmActiveState, 2, 5, "Temperature High",
			AlarmModelSpecificPointer6, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index6, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel6 = AlarmModelNotificationId ++ Index6,
	[4, 6, 123, undefined,
			ItuAlarmGenericModel6] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index6), [1, 2, 3, 4, 5]),
	Index7 = snmp_generic:table_next(AlarmModelTable, Index6),
	AlarmModelSpecificPointer7 = ItuAlarmEventType ++ itu_index(Index7),
	[7, 6, AlarmActiveState, 2, 6, "Temperature High",
			AlarmModelSpecificPointer7, AlarmActiveResourceId, [0,0],
			1] = snmp_generic:table_get_elements(AlarmModelTable,
			Index7, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
	ItuAlarmGenericModel7 = AlarmModelNotificationId ++ Index7,
	[3, 6, 123, undefined,
			ItuAlarmGenericModel7] = snmp_generic:table_get_elements(ItuAlarmTable,
			itu_index(Index7), [1, 2, 3, 4, 5]).

add_alarm() ->
	[{userdata, [{doc, "Add an active alarm."}]}].

add_alarm(Config) ->
	PrivDir = ?config(priv_dir, Config),
	Chars = ["{alarmActiveState, 2, 4, \"Power Failure\", undefined, "
			"alarmActiveResourceId, undefined, critical, equipmentAlarm, "
			"powerProblem, undefined}.", $\n],
	ok = file:write_file(PrivDir ++ "/power-alarm", Chars),
	ok = snmp_simulator_import:file(PrivDir ++ "/power-alarm"),
	[{_, EngineID}] = dets:lookup(snmpa_local_db1, snmpEngineID),
	{value, AlarmActiveState} = snmpa:name_to_oid(alarmActiveState),
	F1 = fun() ->
			MatchSpec = [{#alarmModelTable{key = '$1',
					alarmModelDescription = "Power Failure", _ = '_'}, [], ['$1']}],
			mnesia:select(alarmModelTable, MatchSpec, read)
	end,
	{atomic, [{ListName, Index, State} = Model]} = mnesia:transaction(F1),
	{value, AlarmModelNotificationId} = snmpa:name_to_oid(alarmModelNotificationId),
	ModelPointer = AlarmModelNotificationId
			++ [length(ListName)] ++ ListName ++ [Index, State],
	{value, ItuAlarmEventType} = snmpa:name_to_oid(ituAlarmEventType),
	SpecificPointer = ItuAlarmEventType
			++ [length(ListName)] ++ ListName ++ [Index, 3],
	F2 = fun() ->
			[#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Current,
					alarmActiveStatsActives = Total}]
					= mnesia:read(alarmActiveStatsTable, ListName, read),
			{Current, Total}
	end,
	{atomic, {ActiveCurrent1, ActiveTotal1}} = mnesia:transaction(F2),
	F3 = fun() ->
			[#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current,
					ituAlarmActiveStatsCriticals = Total}]
					= mnesia:read(ituAlarmActiveStatsTable, ListName, read),
			{Current, Total}
	end,
	{atomic, {CriticalCurrent1, CriticalTotal1}} = mnesia:transaction(F3),
	Resource = resource(),
	{ok, AlarmIndex} = snmp_simulator:add_alarm(Model, Resource),
	F4 = fun() ->
			mnesia:read(alarmActiveTable, AlarmIndex, read)
	end,
	{atomic, [#alarmActiveTable{alarmActiveEngineID = EngineID,
			alarmActiveEngineAddressType = 1,
			alarmActiveEngineAddress = [127,0, 0, 1],
			alarmActiveNotificationID = AlarmActiveState,
			alarmActiveResourceId = Resource,
			alarmActiveDescription = "Power Failure",
			alarmActiveModelPointer = ModelPointer,
			alarmActiveSpecificPointer = SpecificPointer}]}
			= mnesia:transaction(F4),
	ActiveCurrent2 = ActiveCurrent1 + 1,
	ActiveTotal2 = ActiveTotal1 + 1,
	{atomic, {ActiveCurrent2, ActiveTotal2}} = mnesia:transaction(F2),
	CriticalCurrent2 = CriticalCurrent1 + 1,
	CriticalTotal2 = CriticalTotal1 + 1,
	{atomic, {CriticalCurrent2, CriticalTotal2}} = mnesia:transaction(F3).

clear_alarm() ->
	[{userdata, [{doc, "Clear an active alarm."}]}].

clear_alarm(Config) ->
	PrivDir = ?config(priv_dir, Config),
	Chars = ["{alarmClearState, 2, 4, \"Power Failure - cleared\", undefined, "
			"alarmActiveResourceId, undefined, clear, equipmentAlarm, "
			"undefined, undefined}.", $\n],
	ok = file:write_file(PrivDir ++ "/power-alarm-cleared", Chars),
	ok = snmp_simulator_import:file(PrivDir ++ "/power-alarm-cleared"),
	F1 = fun() ->
			MatchSpec = [{#alarmModelTable{key = '$1',
					alarmModelDescription = "Power Failure", _ = '_'},
					[], ['$1']}],
			mnesia:select(alarmModelTable, MatchSpec, read)
	end,
	{atomic, [{ListName, _, _} = Model]} = mnesia:transaction(F1),
	{ok, AlarmIndex} = snmp_simulator:add_alarm(Model, resource()),
	F2 = fun() ->
			[#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Current}]
					= mnesia:read(alarmActiveStatsTable, ListName, read),
			Current
	end,
	{atomic, ActiveCurrent1} = mnesia:transaction(F2),
	F3 = fun() ->
			[#ituAlarmActiveStatsTable{ituAlarmActiveStatsCriticalCurrent = Current}]
					= mnesia:read(ituAlarmActiveStatsTable, ListName, read),
			Current
	end,
	{atomic, CriticalCurrent1} = mnesia:transaction(F3),
	ok = snmp_simulator:clear_alarm(AlarmIndex),
	F4 = fun() ->
			mnesia:read(alarmActiveTable, AlarmIndex, read)
	end,
	{atomic, []} = mnesia:transaction(F4),
	ActiveCurrent2 = ActiveCurrent1 - 1,
	{atomic, ActiveCurrent2} = mnesia:transaction(F2),
	CriticalCurrent2 = CriticalCurrent1 - 1,
	{atomic, CriticalCurrent2} = mnesia:transaction(F3).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

-spec itu_index(AlarmModelIndex) -> ItuAlarmIndex
	when
		AlarmModelIndex :: [byte()],
		ItuAlarmIndex :: [byte()].
%% @doc Map AlarmModelTable RowIndex to ItuAlarmIndex RowIndex.
%% @hidden
itu_index(Index) ->
	itu_index1(lists:split(length(Index) - 1, Index)).
%% @hidden
itu_index1({Prefix, [1]}) ->
	Prefix ++ [1];
itu_index1({Prefix, [2]}) ->
	Prefix ++ [2];
itu_index1({Prefix, [3]}) ->
	Prefix ++ [6];
itu_index1({Prefix, [4]}) ->
	Prefix ++ [5];
itu_index1({Prefix, [5]}) ->
	Prefix ++ [4];
itu_index1({Prefix, [6]}) ->
	Prefix ++ [3].

%% @doc Generate a random resource OID.3pec resource() -> snmpa:oid().
%% @hidden
resource() ->
	resource(rand:uniform(6) + 3, []).
resource(0, Acc) ->
	% {value,[1,3,6,1,4,1,50386,5]} = snmampa:name_to_oid(sigscaleExperiment),
	[1,3,6,1,4,1,50386,5] ++ Acc;
resource(N, Acc) ->
	resource(N - 1, [rand:uniform(25) | Acc]).

