%% snmp_simulator_snmp_SUITE.erl
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
%%%  @doc Test suite for SNMP agent of the
%%% 	{@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator_snmp_SUITE).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-define(sigscalePEN, 50386).

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmp_simulator.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

-spec suite() -> DefaultData :: [tuple()].
%% Require variables and set default values for the suite.
%%
suite() ->
	AgentPort = rand:uniform(64511) + 1024,
	ManagerPort = rand:uniform(64511) + 1024,
	AgentEngineId = sigscale_snmp_lib:engine_id(),
	F = fun F(Acc) when length(Acc) < 27 ->
				F([rand:uniform(47) + 47 | Acc]);
			F(Acc) ->
				[128,0,196,210,5] ++ Acc
	end,
   ManagerEngineId = F([]),
	[{userdata, [{doc, "Test suite for SNMP agent in SigScale SNMP Simulator"}]},
	{timetrap, {seconds, 60}},
	{require, snmp_mgr_agent, snmp},
	{default_config, snmp,
			[{start_agent, true},
			{agent_engine_id, AgentEngineId},
			{agent_udp, AgentPort},
			{agent_vsns, [v1, v2]},
			{agent_community, [{"public", "public", "ct", "", ""}]},
			{agent_target_address_def,
					[{"ct_trap", transportDomainUdpIpv4, {[127,0,0,1], ManagerPort},
					4000, 3, "ct_tag", "ct_params", ManagerEngineId, [], 2048}]},
			{start_manager, true},
			{engine_id, ManagerEngineId},
			{mgr_port, ManagerPort},
			{users, [{ct, [snmpm_user_default, []]}]},
			{managed_agents, [{simulator, [ct, {127,0,0,1}, AgentPort,
					[{engine_id, AgentEngineId}, {community, "public"}]]}]}]},
	{require, snmp_app},
	{default_config, snmp_app,
			[{manager,
					[{config, [{verbosity, silence}]},
					{server, [{verbosity, silence}]},
					{notestore, [{verbosity, silence}]},
					{net_if, [{verbosity, silence}]}]},
			{agent,
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
	PrivDir = code:priv_dir(snmp_simulator),
	MibDir = PrivDir ++ "/mibs/",
	ok = snmpm:load_mib(MibDir ++ "ALARM-MIB"),
	ok = snmpm:load_mib(MibDir ++ "IANA-ITU-ALARM-TC-MIB"),
	ok = snmpm:load_mib(MibDir ++ "IANAifType-MIB"),
	ok = snmpm:load_mib(MibDir ++ "IF-MIB"),
	ok = snmpm:load_mib(MibDir ++ "ITU-ALARM-MIB"),
	ok = snmpm:load_mib(MibDir ++ "ITU-ALARM-TC-MIB"),
	ok = snmpm:load_mib(MibDir ++ "RMON-MIB"),
	ok = snmpm:load_mib(MibDir ++ "RMON2-MIB"),
	Config.

-spec end_per_suite(Config :: [tuple()]) -> any().
%% Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = snmp_simulator_test_lib:stop().
	% ok = ct_snmp:stop(Config). % deletes configuration files

-spec init_per_testcase(TestCase :: atom(), Config :: [tuple()]) -> Config :: [tuple()].
%% Initialization before each test case.
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
	[get_model_last_changed, add_model].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

get_model_last_changed() ->
	[{userdata, [{doc, "Get the time of the last model change."}]}].

get_model_last_changed(_Config) ->
	[User] = snmpm:which_users(),
	[Agent] = snmpm:which_agents(),
	{ok, [AlarmModelLastChanged]} = snmpm:name_to_oid(alarmModelLastChanged),
	{ok, {noError, _, [#varbind{variabletype = 'TimeTicks',
			value = Value}]}, _} = snmpm:sync_get(User, Agent,
			[AlarmModelLastChanged ++ [0]]),
	true = is_integer(Value).

add_model() ->
	[{userdata, [{doc, "Add an alarm model."}]}].

add_model(_Config) ->
	[User] = snmpm:which_users(),
	[Agent] = snmpm:which_agents(),
	{ok, [AlarmModelNotificationId]} = snmpm:name_to_oid(alarmModelNotificationId),
	{ok, [AlarmModelVarbindIndex]} = snmpm:name_to_oid(alarmModelVarbindIndex),
	{ok, [AlarmModelVarbindValue]} = snmpm:name_to_oid(alarmModelVarbindValue),
	{ok, [AlarmModelDescription]} = snmpm:name_to_oid(alarmModelDescription),
	{ok, [AlarmModelSpecificPointer]} = snmpm:name_to_oid(alarmModelSpecificPointer),
	{ok, [AlarmModelVarbindSubtree]} = snmpm:name_to_oid(alarmModelVarbindSubtree),
	{ok, [AlarmModelResourcePrefix]} = snmpm:name_to_oid(alarmModelResourcePrefix),
	{ok, [AlarmModelRowStatus]} = snmpm:name_to_oid(alarmModelRowStatus),
	{ok, [AlarmClearState]} = snmpm:name_to_oid(alarmClearState),
	Index = [0, 1, 1],
	OidVars = [{AlarmModelNotificationId ++ Index, AlarmClearState},
			{AlarmModelVarbindIndex ++ Index, 0},
			{AlarmModelVarbindValue ++ Index, 1},
			{AlarmModelDescription ++ Index, "Clear Alarm"},
			{AlarmModelSpecificPointer ++ Index, [0,0]},
			{AlarmModelVarbindSubtree ++ Index, [0,0]},
			{AlarmModelResourcePrefix ++ Index, [0,0]},
			{AlarmModelRowStatus ++ Index, 4}],
	{ok, {noError, _, VarBinds}, _} = snmpm:sync_set("simple_user",
			"simulator", OidVars),
	VarBinds = [#varbind{oid = AlarmModelNotificationId,
					variabletype = 'OBJECT IDENTIFIER',
					value = [1,3,6,1,2,1,118,0,3]},
			#varbind{oid = AlarmModelVarbindIndex,
					variabletype = 'Unsigned32', value = 0},
			#varbind{oid = AlarmModelVarbindValue,
					variabletype = 'INTEGER', value = 1},
			#varbind{oid = AlarmModelDescription,
					variabletype = 'OCTET STRING', value = "Clear Alarm"},
			#varbind{oid = AlarmModelSpecificPointer,
					variabletype = 'OBJECT IDENTIFIER', value = [0,0]},
			#varbind{oid = AlarmModelVarbindSubtree,
					variabletype = 'OBJECT IDENTIFIER', value = [0,0]},
			#varbind{oid = AlarmModelResourcePrefix,
					variabletype = 'OBJECT IDENTIFIER', value = [0,0]},
			#varbind{oid = AlarmModelRowStatus,
					variabletype = 'INTEGER', value = 4}].

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

