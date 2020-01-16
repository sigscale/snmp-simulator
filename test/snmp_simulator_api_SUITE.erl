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
	[import].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

import() ->
	[{userdata, [{doc, "Import alarm models."}]}].

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
	ok = file:write_file(PrivDir ++ "/example-alarm-models", Chars),
	ok = snmp_simulator_import:file(PrivDir ++ "/example-alarm-models").


%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

