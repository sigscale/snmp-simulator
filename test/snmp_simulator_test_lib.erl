%% snmp_simulator_test_lib.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2019-2020 SigScale Global Inc.
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
%%%  @doc Test suite support library in the
%%% 	{@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator_test_lib).
-copyright('Copyright (c) 2019-2020 SigScale Global Inc.').

%% common_test required callbacks
-export([initialize_db/1, start/1, stop/0]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("snmp_simulator.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

initialize_db(Config) ->
	PrivDir = ?config(priv_dir, Config),
	DbDir = PrivDir ++ "db",
	case file:make_dir(DbDir) of
		ok ->
			ok = application:set_env(mnesia, dir, DbDir),
			initialize_db();
		{error, eexist} ->
			ok = application:set_env(mnesia, dir, DbDir),
			initialize_db();
		{error, Reason} ->
			{error, Reason}
	end.
initialize_db() ->
	case mnesia:system_info(is_running) of
		no ->
			ok = application:start(mnesia),
			initialize_db();
		S when S == starting; S == stopping ->
			receive
				after 1000 ->
					initialize_db()
			end;
		yes ->
			Tables = [snmp_variables, alarmModelTable, alarmActiveTable,
					alarmActiveVariableTable, alarmActiveStatsTable,
					alarmClearTable, ituAlarmTable, ituAlarmActiveTable,
					ituAlarmActiveStatsTable],
			case mnesia:wait_for_tables(Tables, 1000) of
				{timeout, _} ->
					ok = application:stop(mnesia),
					{ok, _} = snmp_simulator_app:install(),
					ok;
				ok ->
					ok
			end
	end.

start(Config) ->
	start(Config, [crypto, asn1, mnesia, snmp]).
start(Config, [H | T]) ->
	case application:start(H) of
		ok  ->
			start(Config, T);
	{error, {already_started, H}} ->
		start(Config, T);
	{error, Reason} ->
		{error, Reason}
	end;
start(Config, []) ->
	ok = application:load(snmp_simulator),
	PrivDir = ?config(priv_dir, Config),
	DbDir = PrivDir ++ "db",
	case file:make_dir(DbDir) of
		ok ->
			start1(Config, PrivDir);
		{error, eexist} ->
			start1(Config, PrivDir);
		{error, Reason} ->
			{error, Reason}
	end.
start1(Config, PrivDir) ->
	LogDir = PrivDir ++ "log",
	case file:make_dir(LogDir) of
		ok ->
			start2(Config, PrivDir);
		{error, eexist} ->
			start2(Config, PrivDir);
		{error, Reason} ->
			{error, Reason}
	end.
start2(_Config, _PrivDir) ->
	application:start(snmp_simulator).

stop() ->
	application:stop(snmp_simulator).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

