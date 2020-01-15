%%% snmp_simulator_mib.erl
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc This library module implements the SNMP MIB for the
%%%     {@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator_mib).
-copyright('Copyright (c) 2018-2020 SigScale Global Inc.').

%% export the snmp_simulator_mib public API
-export([load/0, load/1, unload/0, unload/1]).

%% export the snmp_simulator_mib snmp agent callbacks
-export([alarm_model_table/3]).

-include("snmp_simulator.hrl").

%%----------------------------------------------------------------------
%%  The snmp_simulator_mib public API
%%----------------------------------------------------------------------

-spec load() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SNMP agent simulator MIB.
load() ->
	case code:priv_dir(snmp_simulator) of
		PrivDir when is_list(PrivDir) ->
			MibDir = PrivDir ++ "/mibs/",
			Mibs = [MibDir ++ MIB || MIB <- mibs()],
			snmpa:load_mibs(Mibs);
		{error, Reason} ->
			{error, Reason}
	end.

-spec load(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Loads the SNMP agent simulator MIB.
load(Agent) ->
	case code:priv_dir(snmp_simulator) of
		PrivDir when is_list(PrivDir) ->
			MibDir = PrivDir ++ "/mibs/",
			Mibs = [MibDir ++ MIB || MIB <- mibs()],
			snmpa:load_mibs(Agent, Mibs);
		{error, Reason} ->
			{error, Reason}
	end.

-spec unload() -> Result
	when
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SNMP agent simulator MIB.
unload() ->
	snmpa:unload_mibs(mibs()).

-spec unload(Agent) -> Result
	when
		Agent :: pid() | atom(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Unloads the SNMP agent simulator MIB.
unload(Agent) ->
	snmpa:unload_mibs(Agent, mibs()).

%%----------------------------------------------------------------------
%% The snmp_simulator_mib snmp agent callbacks
%----------------------------------------------------------------------

-spec alarm_model_table(Operation, RowIndex, Columns) -> Result
	when
		Operation :: get | get_next | is_set_ok | undo | set,
		RowIndex :: ObjectId,
		ObjectId :: [integer()],
		Columns :: [Column],
		Column :: integer(),
		Result :: [Element] | {genErr, Column} | {noValue, Error},
		Error :: noSuchObject | noSuchInstance | commitFailed | undoFailed,
		Element :: {value, Value} | {ObjectId, Value},
		Value :: atom() | integer() | string() | [integer()].
%% @doc Handle SNMP requests for the alarm model table.
%% @private
alarm_model_table(set = Operation, RowIndex, Columns) ->
	case snmp_generic:variable_func(set, snmp_standard_mib:sys_up_time(),
			{alarmModelLastChanged, mnesia}) of
		noError ->
			snmp_generic:table_func(Operation, RowIndex, Columns,
					{alarmModelTable, mnesia});
		Error ->
			Error
	end;
alarm_model_table(Operation, RowIndex, Columns) ->
	snmp_generic:table_func(Operation, RowIndex, Columns,
			{alarmModelTable, mnesia}).

%%----------------------------------------------------------------------
%% internal functions
%----------------------------------------------------------------------

%% @hidden
mibs() ->
	["ALARM-MIB", "ITU-ALARM-MIB", "IANA-ITU-ALARM-TC-MIB",
			"ITU-ALARM-TC-MIB", "IANAifType-MIB", "IF-MIB",
			"RMON-MIB", "RMON2-MIB"].

