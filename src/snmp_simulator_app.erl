%%% snmp_simulator_app.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2016 - 2020 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/application. application} behaviour callback
%%%   module starts and stops the
%%%   {@link //snmp_simulator. snmp_simulator} application.
%%%
-module(snmp_simulator_app).
-copyright('Copyright (c) 2016 - 2020 SigScale Global Inc.').

-behaviour(application).

%% callbacks needed for application behaviour
-export([start/2, stop/1, config_change/3]).
%% optional callbacks for application behaviour
-export([prep_stop/1, start_phase/3]).
%% export the snmp_simulator_app private API for installation
-export([install/0, install/1]).

-define(WAITFORSCHEMA, 9000).
-define(WAITFORTABLES, 9000).

-include("snmp_simulator.hrl").

-record(state, {}).

%%----------------------------------------------------------------------
%%  The snmp_simulator_app aplication callbacks
%%----------------------------------------------------------------------

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-spec start(StartType, StartArgs) -> Result
	when
		StartType :: start_type(),
		StartArgs :: term(),
		Result :: {ok, pid()} | {ok, pid(), State} | {error, Reason},
		State :: #state{},
		Reason :: term().
%% @doc Starts the application processes.
start(normal = _StartType, _Args) ->
	Tables = [snmp_variables, alarmModelTable, alarmActiveTable,
			alarmActiveVariableTable, alarmActiveStatsTable, alarmClearTable,
			ituAlarmTable, ituAlarmActiveTable, ituAlarmActiveStatsTable],
	case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
		ok ->
			start2();
		{timeout, BadTabList} ->
			case force(BadTabList) of
				ok ->
					start2();
				{error, Reason} ->
					error_logger:error_report(["SNMP agent simulator failed to start",
							{reason, Reason}, {module, ?MODULE}]),
						{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start2() ->
	F = fun() ->
			mnesia:write(#alarmActiveStatsTable{key = [],
					alarmActiveStatsActiveCurrent = 0,
					alarmActiveStatsActives = 0,
					alarmActiveStatsLastRaise = 0,
					alarmActiveStatsLastClear = 0})
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			start3();
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
start3() ->
	F = fun() ->
			mnesia:write(#ituAlarmActiveStatsTable{key = [],
					ituAlarmActiveStatsIndeterminateCurrent = 0,
					ituAlarmActiveStatsCriticalCurrent = 0,
					ituAlarmActiveStatsMajorCurrent = 0,
					ituAlarmActiveStatsMinorCurrent = 0,
					ituAlarmActiveStatsWarningCurrent = 0,
					ituAlarmActiveStatsIndeterminates = 0,
					ituAlarmActiveStatsCriticals = 0,
					ituAlarmActiveStatsMajors = 0,
					ituAlarmActiveStatsMinors = 0,
					ituAlarmActiveStatsWarnings = 0})
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			start4();
		{aborted, Reason} ->
			{error, Reason}
	end.
%% @hidden
start4() ->
	case snmp_simulator_mib:load() of
		ok ->
			start5();
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start5() ->
	case supervisor:start_link(snmp_simulator_sup, []) of
		{ok, TopSup} ->
			start6(TopSup);
		{error, Reason} ->
			{error, Reason}
	end.
%% @hidden
start6(TopSup) ->
	{ok, N} = application:get_env(worker),
	case snmp_simulator:add_worker(N) of
		ok ->
			{ok, TopSup};
		{error, Reason} ->
			{error, Reason}
	end.

-spec start_phase(Phase, StartType, PhaseArgs) -> Result
	when
		Phase :: atom(),
		StartType :: start_type(),
		PhaseArgs :: term(),
		Result :: ok | {error, Reason},
		Reason :: term().
%% @doc Called for each start phase in the application and included
%%   applications.
%% @see //kernel/app
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

-spec prep_stop(State) -> #state{}
	when
		State :: #state{}.
%% @doc Called when the application is about to be shut down,
%%   before any processes are terminated.
%% @see //kernel/application:stop/1
%%
prep_stop(State) ->
	State.

-spec stop(State) -> any()
	when
		State :: #state{}.
%% @doc Called after the application has stopped to clean up.
%%
stop(_State) ->
	ok.

-spec config_change(Changed, New, Removed) -> ok
	when
		Changed:: [{Par, Val}],
		New :: [{Par, Val}],
		Removed :: [Par],
		Par :: atom(),
		Val :: atom().
%% @doc Called after a code  replacement, if there are any
%%   changes to the configuration  parameters.
%%
config_change(_Changed, _New, _Removed) ->
	ok.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

-spec install() -> Result
	when
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @equiv install([node() | nodes()])
install() ->
	Nodes = [node() | nodes()],
	install(Nodes).

-spec install(Nodes) -> Result
	when
		Nodes :: [node()],
		Result :: {ok, Tables},
		Tables :: [atom()].
%% @doc Initialize Snmp Collector Application tables.
%% 	`Nodes' is a list of the nodes where
%%
%% 	If {@link //mnesia. mnesia} is not running an attempt
%% 	will be made to create a schema on all available nodes.
%% 	If a schema already exists on any node
%% 	{@link //mnesia. mnesia} will be started on all nodes
%% 	using the existing schema.
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	case mnesia:system_info(is_running) of
		no ->
			case mnesia:create_schema(Nodes) of
				ok ->
					error_logger:info_report("Created mnesia schema",
							[{nodes, Nodes}]),
					install1(Nodes);
				{error, Reason} ->
					error_logger:error_report(["Failed to create schema",
							mnesia:error_description(Reason),
							{nodes, Nodes}, {error, Reason}]),
					{error, Reason}
			end;
		_ ->
			install2(Nodes)
	end.
%% @hidden
install1([Node] = Nodes) when Node == node() ->
	case mnesia:start() of
		ok ->
			error_logger:info_msg("Started mnesia~n"),
			install2(Nodes);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
					{error, Reason}]),
			{error, Reason}
	end;
install1(Nodes) ->
	case rpc:multicall(Nodes, mnesia, start, [], 50000) of
		{Results, []} ->
			F = fun(ok) ->
						false;
					(_) ->
						true
			end,
			case lists:filter(F, Results) of
				[] ->
					error_logger:info_report(["Started mnesia on all nodes",
							{nodes, Nodes}]),
					install2(Nodes);
				NotOKs ->
					error_logger:error_report(["Failed to start mnesia"
							" on all nodes", {nodes, Nodes}, {errors, NotOKs}]),
					{error, NotOKs}
			end;
		{Results, BadNodes} ->
			error_logger:error_report(["Failed to start mnesia"
					" on all nodes", {nodes, Nodes}, {results, Results},
					{badnodes, BadNodes}]),
			{error, {Results, BadNodes}}
	end.
%% @hidden
install2(Nodes) ->
	case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
		ok ->
			install3(Nodes, []);
		{error, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason};
		{timeout, Tables} ->
			error_logger:error_report(["Timeout waiting for tables",
					{tables, Tables}]),
			{error, timeout}
	end.
%% @hidden
install3(Nodes, Acc) ->
	case mnesia:create_table(snmp_variables, [{disc_copies, Nodes}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new SNMP variables table.~n"),
			install4(Nodes, [snmp_variables | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, snmp_variables}} ->
			error_logger:info_msg("Found existing SNMP variables table.~n"),
			install4(Nodes, [snmp_variables | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install4(Nodes, Acc) ->
	case mnesia:create_table(alarmModelTable, [{disc_copies, Nodes},
			{attributes, record_info(fields, alarmModelTable)},
			{snmp, [{key, {string, integer, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new alarm model table.~n"),
			install5(Nodes, [alarmModelTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, alarmModelTable}} ->
			error_logger:info_msg("Found existing alarm model table.~n"),
			install5(Nodes, [alarmModelTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install5(Nodes, Acc) ->
	case mnesia:create_table(alarmActiveTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, alarmActiveTable)},
			{snmp, [{key, {string, string, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new active alarm table.~n"),
			install6(Nodes, [alarmActiveTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, alarmActiveTable}} ->
			error_logger:info_msg("Found existing active alarm table.~n"),
			install6(Nodes, [alarmActiveTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install6(Nodes, Acc) ->
	case mnesia:create_table(alarmActiveVariableTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, alarmActiveVariableTable)},
			{snmp, [{key, {string, integer, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new active alarm variables table.~n"),
			install7(Nodes, [alarmActiveVariableTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, alarmActiveVariableTable}} ->
			error_logger:info_msg("Found existing active alarm variables table.~n"),
			install7(Nodes, [alarmActiveVariableTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install7(Nodes, Acc) ->
	case mnesia:create_table(alarmActiveStatsTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, alarmActiveStatsTable)},
			{snmp, [{key, string}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new active alarm statistics table.~n"),
			install8(Nodes, [alarmActiveStatsTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, alarmActiveStatsTable}} ->
			error_logger:info_msg("Found existing active alarm statistics table.~n"),
			install8(Nodes, [alarmActiveStatsTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install8(Nodes, Acc) ->
	case mnesia:create_table(alarmClearTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, alarmClearTable)},
			{snmp, [{key, {string, string, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new cleared alarm table.~n"),
			install9(Nodes, [alarmClearTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, alarmClearTable}} ->
			error_logger:info_msg("Found existing cleared alarm table.~n"),
			install9(Nodes, [alarmClearTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install9(Nodes, Acc) ->
	case mnesia:create_table(ituAlarmTable, [{disc_copies, Nodes},
			{attributes, record_info(fields, ituAlarmTable)},
			{snmp, [{key, {string, integer, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new ITU alarm model table.~n"),
			install10(Nodes, [ituAlarmTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, ituAlarmTable}} ->
			error_logger:info_msg("Found existing ITU alarm model table.~n"),
			install10(Nodes, [ituAlarmTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install10(Nodes, Acc) ->
	case mnesia:create_table(ituAlarmActiveTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, ituAlarmActiveTable)},
			{snmp, [{key, {string, string, integer}}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new ITU active alarm table.~n"),
			install11(Nodes, [ituAlarmActiveTable | Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, ituAlarmActiveTable}} ->
			error_logger:info_msg("Found existing ITU active alarm table.~n"),
			install11(Nodes, [ituAlarmActiveTable | Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install11(Nodes, Acc) ->
	case mnesia:create_table(ituAlarmActiveStatsTable, [{ram_copies, Nodes},
			{attributes, record_info(fields, ituAlarmActiveStatsTable)},
			{snmp, [{key, string}]}]) of
		{atomic, ok} ->
			error_logger:info_msg("Created new ITU active alarm statistics table.~n"),
			install12(Nodes, [ituAlarmActiveStatsTable| Acc]);
		{aborted, {not_active, _, Node} = Reason} ->
			error_logger:error_report(["Mnesia not started on node",
					{node, Node}]),
			{error, Reason};
		{aborted, {already_exists, ituAlarmActiveStatsTable}} ->
			error_logger:info_msg("Found existing ITU active alarm statistics table.~n"),
			install12(Nodes, [ituAlarmActiveStatsTable| Acc]);
		{aborted, Reason} ->
			error_logger:error_report([mnesia:error_description(Reason),
				{error, Reason}]),
			{error, Reason}
	end.
%% @hidden
install12(_Nodes, Acc) ->
	F = fun() ->
			mnesia:write({snmp_variables, alarmActiveIndex, 0}),
			mnesia:write({snmp_variables, alarmModelIndex, 0})
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{ok, Acc};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec force(Tables) -> Result
	when
		Tables :: [TableName],
		Result :: ok | {error, Reason},
		TableName :: atom(),
		Reason :: term().
%% @doc Try to force load bad tables.
force([H | T]) ->
	case mnesia:force_load_table(H) of
		yes ->
			force(T);
		ErrorDescription ->
			{error, ErrorDescription}
		end;
force([]) ->
	ok.

