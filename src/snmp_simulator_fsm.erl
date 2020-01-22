%%% snmp_simulator_fsm.erl
%%% vim: ts=3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2020 SigScale Global Inc.
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements an alarm generator in the
%%% 	(@link //snmp_simulator. snmp_simulator} application.
-module(snmp_simulator_fsm).
-copyright('Copyright (c) 2020 SigScale Global Inc.').

-behaviour(gen_fsm).
-include("snmp_simulator.hrl").

%% export the public API
-export([]).

%% export the snmp_simulator_fsm states
-export([transaction/2]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

-record(statedata,
		{active :: pos_integer(),
		mean :: pos_integer(),
		deviation :: 0..100,
		alarms :: pos_integer(),
		prefix :: snmpa:oid()}).

%% support deprecated_time_unit()
-define(MILLISECOND, milli_seconds).
%-define(MILLISECOND, millisecond).

%%----------------------------------------------------------------------
%%  The snmp_simulator_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

-spec init(Args) -> Result
	when
		Args :: list(),
		Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason} | ignore,
		StateName :: atom(),
		StateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init(_Args) ->
	{ok, Active} = application:get_env(active),
	{ok, Mean} = application:get_env(mean),
	{ok, Deviation} = application:get_env(deviation),
	{ok, Prefix} = application:get_env(prefix),
	F = fun() ->
			mnesia:read(snmp_variables, alarmModelIndex, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [{_, _, Alarms}]} ->
			{ok, transaction, #statedata{active = Active, mean = Mean,
					deviation = Deviation, alarms = Alarms,
					prefix = prefix(Prefix)}, rand:uniform(4000)};
		{aborted, Reason} ->
			{stop, Reason}
	end.

-spec transaction(Event, StateData) -> Result
	when
		Event :: timeout | term(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%%		gen_fsm:send_event/2} in the <b>transaction</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
transaction(timeout, #statedata{active = MaxActive} = StateData) ->
	Start = erlang:system_time(?MILLISECOND),
	F = fun() ->
			mnesia:read(alarmActiveStatsTable, [], read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#alarmActiveStatsTable{alarmActiveStatsActiveCurrent = Active}]}
				when Active < MaxActive ->
			case model(StateData) of
				{ok, Model} ->
					snmp_simulator:add_alarm(Model, resource(StateData)),
					{next_state, transaction, StateData, timeout(Start, StateData)};
				{error, Reason} ->
					{stop, Reason, StateData}
			end;
		{atomic, [#alarmActiveStatsTable{}]} ->
			{ok, AlarmIndex} = mnesia:snmp_get_next_index(alarmActiveTable, []),
			snmp_simulator:clear_alarm(AlarmIndex),
			{next_state, transaction, StateData, timeout(Start, StateData)};
		{aborted, Reason} ->
			{stop, Reason, StateData}
	end.

-spec handle_event(Event, StateName, StateData) -> Result
	when
		Event :: term(),
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason , NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec handle_sync_event(Event, From, StateName, StateData) -> Result
	when
		Event :: term(),
		From :: {Pid :: pid(), Tag :: term()},
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
		Reply :: term(),
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

-spec handle_info(Info, StateName, StateData) -> Result
	when
		Info :: term(),
		StateName :: atom(),
		StateData :: #statedata{},
		Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
		NextStateName :: atom(),
		NewStateData :: #statedata{},
		Timeout :: non_neg_integer() | infinity,
		Reason :: normal | term().
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(_info, StateName, StateData) ->
	{next_state, StateName, StateData}.

-spec terminate(Reason, StateName, StateData) -> any()
	when
		Reason :: normal | shutdown | term(),
		StateName :: atom(),
		StateData :: #statedata{}.
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate(_Reason1, _StateName,  _StateData) ->
	ok.

-spec code_change(OldVsn, StateName, StateData, Extra) -> Result
	when
		OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		StateName :: atom(),
		StateData :: #statedata{},
		Extra :: term(),
		Result :: {ok, NextStateName :: atom(), NewStateData :: #statedata{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
prefix(Prefix) when is_list(Prefix) ->
	prefix(Prefix, []).
%% @hidden
prefix([H | T], Acc) when is_atom(H) ->
	{value, Prefix} = snmpa:name_to_oid(H),
	prefix(T, [Prefix | Acc]);
prefix([H | T], Acc) when is_list(H) ->
	prefix(T, [H | Acc]);
prefix([], Acc) ->
	lists:reverse(Acc).

-spec model(StateData) -> Result
	when
		StateData :: #statedata{},
		Result :: {ok, Model} | {error, Reason},
		Model :: snmpa:oid(),
		Reason :: term().
%% @doc Randomly choose an alarm model.
%% @hidden
model(#statedata{alarms = Alarms}) ->
	N = rand:uniform(Alarms),
	F = fun() ->
			MatchSpec = [{#alarmModelTable{key = '$1', _ = '_'},
					[{'=:=', {element, 2, '$1'}, N},
					{'>', {element, 3, '$1'}, 1}], ['$1']}],
			mnesia:select(alarmModelTable, MatchSpec, read)
	end,
	case mnesia:transaction(F) of
		{atomic, Keys} ->
			{ok, lists:nth(rand:uniform(length(Keys)), Keys)};
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec resource(StateData) -> Resource
	when
		StateData :: #statedata{},
		Resource :: snmpa:oid().
%% @doc Returns a resource OID with one of the configured
%% 	prefixes and a random index.
%% @hidden
resource(#statedata{prefix = Prefix, active = Active}) ->
	lists:nth(rand:uniform(length(Prefix)), Prefix) ++ [rand:uniform(Active)].

-spec timeout(Start, StateData) -> Timeout
	when
		Start :: pos_integer(),
		StateData :: #statedata{},
		Timeout :: pos_integer().
%% @doc Returns a timeout taking into account the time it took to
%% 	process the current transaction, the configured `mean' rate
%% 	and random `deviation' percentage.
%% @hidden
timeout(Start, #statedata{mean = Mean, deviation = Deviation}) ->
	End = erlang:system_time(?MILLISECOND),
	case (1000 div Mean) - (End - Start) of
		Interval when Interval > 0 ->
			case (Interval * Deviation) div 100 of
				Range when Range > 0 ->
					Interval + (rand:uniform(Range * 2) - Range);
				_Range ->
					0
			end;
		_Interval ->
			0
	end.

