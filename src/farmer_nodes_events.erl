%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

%% @doc module to notify events of nodes managed by farmer

-module(farmer_nodes_events).
-behaviour(gen_event).

-include_lib("couch/include/couch_db.hrl").

-export([start_link/1, notify/1, stop/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
        terminate/2, code_change/3]).

start_link(Exec) ->
    farmer_event_sup:start_link(farmer_node_event, {farmer_nodes_events,
            make_ref()}, Exec).

notify(Event) ->
    gen_event:notify(farmer_node_event, Event).

stop(Pid) ->
    farmer_event_sup:stop(Pid).

init(Exec) ->
    {ok, Exec}.

handle_event(Event, Fun) when is_function(Fun, 1) ->
    Fun(Event),
    {ok, Fun};
handle_event(Event, {Fun, Acc}) ->
    Acc1 = Fun(Event, Acc),
    {ok, {Fun, Acc1}};
handle_event(Event, Pid) ->
    Pid ! Event,
    {ok, Pid}.

handle_call(_, Exec) ->
    {ok, ok, Exec}.

handle_info(_, Exec) ->
    {ok, Exec}.

terminate(_Reason, Pid) when is_pid(Pid) ->
    Pid ! stop,
    ok;
terminate(_Reason, _Exec) ->
    ok.

code_change(_OldVsn, Exec, _Extra) ->
    {ok, Exec}.
