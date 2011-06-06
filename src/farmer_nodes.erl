%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

-module(farmer_nodes).
-behaviour(gen_server).

-include("farmer.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([start_link/0]).

-export([get_nodes/0, last_update/0]).


%% gen_server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        nodes=dict:new(),
        events_pid,
        last_update=erlang:now()  
}).

get_nodes() ->
    gen_server:call(?MODULE, nodes, infinity).

last_update() ->
    gen_server:call(?MODULE, last_update).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Self = self(),
    {ok, Pid} = farmer_nodes_events:start_link(Self),
    {ok, #state{events_pid=Pid}}.

handle_call(nodes, _From, #state{nodes=Nodes}=State) ->
    NodesList = dict:to_list(Nodes),
    {reply, NodesList, State};
handle_call(last_update, _From, #state{last_update=Last}=State) ->
    {reply, Last, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add_node, #node{host=Host, port=Port}=Node}, #state{nodes=Nodes}=State) ->
    ?LOG_INFO("add: ~p~n", [Node]),

    Nodes1 = dict:store({Host, Port}, Node, Nodes),
    {noreply, State#state{nodes=Nodes1, last_update=erlang:now()}};
handle_info({remove_node, #node{host=Host, port=Port}}, #state{nodes=Nodes}=State) ->
    Nodes1 = dict:erase({Host, Port}, Nodes),
    {noreply, State#state{nodes=Nodes1, last_update=erlang:now()}};
handle_info(_Msg, State) ->
    ?LOG_INFO("unknown msg: ~p~n", [_Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State#state{nodes=dict:new()}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
