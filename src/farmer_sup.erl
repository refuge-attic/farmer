%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

-module(farmer_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {farmer_dnssd, 
            {farmer_dnssd, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [farmer_dnssd]},
        {farmer_node_event_sup,
            {gen_event, start_link, [{local, farmer_node_event}]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
        {farmer_nodes,
            {farmer_nodes, start_link, []},
            permanent,
            1000,
            worker,
            [farmer_nodes]}
        ],
    {ok, {{one_for_one, 10, 3600}, Children}}.

