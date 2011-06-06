%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.


%% @doc copy from couch_event_sup
%%  Instead calling the
%%  ok = gen_event:add_sup_handler(error_logger, my_log, Args)
%% 
%%  do this:
%%  {ok, LinkedPid} = couch_event_sup:start_link(error_logger, my_log, Args)
%% 
%%  The benefit is the event is now part of the process tree, and can be
%%  started, restarted and shutdown consistently like the rest of the server
%%  components.
%% 
%%  And now if the "event" crashes, the supervisor is notified and can restart
%%  the event handler.
%% 
%%  Use this form to named process:
%%  {ok, LinkedPid} = farmer_event_sup:start_link({local, my_log}, error_logger, 
%%                                          my_log, Args)

-module(farmer_event_sup).
-behaviour(gen_server).

-export([start_link/3,start_link/4, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
        handle_info/2,code_change/3]).


start_link(EventMgr, EventHandler, Args) ->
    gen_server:start_link(couch_event_sup, {EventMgr, EventHandler,
            Args}, []).

start_link(ServerName, EventMgr, EventHandler, Args) ->
    gen_server:start_link(ServerName, couch_event_sup, {EventMgr,
            EventHandler, Args}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init({EventMgr, EventHandler, Args}) ->
    case gen_event:add_sup_handler(EventMgr, EventHandler, Args) of
    ok ->
        {ok, {EventMgr, EventHandler}};
    {stop, Error} ->
        {stop, Error}
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(_Whatever, _From, State) ->
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
    {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
