%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

%% @doc the http handler to get list of nodes
%% By default you can access to this handler on 
%%      http://host:port/_nodes
%% 
%% Allowed query arguments are :
%%  - feed: normal, longpoll, continuous
%%  - timeout: used to set timeout for longpoll connections (values can
%%    be infinity or an integer
%%  - heartbeat: used for continuous connections (values can be an
%%    integer to set time after when an heartbeat is send (\n), or true if
%%    you want to use the default timeout.
%%
%% Default timeout is set in 
%%    [farmer]
%%    events_timeout = 60
%%
%% Timeout is set in seconds

-module(farmer_httpd).

-include("farmer.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([handle_nodes_req/1]).


handle_nodes_req(#httpd{method='GET'}=Req) ->
    Feed = couch_httpd:qs_value(Req, "feed", "normal"),
    case Feed of
        "normal" ->
            Etag = couch_httpd:make_etag(farmer_nodes:last_update()),
            couch_httpd:etag_respond(Req, Etag, fun() ->
                        {ok, Resp} = couch_httpd:start_json_response(Req, 
                            200, [{"Etag", Etag}]),
                        handle_nodes_updates(Req, Resp, Feed)
            end);
        "longpoll" ->
            {ok, Resp} = couch_httpd:start_json_response(Req, 200),
            handle_nodes_updates(Req, Resp, Feed);
        _ ->
            {ok, Resp} = couch_httpd:start_chunked_response(Req, 200,
                [{"Cache-Control", "must-revalidate"}]),
            handle_nodes_updates(Req, Resp, Feed)
    end;
handle_nodes_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD").

handle_nodes_updates(Req, Resp ,Feed) ->
    Self = self(),

    %% Try first to send old events
    Since = list_to_integer(couch_httpd:qs_value(Req, "since", "0")),    
    case stream_node_event({start, Since}, Resp, Feed) of
        stop ->
            stream_node_event(stop, Resp, Feed);
        _ ->  
            {Timeout, TimeoutFun} = get_events_timeout(Req),
            
            %% start to listen event
            Ref = erlang:make_ref(),
            {ok, Pid} = farmer_nodes_events:start_link(fun(Event) ->
                    Self ! {Ref, Event}
            end),
            
            try
                keep_sending_nodes_events(Resp, Feed, Timeout,
                    TimeoutFun, Ref)
            after
                farmer_nodes_events:stop(Pid)
            end
    end.


get_events_timeout(Req) ->
    DefaultTimeout = list_to_integer(couch_config:get("farmer",
            "events_timeout", "60000")),

    Timeout = couch_httpd:qs_value(Req, "timeout"),
    Heartbeat = couch_httpd:qs_value(Req, "heartbeat"),

    case Heartbeat of
        undefined ->
            case Timeout of
                undefined ->
                    {DefaultTimeout, fun stop_sending_node_events/1};
                "infinity" ->
                    {infinity, fun stop_sending_node_events/1};
                _ -> 
                    {list_to_integer(Timeout), fun stop_sending_node_events/1}
            end;
        "true" ->
            {DefaultTimeout, fun do_hearbeat/1};
        _ ->
            {list_to_integer(Heartbeat), fun do_hearbeat/1}
    end.

stop_sending_node_events(_) ->
    stop.

do_hearbeat(Resp) ->
    couch_httpd:send_chunk(Resp, "\n"),
    ok.

keep_sending_nodes_events(Resp, Feed, Timeout, TimeoutFun, Ref) ->
    receive
        {Ref, Event} ->
            case stream_node_event(Event, Resp, Feed) of
                stop ->
                    stream_node_event(stop, Resp, Feed);
                _->
                    keep_sending_nodes_events(Resp, Feed, Timeout,
                        TimeoutFun, Ref)
            end
    after Timeout ->
            case TimeoutFun(Resp) of
                ok ->
                    keep_sending_nodes_events(Resp, Feed, Timeout,
                        TimeoutFun, Ref);
                stop ->
                    stream_node_event(stop, Resp, Feed)
            end
    end.

stream_node_event({start, Since}, Resp, "continuous") ->
    lists:foreach(fun
            ({_, #node{time=TS}=Node}) when (TS > Since) ->
                send_event(Resp, <<"add">>, Node, "continuous");
            (_) ->
                ok
        end, farmer_nodes:get_nodes()),
    ok;
stream_node_event({start, Since}, Resp, _) ->
    couch_httpd:send_chunk(Resp, "[\n"),
    Prepend= lists:foldl(fun
                ({_, #node{time=TS}=Node}, Prepend1) when (TS > Since) ->
                    send_event(Resp, <<"add">>, Node, "longpoll", Prepend1),
                    "\n,";
                (_Node, Prepend1) ->
                    Prepend1
            end, "", farmer_nodes:get_nodes()),
    case Prepend of
        "" -> ok;
        _ -> stop
    end;
stream_node_event({add_node, Node}, Resp, "continuous") ->
    send_event(Resp, <<"add">>, Node, "continuous"),
    ok;
stream_node_event({add_node, Node}, Resp, Feed) ->
    send_event(Resp, <<"add">>, Node, Feed),
    stop;
stream_node_event({remove_node, Node}, Resp, "continuous") ->
    send_event(Resp, <<"remove">>, Node, "continuous"),
    ok;
stream_node_event({remove_node, Node}, Resp, Feed) ->
    send_event(Resp, <<"remove">>, Node, Feed),
    stop;
stream_node_event(stop, Resp, "continuous") ->
    couch_httpd:last_chunk(Resp);
stream_node_event(stop, Resp, _) ->
    couch_httpd:send_chunk(Resp, "\n]\n"),
    couch_httpd:end_json_response(Resp);

stream_node_event(_, _, _) ->
    ok.

send_event(Resp, Type, Node, Feed) ->
    send_event(Resp, Type, Node, Feed, "").

send_event(Resp, Type, Node, "continuous", _) ->
    Event = make_event(Type, Node),
    couch_httpd:send_chunk(Resp, [?JSON_ENCODE(Event) | "\n"]);
send_event(Resp, Type, Node, "longpoll", Prepend) ->
    Event = make_event(Type, Node),
    couch_httpd:send_chunk(Resp, [Prepend, ?JSON_ENCODE(Event)]);
send_event(_, _, _, _, _) ->
    ok.

make_event(Type, Node) ->
    {[
        {<<"type">>, Type},
        {<<"node">>, node_to_json(Node)}
    ]}.

node_to_json(Node) ->
    node_to_json(Node, []).

node_to_json(Node, Extra) ->
    #node{name=Name, host=Host,port=Port, txt=Txt, time=Time} = Node,
    JsonProps = [{<<"name">>, Name},
                {<<"host">>, Host},
                {<<"port">>, Port},
                {<<"txt">>, {Txt}},
                {<<"time">>, Time}] ++ Extra,

    {JsonProps}.
