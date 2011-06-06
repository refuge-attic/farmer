%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

-module(farmer_dnssd).
-behaviour(gen_server).

-include("farmer.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        local_only,
        service_name,
        reg_ref,
        browse_ref}).
    
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, Ref} = dnssd:browse("_http._tcp,_refuge"),
    LocalOnly = is_local_only(),
    State = #state{local_only = LocalOnly, browse_ref = Ref},
    init(State);
init(#state{local_only = true} = State) ->
    {ok, State};
init(#state{local_only = false} = State) ->
    ServiceName = service_name(),
    {ok, Ref} = dnssd:register(ServiceName, "_http._tcp,_refuge",
			       httpd_port(), [{path, "/"}]),
    NewState = State#state{service_name=ServiceName, reg_ref = Ref},
    {ok, NewState}.

handle_call(list_services, _From, #state{} = State) ->
    {ok, RegResults} = dnssd:results(State#state.reg_ref),
    {ok, BrowseResults} = dnssd:results(State#state.browse_ref),
    Services = BrowseResults -- RegResults,
    Reply = {ok, Services},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info({dnssd, Ref, {browse, add, Result}}, #state{browse_ref = Ref}=State) ->
    spawn_link(fun() ->
                maybe_notify(add_node, Result)
        end),
    {noreply, State};
handle_info({dnssd, Ref, {browse, remove, Result}}, #state{browse_ref = Ref}=State) ->
    spawn_link(fun() ->
                maybe_notify(remove_node, Result)
        end),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{browse_ref=BrowseRef, reg_ref=RegRef}) ->
    ok = dnssd:stop(BrowseRef),
    ok = dnssd:stop(RegRef),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

maybe_notify(Type, Result) ->
    case make_node_info(Result) of
        {error, _} -> ok;
        Node ->
            ?LOG_INFO(?MODULE_STRING " ~p ~p~n", [Type, Node]),
            farmer_nodes_events:notify({Type, Node})
    end.


make_node_info({Name, Type, Domain}=Result) ->
    case dnssd:resolve_sync(Name, Type, Domain) of
        {ok, {Host, Port, Txt}} ->
            #node{name=Name, host=Host, port=Port, txt=Txt,
                time=farmer_util:get_unix_timestamp(erlang:now())};
        Error ->
            ?LOG_ERROR("error resolving ~p : ~p", [Result, Error]),
            Error
    end.


is_local_only() ->
    case couch_config:get(<<"httpd">>, <<"bind_address">>) of
	"127." ++ _ -> true;
	"::1" -> true;
	_ -> false
    end.

httpd_port() ->
    PortStr = couch_config:get(<<"httpd">>, <<"port">>),
    list_to_integer(PortStr).

service_name() ->
    case couch_config:get(<<"refuge">>, <<"name">>) of
        ServiceName when is_list(ServiceName) andalso
        length(ServiceName) < 64 ->
            ServiceName;
        _ ->
            Name = build_service_name(),
            couch_config:set("refuge", "name", Name),
            Name
    end.

build_service_name() ->
    Prefix = case username() of
        {ok, Username} ->
            case lists:reverse(Username) of
                "s" ++ _ -> Username ++ "' refuge on ";
                _ -> Username ++ "'s refuge on "
            end;
        _ -> "refuge on "
    end,
    PrefixLen = length(Prefix),
    case inet:gethostname() of
        {ok, Hostname} ->
            HostnameLen = length(Hostname),
            if HostnameLen + PrefixLen < 64 ->
                    Prefix ++ Hostname;
                true -> ""
            end;
        _ -> ""
    end.

username() ->
    case os:getenv("USER") of
        Username when is_list(Username) ->
            {ok, Username};
        _ ->
            case os:getenv("USERNAME") of
                Username when is_list(Username) ->
                    {ok, Username};
                _ ->
                    undefined
            end
    end.






    

