%%% -*- erlang -*-
%%%
%%% This file is part of farmer released under the MIT license. 
%%% See the NOTICE for more information.

-module(farmer_util).

-export([ipv6_supported/0, get_addrs/1,
        address_to_binary/2, nodeid/2,
        get_unix_timestamp/1]).

-include_lib("couch/include/couch_db.hrl").

%% @doc get_unix_timestamp
%% @spec
%% @output
get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

%% @doc is ipv6 supported on this machine ?
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} ->
            true;
        {error, _} ->
            false
    end.

%% @doc get ip addresses of an host
get_addrs(Host) when is_binary(Host) ->
    get_addrs(binary_to_list(Host));
get_addrs(Host) ->
    BindAddress = couch_config:get("httpd", "bind_address", any),
    {ok, Ip} = inet_parse:address(BindAddress),
    Protos = case Ip of
        any ->
            case ipv6_supported() of 
                true -> [inet, inet6];
                _ -> [inet] 
            end;
        {_, _, _, _} -> % IPv4
            [inet];
        {_, _, _, _, _, _, _, _} -> % IPv6
            [inet6]
    end,
    
    ?LOG_INFO("host ~p~n", [Host]),
    lists:foldl(fun(Proto, Acc) ->
        case (catch inet:getaddr(Host, Proto)) of
            {ok, Addr} ->
                [Addr|Acc];
            {error, _} ->
                Acc
        end
    end, [], Protos).

%% @doc convert an address to its binary representation
address_to_binary({_, _, _, _}=Ip, Port) ->
    iolist_to_binary([inet_parse:ntoa(Ip), ":", integer_to_list(Port)]);
address_to_binary({_,_,_,_,_,_,_,_}=Ip, Port) ->
    iolist_to_binary(["[", inet_parse:ntoa(Ip), "]:",
            integer_to_list(Port)]).

%% @doc create id from Ip and Port
nodeid(Ip, Port) ->
    crypto:sha(address_to_binary(Ip, Port)).
