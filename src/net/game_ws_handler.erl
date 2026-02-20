%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 11月 2025 下午 8:12
%%%-------------------------------------------------------------------
-module(game_ws_handler).
-author("si").
-behavior(cowboy_websocket).
-include("game_net.hrl").

%% API
-export([init/2]).


init(Req, Opts) ->
    IpBin = get_ip([<<"cdn-src-ip">>, <<"x-forwarded-for">>], Req),
    {cowboy_websocket, Req, {IpBin, Opts}}.


websocket_init({Ip,_}) ->
    {ok,#game_net_state{ip = Ip}}.

websocket_handle({binary,Binary}, State) ->

    skip.

websocket_info(Msg, State) ->
    skip.






get_ip([HeaderName | Headers], Req) ->
    case cowboy_req:header(HeaderName, Req) of
        undefined ->
            get_ip(Headers, Req);
        XRealIP ->
            case binary:split(XRealIP, [<<",">>, <<" ">>], [global, trim_all]) of
                [FIP | _] -> FIP;
                _ ->
                    get_ip(Headers, Req)
            end
    end;

get_ip([], Req) ->
    {{A, B, C, D}, _} = cowboy_req:peer(Req),
    list_to_binary(lists:concat(lists:join(".", [A, B, C, D]))).