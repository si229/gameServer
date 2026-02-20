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
-include("game_net.hrl").
-include("common.hrl").
-define(HEARTBEAT_KICK_OUT_TIMEOUT, 120 * 1000).
-define(HEARTBEAT_MAX_TIMEOUT, 3).
%% API
-export([init/2]).

-export([websocket_handle/2, websocket_init/1, websocket_info/2]).


init(Req, Opts) ->
    IpBin = get_ip([<<"cdn-src-ip">>, <<"x-forwarded-for">>], Req),
    {cowboy_websocket, Req, {IpBin, Opts}}.

websocket_init({Ip, _}) ->
    process_flag(trap_exit, true),
    {ok, #game_net_state{ip = Ip, last_heartbeat = erlang:system_time(1000)}}.

websocket_handle({binary, Binary}, State) ->
    case game_msg:dispatch_msg(Binary, State) of
        {ok, RespBinary, NewState} ->
            send_msg(RespBinary, NewState);
        {ok, NewState} -> {ok, NewState};
        {stop, Reason, RespBinary, NewState} ->
            ?WARNING("-- ~p", [Reason]),
            send_stop_msg(RespBinary, NewState);
        {stop, Reason, NewState} ->
            ?WARNING("-- ~p", [Reason]),
            {stop, NewState}
    end;
websocket_handle({text, _Binary}, State) ->
    {reply, {text, <<"notsupport">>}, State};
websocket_handle({ping, Binary}, State) ->
    {reply, {pong, Binary}, State};
websocket_handle(Data, State) ->
    ?WARNING("-- unknown ~p", [Data]),
    {ok, State}.


websocket_info({send, []}, State) ->
    {ok, State};
websocket_info({send, Binary}, State) ->
    send_msg(Binary, State);
websocket_info(check_tick, #game_net_state{last_heartbeat = LastTime, timeout_times = TimeoutTimes} = State) ->
    case LastTime + ?HEARTBEAT_KICK_OUT_TIMEOUT >= erlang:system_time(1000) of
        true when TimeoutTimes > ?HEARTBEAT_MAX_TIMEOUT ->
            {stop, State};
        true ->
            {stop, State#game_net_state{timeout_times = TimeoutTimes + 1}};
        false ->
            {ok, State#game_net_state{timeout_times = 0}}
    end;
websocket_info({stop, BinaryList}, State) ->
    send_stop_msg(BinaryList, State);
websocket_info(stop, State) ->
    {stop, State};
websocket_info(Msg, State) ->
    ?WARNING("-- unknown ~p", [Msg]),
    {stop, State}.

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

send_msg([], State) -> {ok, State};
send_msg(RespBinary, State) when is_list(RespBinary) ->
    RespBinaryList = [{binary, Binary} || Binary <- RespBinary],
    {reply, RespBinaryList, State};
send_msg(RespBinary, State) ->
    {reply, [{binary, RespBinary}], State}.

send_stop_msg(RespBinary, State) ->
    RespBinaryList = [{binary, Binary} || Binary <- RespBinary],
    {reply, RespBinaryList ++ [{close, 1000, <<>>}], State}.
