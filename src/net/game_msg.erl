%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 11月 2025 下午 9:31
%%%-------------------------------------------------------------------
-module(game_msg).
%% API
-include("game_net.hrl").
-include("msg.hrl").

-export([
    handle_not_login_msg/2,
    handle_game_msg/2
]).

handle_not_login_msg(#{<<"msg_id">> := <<"heartbeat_req">>, <<"id">> := Id}, State) ->
    Time = erlang:system_time(1000),
    RespMsg = jsx:encode(#{msg_id => heartbeat_resp, time => Time, id => Id}),
    {ok, RespMsg, State#game_net_state{last_heartbeat = Time}};

handle_not_login_msg(#{<<"msg_id">> := <<"login_req">>} = Msg, State) ->
    mod_user:login(Msg, State).



handle_game_msg(#{<<"msg_id">> := <<"login_req">>} = Msg, State) ->
    mod_user:login(Msg, State).