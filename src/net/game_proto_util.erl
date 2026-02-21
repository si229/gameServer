%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 2月 2026 上午 11:03
%%%-------------------------------------------------------------------
-module(game_proto_util).
-author("si").

%% API
-export([login_resp/3]).


login_resp(Account,Chips,ReconnectInfo)->
    jsx:encode(#{msg_id=>login_resp,account=>Account,chips=>Chips,reconnect_info=>ReconnectInfo}).