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
-export([dispatch_msg/2]).

dispatch_msg(#{<<"msg_type">> := <<"login_req">>} = Msg, State) ->
    mod_user:login(Msg, State).