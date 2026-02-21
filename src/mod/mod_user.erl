%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 11月 2025 下午 9:34
%%%-------------------------------------------------------------------
-module(mod_user).
-include("game_net.hrl").
-include("msg.hrl").
-include("common.hrl").
%% API
-export([login/2]).


login(#{<<"msg_id">> := <<"login_req">>, <<"account">> := Account, <<"password">> := Password}, #game_net_state{} = State) ->
    ?INFO("# ~p", [{Account, Password}]),
    case supervisor:start_child(user_sup, [Account, self()]) of
        {ok, Pid} ->
            Msg = game_proto_util:login_resp(Account, 1000, none),
            {ok, Msg, State#game_net_state{account = Account, pid = Pid}};
        R ->
            ?WARNING("# ~p", [R]),
            {ok, State}
    end.

