%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 2月 2026 下午 3:47
%%%-------------------------------------------------------------------
-module(mod_room).
-author("si").
-include("user.hrl").
-include("msg.hrl").

%% API
-export([enter_room/2]).


enter_room(#{<<"msg_id">> := <<"enter_room_req">>
    , <<"play_type">> := PlayType, <<"game_type">> := GameType},
    #user_state{account = Account, play_type = PlayType, game_type = GameType, user = User} = User) ->
    case room_srv:enter(Account, PlayType, GameType, User#user.bonus_credits, User#user.real_money) of
        ok ->
            Msg = game_proto_util:enter_room(?ok),
            {ok, Msg, User};
        _ ->
            Msg = game_proto_util:enter_room(?fail),
            {ok, Msg, User}
    end;

enter_room(#{<<"msg_id">> := <<"enter_room_req">>
    , <<"play_type">> := PlayType, <<"game_type">> := GameType},
    #user_state{account = Account, play_type = RPlayType, game_type = RGameType, user = User} = UserState) ->

    case room_srv:leave(Account, RPlayType, RGameType) of
        ok ->
            case room_srv:enter(Account, PlayType, GameType, User#user.bonus_credits, User#user.real_money) of
                ok ->
                    Msg = game_proto_util:enter_room(?ok),
                    {ok, Msg, UserState#user_state{play_type = PlayType, game_type = GameType}};
                _ ->
                    Msg = game_proto_util:enter_room(?fail),
                    {ok, Msg, UserState}
            end;
        _ ->
            Msg = game_proto_util:enter_room(?fail),
            {ok, Msg, UserState}
    end.

