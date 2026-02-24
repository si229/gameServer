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
-include("common.hrl").

%% API
-export([enter_room/2
    , leave_room/2
    , bet/2
]).


enter_room(#{<<"msg_id">> := <<"enter_room_req">>
    , <<"play_type">> := PlayType, <<"game_type">> := GameType},
    #user_state{account = Account, play_type = PlayType, game_type = GameType, user = User} = UserState) ->
    case room_srv:enter(Account, PlayType, GameType, User#user.bonus_credits, User#user.real_money) of
        ok ->
            Msg = game_proto_util:enter_room(?ok),
            {ok, Msg, UserState};
        _ ->
            Msg = game_proto_util:enter_room(?fail),
            {ok, Msg, UserState}
    end;

enter_room(#{<<"msg_id">> := <<"enter_room_req">>
    , <<"play_type">> := PlayType, <<"game_type">> := GameType},
    #user_state{account = Account, play_type = RPlayType, game_type = RGameType, user = User} = UserState)
    when is_integer(RPlayType) ->
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
    end;

enter_room(#{<<"msg_id">> := <<"enter_room_req">>
    , <<"play_type">> := PlayType, <<"game_type">> := GameType},
    #user_state{account = Account, user = User} = UserState)
    ->
    case room_srv:enter(Account, PlayType, GameType, User#user.bonus_credits, User#user.real_money) of
        ok ->
            Msg = game_proto_util:enter_room(?ok),
            ?WARNING("## ~p", [{Account, PlayType, GameType}]),
            {ok, Msg, UserState#user_state{play_type = PlayType, game_type = GameType}};
        _ ->
            Msg = game_proto_util:enter_room(?fail),
            {ok, Msg, UserState}
    end.

leave_room(#{<<"msg_id">> := <<"leave_room_req">>},
    #user_state{account = Account, play_type = PlayType, game_type = GameType} = UserState) ->
    case room_srv:leave(Account, PlayType, GameType) of
        ok ->
            Msg = game_proto_util:leave_room(?ok),
            {ok, Msg, UserState#user_state{play_type = undefined, game_type = undefined}};
        _ ->
            Msg = game_proto_util:leave_room(?fail),
            {ok, Msg, UserState}
    end.

bet(#{<<"msg_id">> := <<"betting_req">>
    , <<"amount">> := Amount, <<"zone">> := Zone
    , <<"mode">> := Mode
}, #user_state{
    account = Account, user = #user{}, play_type = PlayType, game_type = GameType
} = UserState) ->
    case room_srv:bet(Account, PlayType, GameType, Mode, Zone, Amount) of
        ok ->
            {ok, UserState};
        _ ->
            Msg = game_proto_util:bet(true, Amount, ?fail),
            {ok, Msg, UserState}
    end.

