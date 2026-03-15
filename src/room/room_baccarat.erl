%%%-------------------------------------------------------------------
%%% @author nm_jok
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 3月 2026 14:24
%%%-------------------------------------------------------------------
-module(room_baccarat).
-author("nm_jok").
-include("msg.hrl").
-include("common.hrl").
-include("baccarat.hrl").
-include("room.hrl").
%% API
-export([handle_state/3]).
-export([phase_info/0, next_state/1, next_state/2]).
-export([handle_message/2]).

-export([init/1]).


init(#room_state{} = State) ->
    State#room_state{game_state = #game_state{}}.

handle_state(?preparation, CutOffTime, #room_state{game_state = #game_state{deck = Deck} = GameState} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    RoundId = game_server_id:next_id(),
    case game_baccarat:try_reshuffle_the_shoe(Deck) of
        {true, NewDeck} ->
            Msg = game_proto_util:phase_pre_push(?preparation, DTime, RoundId, true),
            room_srv:broadcast(Msg, State),
            State#room_state{round_id = RoundId, phase_state = {?preparation, DTime}, game_state = GameState#game_state{deck = NewDeck}};
        _ ->
            Msg = game_proto_util:phase_pre_push(?preparation, DTime, RoundId, false),
            room_srv:broadcast(Msg, State),
            State#room_state{round_id = RoundId, phase_state = {?preparation, DTime}}
    end;


handle_state(?dealing, CutOffTime, #room_state{
    game_state = #game_state{deck = Deck} = GameState
    , play_type = ?GUEST} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr, player_cards => PlayerCards, banker_cards => BankerCards},


    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, DealInfo, none),
    room_srv:broadcast(Msg, State),
    NewGameState = GameState#game_state{deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards},
    State#room_state{phase_state = {?dealing, DTime}, game_state = NewGameState};

handle_state(?dealing, CutOffTime, #room_state{game_state = #game_state{deck = Deck} = GameState} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr},
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, #{hash_value => HashValue}, none),
    room_srv:broadcast(Msg, State),
    NewGameState = GameState#game_state{deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards},
    State#room_state{phase_state = {?dealing, DTime}, game_state = NewGameState};


handle_state(?betting, CutOffTime, #room_state{} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?betting, DTime, false),
    room_srv:broadcast(Msg, State),
    State#room_state{phase_state = {?betting, DTime}};


handle_state(?settlement, CutOffTime, #room_state{game_state =
#game_state{deal_info = DealInfo, player_cards = PlayerCards, banker_cards = BankerCards, deck = Deck}
    , guest_role_list = GuestRoleList, play_type = PlayType,
    normal_role_list = NormalRoleList, game_type = GameType
} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Payout = game_baccarat:payout_calculation(GameType, PlayerCards, BankerCards),
    NewGuestRoleList = lists:map(fun(#room_role{bet_info = BetInfo, bonus_credits = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        Pid ! {settle, Msg, {?GUEST, Profit}},
        Role#room_role{bonus_credits = Chips + Profit, bet_info = []}
                                 end, GuestRoleList),

    NewNormalRoleList = lists:map(fun(#room_role{bet_info = BetInfo, real_money = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        Pid ! {settle, Msg, {?NORMAL, Profit}},
        Role#room_role{real_money = Chips + Profit, bet_info = []}
                                  end, NormalRoleList),
    room_road:add_road(PlayType, GameType, {[BetZone || {BetZone, _Odds} <- Payout], PlayerCards, BankerCards}),
    State#room_state{phase_state = {?settlement, DTime}
        , guest_role_list = NewGuestRoleList
        , normal_role_list = NewNormalRoleList
        , game_state = #game_state{deck = Deck}
    }.

handle_message({bet, {Account, ?GUEST, Zone, Amount}}
    , State = #room_state{guest_role_list = GuestRoleList
        , game_state = #game_state{bet_info = RoomBetInfo} = GameState
    }) ->
    case lists:keytake(Account, #room_role.account, GuestRoleList) of
        {value, #room_role{bet_info = BetInfo} = Role, LGuestRoleList} ->
            NewBetAmount = proplists:get_value(Zone, BetInfo, 0) + Amount,
            NewRoomBetAmount = proplists:get_value(Zone, RoomBetInfo, 0) + Amount,
            NewBetInfo = lists:keystore(Zone, 1, BetInfo, {Zone, NewBetAmount}),
            NewRoomBetInfo = lists:keystore(Zone, 1, RoomBetInfo, {Zone, NewRoomBetAmount}),
            lists:foreach(fun(#room_role{pid = Pid, account = A, bet_info = RoleBetInfo}) ->
                if A =/= Account ->
                    RBetAmount = proplists:get_value(Zone, RoleBetInfo, 0),
                    SMsg = game_proto_util:bet(false, Amount, Zone, RBetAmount, NewRoomBetAmount, ?ok),
                    Pid ! {send, SMsg};
                    true ->
                        Msg = game_proto_util:bet(true, Amount, Zone, NewBetAmount, NewRoomBetAmount, ?ok),
                        ?WARNING("## ~p", [{Account, Amount, NewBetAmount, NewRoomBetAmount}]),
                        Pid ! {send, Msg}
                end
                          end, GuestRoleList),
            {reply, ok, State#room_state{
                guest_role_list = [Role#room_role{bet_info = NewBetInfo} | LGuestRoleList],
                game_state = GameState#game_state{bet_info = NewRoomBetInfo}
            }};
        _ ->
            {reply, ok, State}
    end;

handle_message({bet, {Account, ?NORMAL, Zone, Amount}}
    , State = #room_state{normal_role_list = NormalRoleList
        , game_state = #game_state{bet_info = RoomBetInfo} = GameState
    }) ->
    case lists:keytake(Account, #room_role.account, NormalRoleList) of
        {value, #room_role{bet_info = BetInfo} = Role, LGuestRoleList} ->
            NewBetAmount = proplists:get_value(Zone, BetInfo, 0) + Amount,
            NewRoomBetAmount = proplists:get_value(Zone, RoomBetInfo, 0) + Amount,
            NewBetInfo = lists:keystore(Zone, 1, BetInfo, {Zone, NewBetAmount}),
            NewRoomBetInfo = lists:keystore(Zone, 1, RoomBetInfo, {Zone, NewRoomBetAmount}),
            lists:foreach(fun(#room_role{pid = Pid, account = A, bet_info = RoleBetInfo}) ->
                if A =/= Account ->
                    RBetAmount = proplists:get_value(Zone, RoleBetInfo, 0),
                    SMsg = game_proto_util:bet(false, Amount, Zone, RBetAmount, NewRoomBetAmount, ?ok),
                    Pid ! {send, SMsg};
                    true ->
                        Msg = game_proto_util:bet(true, Amount, Zone, NewBetAmount, NewRoomBetAmount, ?ok),
                        Pid ! {send, Msg}
                end
                          end, NormalRoleList),
            {reply, ok, State#room_state{
                normal_role_list = [Role#room_role{bet_info = NewBetInfo} | LGuestRoleList],
                game_state = GameState#game_state{bet_info = NewRoomBetInfo}
            }};
        _ ->
            {reply, ok, State}
    end;

handle_message(_Msg, State) -> {reply, ok, State}.


phase_info() ->
    [{?preparation, 2000}, {?dealing, 1000}, {?betting, 15000}, {?settlement, 500}].


next_state(?settlement) ->
    PhaseInfo = phase_info(),
    {?preparation, proplists:get_value(?preparation, PhaseInfo)};


next_state(Phase) ->
    PhaseInfo = phase_info(),
    next_state(Phase, PhaseInfo).

next_state(Phase, [{Phase, _}, Next | _]) -> Next;
next_state(Phase, [_ | Next]) -> next_state(Phase, Next).