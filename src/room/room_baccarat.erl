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
-include("room.hrl").
%% API
-export([handle_state/3]).
-export([phase_info/0,next_state/1,next_state/2]).


handle_state(?preparation, CutOffTime, #room_state{deck = Deck} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    case game_baccarat:try_reshuffle_the_shoe(Deck) of
        {true, NewDeck} ->
            Msg = game_proto_util:phase_change_push(?preparation, DTime, true),
            room_srv:broadcast(Msg, State),
            State#room_state{phase_state = {?preparation, DTime}, deck = NewDeck};
        _ ->
            Msg = game_proto_util:phase_change_push(?preparation, DTime, false),
            room_srv:broadcast(Msg, State),
            State#room_state{phase_state = {?preparation, DTime}}
    end;


handle_state(?dealing, CutOffTime, #room_state{deck = Deck, play_type = ?GUEST} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr},
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, DealInfo, none),
    room_srv:broadcast(Msg, State),
    State#room_state{phase_state = {?dealing, DTime}, deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards};

handle_state(?dealing, CutOffTime, #room_state{deck = Deck} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr},
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, #{hash_value => HashValue}, none),
    room_srv:broadcast(Msg, State),
    State#room_state{phase_state = {?dealing, DTime}, deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards};


handle_state(?betting, CutOffTime, #room_state{} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?betting, DTime, false),
    room_srv:broadcast(Msg, State),
    State#room_state{phase_state = {?betting, DTime}};


handle_state(?settlement, CutOffTime, #room_state{deal_info = DealInfo,
    player_cards = PlayerCards, banker_cards = BankerCards
    , guest_role_list = GuestRoleList, play_type = PlayType,
    normal_role_list = NormalRoleList, game_type = GameType
} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Payout = game_baccarat:payout_calculation(GameType, PlayerCards, BankerCards),
    NewGuestRoleList = lists:map(fun(#room_role{bet_info = BetInfo, bonus_credits = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        Pid ! {settle, Msg, {?GUEST, Profit}},
        Role#room_role{bonus_credits = Chips + Profit}
                                 end, GuestRoleList),

    NewNormalRoleList = lists:map(fun(#room_role{bet_info = BetInfo, real_money = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        Pid ! {settle, Msg, {?NORMAL, Profit}},
        Role#room_role{real_money = Chips + Profit}
                                  end, NormalRoleList),

    room_road:add_road(PlayType, GameType, {[BetZone || {BetZone, _Odds} <- Payout], PlayerCards, BankerCards}),

    State#room_state{phase_state = {?settlement, DTime}, guest_role_list = NewGuestRoleList, normal_role_list = NewNormalRoleList}.


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