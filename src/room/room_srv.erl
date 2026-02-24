%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(room_srv).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([pid/1, enter/5, leave/3]).

-export([update_bonus_chips/4, update_real_chips/4]).

-define(SERVER, ?MODULE).
-include("room.hrl").
-include("msg.hrl").
-include("common.hrl").
-define(ROOM_PID(ID), {room_pid, ID}).

-record(state, {id, type, guest_role_list = [], game_type
    , normal_role_list = [], loop_timer_ref, phase_state
    , deck = [], player_cards, banker_cards, hash_value
    , deal_info
}).

-define(LOOP_TIMER(), erlang:start_timer(500, self(), loop_timer)).


pid(Id) ->
    gproc:lookup_local_name(?ROOM_PID(Id)).

enter(Account, PlayType, GameType, BonusCredits, RealMoney) ->
    Role = #room_role{account = Account
        , pid = self()
        , bonus_credits = BonusCredits
        , real_money = RealMoney
    },
    ?WARNING("## ~p", [{Account, PlayType, GameType}]),
    gen_server:call(pid({PlayType, GameType}), {enter, Role}).

leave(Account, PlayType, GameType) ->
    ?WARNING("## ~p", [{Account, PlayType, GameType}]),
    gen_server:call(pid({PlayType, GameType}), {leave, Account}).

update_real_chips(Account, Type, GameType, Chips) ->
    Role = #room_role{account = Account, pid = self(), real_money = Chips},
    gen_server:cast(pid({Type, GameType}), {update_real_chips, Role}).

update_bonus_chips(Account, Type, GameType, Chips) ->
    Role = #room_role{account = Account, pid = self(), bonus_credits = Chips},
    gen_server:cast(pid({Type, GameType}), {update_bonus_chips, Role}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link(Type, GameType) ->
    gen_server:start_link(?MODULE, [Type, GameType], []).

init([Type, GameType]) ->
    case catch gproc:add_local_name(?ROOM_PID({Type, GameType})) of
        true ->
            erlang:process_flag(trap_exit, true),
            {ok, #state{type = Type, loop_timer_ref = ?LOOP_TIMER(), game_type = GameType}};
        _ ->
            {stop, normal}
    end.

handle_call(Request, From, State = #state{}) ->
    try
        handle_call_do(Request, From, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {reply, error, State}
    end.

handle_cast(Request, State = #state{}) ->
    try
        handle_cast_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.

handle_info(Request, State = #state{}) ->
    try
        handle_info_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.


handle_call_do({enter, #room_role{account = Account, pid = Pid} = R}, _From
    , State = #state{type = ?GUEST, guest_role_list = GuestRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = Role, LGuestRoleList} ->
                               [Role#room_role{pid = Pid} | LGuestRoleList];
                           _ ->
                               [R | GuestRoleList]
                       end,
    {reply, ok, State#state{guest_role_list = NewGuestRoleList}};

handle_call_do({enter, #room_role{account = Account, pid = Pid} = R}, _From
    , State = #state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = GRole, LGuestRoleList} ->
                               [GRole#room_role{pid = Pid} | LGuestRoleList];
                           _ ->
                               [R | GuestRoleList]
                       end,
    NewNormalRoleList = case lists:keytake(Account, #room_role.account, NormalRoleList) of
                            {value, #room_role{} = RRole, LNormalRoleList} ->
                                [RRole#room_role{pid = Pid} | LNormalRoleList];
                            _ ->
                                [R | NormalRoleList]
                        end,
    {reply, ok, State#state{guest_role_list = NewGuestRoleList, normal_role_list = NewNormalRoleList}};


handle_call_do({leave, Account}, _From
    , State = #state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
    case lists:keytake(Account, #room_role.account, NormalRoleList) of
        {value, #room_role{bet_info = BetInfo}, LNormalRoleList} when BetInfo =/= [] ->
            {reply, fail, State};
        {value, #room_role{bet_info = BetInfo}, LNormalRoleList} ->
            {reply, ok, State#state{
                guest_role_list = lists:keydelete(Account, #room_role.account, GuestRoleList)
                , normal_role_list = LNormalRoleList}};
        _ ->
            {reply, ok, State}
    end;


handle_call_do(_Request, _From, State = #state{}) ->
    {reply, ok, State}.


handle_cast_do({update_real_chips, #room_role{real_money = RealMoney, account = Account}}, State = #state{
    normal_role_list = NormalRoleList}) ->
    NewNormalRoleList = case lists:keytake(Account, #room_role.account, NormalRoleList) of
                            {value, #room_role{} = Role, LNormalRoleList} ->
                                [Role#room_role{real_money = RealMoney} | LNormalRoleList];
                            _ ->
                                NormalRoleList
                        end,
    {noreply, State#state{normal_role_list = NewNormalRoleList}};
handle_cast_do({update_bonus_chips, #room_role{bonus_credits = BonusCredits, account = Account}}
    , State = #state{guest_role_list = GuestRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = Role, LGuestRoleList} ->
                               [Role#room_role{bonus_credits = BonusCredits} | LGuestRoleList];
                           _ ->
                               GuestRoleList
                       end,
    {noreply, State#state{guest_role_list = NewGuestRoleList}};
handle_cast_do(_Request, State = #state{}) ->
    {noreply, State}.

handle_info_do({timeout, Ref, loop_timer}, State = #state{loop_timer_ref = Ref}) ->
    NewState = handle_loop(State),
    {noreply, NewState#state{loop_timer_ref = ?LOOP_TIMER()}};
handle_info_do(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_loop(#state{phase_state = undefined} = State) ->
    {Phase, CutOffTime} = hd(phase_info()),
    handle_state(Phase, CutOffTime, State);
handle_loop(#state{phase_state = {Phase, CutOffTime}} = State) ->
    Now = ?MILLI_TIMESTAMP,
    if CutOffTime =< Now ->
        {NewPhase, NewCutOffTime} = next_state(Phase),
        handle_state(NewPhase, NewCutOffTime, State);
        true ->
            State
    end.

handle_state(?preparation, CutOffTime, #state{deck = Deck} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    case game_baccarat:try_reshuffle_the_shoe(Deck) of
        {true, NewDeck} ->
            Msg = game_proto_util:phase_change_push(?preparation, DTime, true),
            broadcast(Msg, State),
            State#state{phase_state = {?preparation, DTime}, deck = NewDeck};
        _ ->
            Msg = game_proto_util:phase_change_push(?preparation, DTime, false),
            broadcast(Msg, State),
            State#state{phase_state = {?preparation, DTime}}
    end;


handle_state(?dealing, CutOffTime, #state{deck = Deck, type = ?GUEST} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr},
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, DealInfo, none),
    broadcast(Msg, State),
    State#state{phase_state = {?dealing, DTime}, deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards};

handle_state(?dealing, CutOffTime, #state{deck = Deck} = State) ->
    {PlayerCards, BankerCards, NewDeck} = game_baccarat:deal(Deck),
    {HashValue, Str, Timestamp, RandomStr, CardStr} = game_baccarat:gen_hash(PlayerCards, BankerCards),
    DealInfo = #{hash_value => HashValue, str => Str, timestamp => Timestamp
        , random_str => RandomStr, card_str => CardStr},
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?dealing, DTime, false, #{hash_value => HashValue}, none),
    broadcast(Msg, State),
    State#state{phase_state = {?dealing, DTime}, deck = NewDeck, deal_info = DealInfo
        , player_cards = PlayerCards, banker_cards = BankerCards};


handle_state(?betting, CutOffTime, #state{} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Msg = game_proto_util:phase_change_push(?betting, DTime, false),
    broadcast(Msg, State),
    State#state{phase_state = {?betting, DTime}};


handle_state(?settlement, CutOffTime, #state{deal_info = DealInfo,
    player_cards = PlayerCards, banker_cards = BankerCards
    , guest_role_list = GuestRoleList,
    normal_role_list = NormalRoleList, game_type = GameType
} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Payout = game_baccarat:payout_calculation(GameType, PlayerCards, BankerCards),

    if length(GuestRoleList) =/= 0 ->
        ?INFO("## ~p", [{?settlement, length(GuestRoleList), length(GuestRoleList)}]);
        true ->
            skip
    end,
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

    State#state{phase_state = {?settlement, DTime}, guest_role_list = NewGuestRoleList, normal_role_list = NewNormalRoleList}.


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


broadcast(Msg, #state{type = ?GUEST, guest_role_list = GuestRoleList}) ->
    lists:foreach(fun(#room_role{pid = Pid}) ->
        Pid ! {send, Msg}
                  end, GuestRoleList);
broadcast(Msg, #state{normal_role_list = NormalRoleList}) ->
    lists:foreach(fun(#room_role{pid = Pid}) ->
        Pid ! {send, Msg}
                  end, NormalRoleList).

%%broadcast(SelfMsg, OtherMsg, Account, #state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
%%    lists:foreach(fun(#room_role{pid = Pid, account = A}) ->
%%        if A =/= Account ->
%%            Pid ! {send, OtherMsg};
%%            true ->
%%                Pid ! {send, SelfMsg}
%%        end
%%                  end, GuestRoleList),
%%    lists:foreach(fun(#room_role{pid = Pid, account = A}) ->
%%        if A =/= Account ->
%%            Pid ! {send, OtherMsg};
%%            true ->
%%                Pid ! {send, SelfMsg}
%%        end
%%                  end, NormalRoleList).