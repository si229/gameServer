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

-export([pid/1, enter/4]).

-define(SERVER, ?MODULE).
-include("room.hrl").
-include("msg.hrl").
-include("common.hrl").
-define(ROOM_PID(ID), {room_pid, ID}).

-record(state, {id, type, guest_role_list = []
    , normal_role_list = [], loop_timer_ref, phase_state
    , deck = [], player_cards, banker_cards, hash_value
    , deal_info
}).

-define(START_TIMER(), erlang:start_timer(500, self(), loop_timer)).


pid(Id) ->
    gproc:lookup_local_name(?ROOM_PID(Id)).

enter(Account, Type, Chips, Id) ->
    Role = #room_role{account = Account, pid = self(), type = Type, chips = Chips},
    gen_server:call(pid(Id), {enter, Role}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link(Id, Type) ->
    gen_server:start_link(?MODULE, [Id, Type], []).

init([Id, Type]) ->
    case catch gproc:add_local_name(?ROOM_PID({Id, Type})) of
        true ->
            erlang:process_flag(trap_exit, true),
            {ok, #state{id = Id, type = Type, loop_timer_ref = ?START_TIMER()}};
        _ ->
            {stop, normal}
    end.

handle_call({enter, #room_role{account = Account, pid = Pid} = R}, _From
    , State = #state{type = ?GUEST, guest_role_list = GuestRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = Role, LGuestRoleList} ->
                               [Role#room_role{pid = Pid} | LGuestRoleList];
                           _ ->
                               [R | GuestRoleList]
                       end,
    {reply, ok, State#state{guest_role_list = NewGuestRoleList}};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.



handle_info({timeout, Ref, loop_timer}, State = #state{loop_timer_ref = Ref}) ->
    NewState = handle_loop(State),
    {noreply, NewState#state{loop_timer_ref = ?START_TIMER()}};
handle_info(_Info, State = #state{}) ->
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
        ?INFO("# ~p",[Phase]),
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
            Msg = game_proto_util:phase_change_push(?preparation, CutOffTime, false),
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
    normal_role_list = NormalRoleList, type = Type
} = State) ->
    DTime = CutOffTime + ?MILLI_TIMESTAMP,
    Payout = game_baccarat:payout_calculation(PlayerCards, BankerCards),
    NewGuestRoleList = lists:map(fun(#room_role{bet_info = BetInfo, chips = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        Pid ! {send, Msg},
        Role#room_role{chips = Chips + Profit}
                                 end, GuestRoleList),

    NewNormalRoleList = lists:map(fun(#room_role{bet_info = BetInfo, chips = Chips, pid = Pid} = Role) ->
        Profit = game_baccarat:settlement(BetInfo, Payout),
        Msg = game_proto_util:phase_change_push(?settlement, DTime, false, DealInfo, Profit),
        if Type == ?NORMAL ->
            Pid ! {settle, Msg, Profit};
            true ->
                Pid ! {send, Msg}
        end,
        Role#room_role{chips = Chips + Profit}
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


broadcast(Msg, #state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
    lists:foreach(fun(#room_role{pid = Pid}) ->
        Pid ! {send, Msg}
                  end, GuestRoleList),
    lists:foreach(fun(#room_role{pid = Pid}) ->
        Pid ! {send, Msg}
                  end, NormalRoleList).

broadcast(SelfMsg, OtherMsg, Account, #state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
    lists:foreach(fun(#room_role{pid = Pid, account = A}) ->
        if A =/= Account ->
            Pid ! {send, OtherMsg};
            true ->
                Pid ! {send, SelfMsg}
        end
                  end, GuestRoleList),
    lists:foreach(fun(#room_role{pid = Pid, account = A}) ->
        if A =/= Account ->
            Pid ! {send, OtherMsg};
            true ->
                Pid ! {send, SelfMsg}
        end
                  end, NormalRoleList).