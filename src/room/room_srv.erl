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

-export([pid/1, enter/5, leave/3, bet/6]).

-export([update_bonus_chips/4, update_real_chips/4]).


-export([broadcast/2]).

-define(SERVER, ?MODULE).
-include("room.hrl").
-include("msg.hrl").
-include("common.hrl").
-define(ROOM_PID(ID), {room_pid, ID}).


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

bet(Account, PlayType, GameType, Mode, Zone, Amount) ->
    ?WARNING("## ~p", [{Account, PlayType, GameType, Mode, Zone, Amount}]),
    gen_server:call(pid({PlayType, GameType}), {bet, {Account, Mode, Zone, Amount}}).

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
            RoomMod = room_mod(Type, GameType),
            RoomState = #room_state{play_type = Type, loop_timer_ref = ?LOOP_TIMER()
                , game_type = GameType, room_mod = RoomMod},
            {ok, RoomMod:init(RoomState)};
        _ ->
            {stop, normal}
    end.

handle_call(Request, From, State = #room_state{}) ->
    try
        handle_call_do(Request, From, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {reply, error, State}
    end.

handle_cast(Request, State = #room_state{}) ->
    try
        handle_cast_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.

handle_info(Request, State = #room_state{}) ->
    try
        handle_info_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.

room_mod(_PlayType, ?GAME_TYPE_BACCARAT_LUCKY) ->
    room_baccarat;
room_mod(_PlayType, ?GAME_TYPE_BACCARAT_CLASSIC) ->
    room_baccarat;
room_mod(_PlayType, ?GAME_TYPE_AMERICAN_ROULETTE) ->
    room_roulette;
room_mod(_PlayType, ?GAME_TYPE_FRENCH_ROULETTE) ->
    room_roulette;
room_mod(_PlayType, ?GAME_TYPE_DICE) ->
    room_dice.

handle_call_do({enter, #room_role{account = Account, pid = Pid} = R}, _From
    , State = #room_state{play_type = ?GUEST, guest_role_list = GuestRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = Role, LGuestRoleList} ->
                               [Role#room_role{pid = Pid} | LGuestRoleList];
                           _ ->
                               [R | GuestRoleList]
                       end,
    {reply, ok, State#room_state{guest_role_list = NewGuestRoleList}};

handle_call_do({enter, #room_role{account = Account, pid = Pid} = R}, _From
    , State = #room_state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
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
    {reply, ok, State#room_state{guest_role_list = NewGuestRoleList, normal_role_list = NewNormalRoleList}};


handle_call_do({leave, Account}, _From
    , State = #room_state{guest_role_list = GuestRoleList, normal_role_list = NormalRoleList}) ->
    case lists:keytake(Account, #room_role.account, NormalRoleList) of
        {value, #room_role{bet_info = BetInfo}, LNormalRoleList} when BetInfo =/= [] ->
            {reply, fail, State};
        {value, #room_role{bet_info = BetInfo}, LNormalRoleList} ->
            {reply, ok, State#room_state{
                guest_role_list = lists:keydelete(Account, #room_role.account, GuestRoleList)
                , normal_role_list = LNormalRoleList}};
        _ ->
            {reply, ok, State}
    end;


handle_call_do({bet, _} = Msg, _From
    , State = #room_state{room_mod = RoomMod}) ->
    RoomMod:handle_message(Msg, State);

handle_call_do(_Request, _From, State = #room_state{}) ->
    {reply, ok, State}.


handle_cast_do({update_real_chips, #room_role{real_money = RealMoney, account = Account}}, State = #room_state{
    normal_role_list = NormalRoleList}) ->
    NewNormalRoleList = case lists:keytake(Account, #room_role.account, NormalRoleList) of
                            {value, #room_role{} = Role, LNormalRoleList} ->
                                [Role#room_role{real_money = RealMoney} | LNormalRoleList];
                            _ ->
                                NormalRoleList
                        end,
    {noreply, State#room_state{normal_role_list = NewNormalRoleList}};
handle_cast_do({update_bonus_chips, #room_role{bonus_credits = BonusCredits, account = Account}}
    , State = #room_state{guest_role_list = GuestRoleList}) ->
    NewGuestRoleList = case lists:keytake(Account, #room_role.account, GuestRoleList) of
                           {value, #room_role{} = Role, LGuestRoleList} ->
                               [Role#room_role{bonus_credits = BonusCredits} | LGuestRoleList];
                           _ ->
                               GuestRoleList
                       end,
    {noreply, State#room_state{guest_role_list = NewGuestRoleList}};
handle_cast_do(_Request, State = #room_state{}) ->
    {noreply, State}.

handle_info_do({timeout, Ref, loop_timer}, State = #room_state{loop_timer_ref = Ref}) ->
    NewState = handle_loop(State),
    {noreply, NewState#room_state{loop_timer_ref = ?LOOP_TIMER()}};
handle_info_do(_Info, State = #room_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #room_state{}) ->
    ok.

code_change(_OldVsn, State = #room_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_loop(#room_state{phase_state = undefined, room_mod = RoomMod} = State) ->
    {Phase, CutOffTime} = hd(RoomMod:phase_info()),
    RoomMod:handle_state(Phase, CutOffTime, State);
handle_loop(#room_state{phase_state = {Phase, CutOffTime}, room_mod = RoomMod} = State) ->
    Now = ?MILLI_TIMESTAMP,
    if CutOffTime =< Now ->
        {NewPhase, NewCutOffTime} = RoomMod:next_state(Phase),
        RoomMod:handle_state(NewPhase, NewCutOffTime, State);
        true ->
            State
    end.



broadcast(Msg, #room_state{play_type = ?GUEST, guest_role_list = GuestRoleList}) ->
    lists:foreach(fun(#room_role{pid = Pid}) ->
        Pid ! {send, Msg}
                  end, GuestRoleList);
broadcast(Msg, #room_state{normal_role_list = NormalRoleList}) ->
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