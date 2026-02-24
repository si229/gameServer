%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(user_srv).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([pid/1]).

-define(SERVER, ?MODULE).
-include("user.hrl").
-include("common.hrl").
-define(USER_PID(ID), {user_pid, ID}).

-define(LOOP_TIMER(), erlang:start_timer(1000, self(), loop_timer)).
-define(TIMEOUT_OFFLINE, 120 * 1000).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User, WsPid) ->
    gen_server:start_link(?MODULE, [User, WsPid], []).

pid(Account) ->
    gproc:lookup_local_name(?USER_PID(Account)).

init([#user{account = Account} = User, WsPid]) ->
    case catch gproc:add_local_name(?USER_PID(Account)) of
        true ->
            erlang:process_flag(trap_exit, true),
            erlang:monitor(process, WsPid),
            {ok, #user_state{account = Account
                , ws_pid = WsPid
                , user = User
                , timer_ref = ?LOOP_TIMER()
            }};
        _ ->
            {stop, normal}
    end.

handle_call(Request, From, State) ->
    try
        handle_call_do(Request, From, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {reply, error, State}
    end.

handle_cast(Request, State) ->
    try
        handle_cast_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.

handle_info(Request, State) ->
    try
        handle_info_do(Request, State)
    catch
        E:E1:E2 ->
            ?WARNING("~p", [{E, E1, E2}]),
            {noreply, State}
    end.

handle_call_do(_Request, _From, State = #user_state{}) ->
    {reply, ok, State}.

handle_cast_do(_Request, State = #user_state{}) ->
    {noreply, State}.

handle_info_do({reconnect, WsPid}, #user_state{} = State) ->
    {noreply, State#user_state{ws_pid = WsPid}};
handle_info_do({msg, Msg}, State = #user_state{ws_pid = WsPid}) ->
    case game_msg:handle_game_msg(Msg, State) of
        {ok, #user_state{} = NewState} ->
            {noreply, NewState};
        {ok, RespMsg, #user_state{} = NewState} ->
            send_msg(WsPid, RespMsg),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
handle_info_do({settle, Msg, {?GUEST, Profit}}, State = #user_state{ws_pid = WsPid,
    user = #user{bonus_credits = BonusCredits} = User}) ->
    send_msg(WsPid, Msg),
    NewUser = User#user{bonus_credits = BonusCredits + Profit},
    {noreply, State#user_state{user = NewUser}};
handle_info_do({settle, Msg, {?NORMAL, Profit}}, State = #user_state{ws_pid = WsPid
    , user = #user{real_money = RealMoney} = User}) ->
    send_msg(WsPid, Msg),
    NewUser = User#user{real_money = RealMoney + Profit},
    game_user:save(NewUser),
    {noreply, State#user_state{user = NewUser}};
handle_info_do({send, Msg}, State = #user_state{ws_pid = WsPid}) ->
    send_msg(WsPid, Msg),
    {noreply, State};

handle_info_do({'DOWN', _Ref, process, WsPid, Reason}, State = #user_state{ws_pid = WsPid, account = Account}) ->
    ?WARNING("# offline  ~p", [{Account, Reason}]),
    {noreply, State#user_state{ws_pid = erlang:system_time(1000)}};


handle_info_do({timeout, _, loop_timer}, State = #user_state{}) ->
    case handle_loop(State) of
        stop ->
            {stop, normal, State};
        #user_state{} = NewState ->
            ?LOOP_TIMER(),
            {noreply, NewState};
        _ ->
            ?LOOP_TIMER(),
            {noreply, State}
    end;

handle_info_do(_Info, State = #user_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #user_state{}) ->
    ok.

code_change(_OldVsn, State = #user_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_loop(#user_state{ws_pid = T} = State) when is_integer(T) ->
    Now = erlang:system_time(1000),
    case T + ?TIMEOUT_OFFLINE >= Now of
        true ->
            stop;
        _ ->
            State
    end;
handle_loop(State) -> State.


send_msg(WsPid, Msg) ->
    case is_pid(WsPid) andalso is_process_alive(WsPid) of
        true ->
            WsPid ! {send, Msg};
        _ ->
            false
    end.