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
-record(state, {account, ws_pid, user, timer_ref}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Account, WsPid) ->
    gen_server:start_link(?MODULE, [Account, WsPid], []).

pid(Account) ->
    gproc:lookup_local_name(?USER_PID(Account)).

init([Account, WsPid]) ->
    case catch gproc:add_local_name(?USER_PID(Account)) of
        true ->
            erlang:process_flag(trap_exit, true),
            erlang:monitor(process, WsPid),
            {ok, #state{account = Account
                , ws_pid = WsPid
                , user = game_user:load(Account)
                , timer_ref = ?LOOP_TIMER()
            }};
        _ ->
            {stop, normal}
    end.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({reconnect, WsPid}, #state{} = State) ->
    {noreply, State#state{ws_pid = WsPid}};
handle_info({msg, Msg}, State = #state{ws_pid = WsPid}) ->
    case game_msg:handle_game_msg(Msg, State) of
        {ok, #state{} = NewState} ->
            {noreply, NewState};
        {ok, Msg, #state{} = NewState} ->
            send_msg(WsPid, Msg),
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
handle_info({settle, Msg, {?GUEST, Profit}}, State = #state{ws_pid = WsPid,
    user = #user{type = ?GUEST, bonus_credits = BonusCredits} = User}) ->
    send_msg(WsPid, Msg),
    NewUser = User#user{bonus_credits = BonusCredits + Profit},
    {noreply, State#state{user = NewUser}};
handle_info({settle, Msg, {?NORMAL, Profit}}, State = #state{ws_pid = WsPid
    , user = #user{real_money = RealMoney} = User}) ->
    send_msg(WsPid, Msg),
    NewUser = User#user{real_money = RealMoney + Profit},
    game_user:save(NewUser),
    {noreply, State#state{user = NewUser}};
handle_info({send, Msg}, State = #state{ws_pid = WsPid}) ->
    send_msg(WsPid, Msg),
    {noreply, State};

handle_info({'DOWN', _Ref, process, WsPid, Reason}, State = #state{ws_pid = WsPid, account = Account}) ->
    ?WARNING("# offline  ~p", [{Account, Reason}]),
    {noreply, State#state{ws_pid = erlang:system_time(1000)}};


handle_info({timeout, _, loop_timer}, State = #state{}) ->
    case handle_loop(State) of
        stop ->
            {stop, normal, State};
        #state{} = NewState ->
            ?LOOP_TIMER(),
            {noreply, NewState};
        _ ->
            ?LOOP_TIMER(),
            {noreply, State}
    end;

handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_loop(#state{ws_pid = T} = State) when is_integer(T) ->
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