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

-record(state, {account, ws_pid, user}).

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
            {ok, #state{account = Account, ws_pid = WsPid, user = game_user:load(Account)}};
        _ ->
            {stop, normal}
    end.

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info({msg, _Msg}, State = #state{}) ->
    {noreply, State};
handle_info({settle, Msg, {?GUEST, Profit}}, State = #state{ws_pid = WsPid,
    user = #user{type = ?GUEST, bonus_credits = BonusCredits} = User}) ->
    WsPid ! {send, Msg},
    NewUser = User#user{bonus_credits = BonusCredits + Profit},
    {noreply, State#state{user = NewUser}};
handle_info({settle, Msg, {?NORMAL, Profit}}, State = #state{ws_pid = WsPid
    , user = #user{real_money = RealMoney} = User}) ->
    WsPid ! {send, Msg},
    NewUser = User#user{real_money = RealMoney + Profit},
    game_user:save(NewUser),
    {noreply, State#state{user = NewUser}};
handle_info({send, Msg}, State = #state{ws_pid = WsPid}) ->
    WsPid ! {send, Msg},
    {noreply, State};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


