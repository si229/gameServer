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

-export([pid/1]).

-define(SERVER, ?MODULE).
-include("room.hrl").
-include("msg.hrl").
-include("common.hrl").
-define(ROOM_PID(ID), {room_pid, ID}).

-record(state, {id, type, guest_role_list = [], normal_role_list = [], loop_timer_ref, phase_state}).

-define(START_TIMER(), erlang:start_timer(500, self(), loop_timer)).


pid(Id) ->
    gproc:lookup_local_name(?ROOM_PID(Id)).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link(Id, Type) ->
    gen_server:start_link(?MODULE, [Id, Type], []).

init([Id, Type]) ->
    case catch gproc:add_local_name(?ROOM_PID(Id)) of
        true ->
            erlang:process_flag(trap_exit, true),
            {ok, #state{id = Id, type = Type, loop_timer_ref = ?START_TIMER()}};
        _ ->
            {stop, normal}
    end.

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
handle_loop(#state{ phase_state = undefined} = State) ->
    {Phase, CutOffTime} = hd(phase_info()),
    handle_state(Phase, CutOffTime, State);
handle_loop(#state{ phase_state = undefined} = State) ->
    {Phase, CutOffTime} = hd(phase_info()),
    handle_state(Phase, CutOffTime, State).

handle_state(?preparation, CutOffTime, #state{} = State) ->
    Msg = game_proto_util:phase_change_push(?preparation,CutOffTime,none,none),
    broadcast(Msg,State),
    State#state{phase_state = {?preparation,CutOffTime+?MILLI_TIMESTAMP}};

handle_state(?dealing, CutOffTime, #state{} = State) ->

    Msg = game_proto_util:phase_change_push(?dealing,CutOffTime,none,none),
    broadcast(Msg,State),
    State#state{phase_state = {?preparation,CutOffTime+?MILLI_TIMESTAMP}}.



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