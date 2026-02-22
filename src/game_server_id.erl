%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2026 上午 11:17
%%%-------------------------------------------------------------------
-module(game_server_id).
-author("si").
-export([start/0, next_id/0]).

-define(EPOCH, 1771730280124). % 自定义起始时间戳 (毫秒)

-record(state, {
    last_ts = 0,
    sequence = 0,
    machine_id = 1
}).

start() ->
    register(?MODULE, spawn(fun() -> loop(#state{}) end)).

next_id() ->
    ?MODULE ! {self(), next},
    receive
        {id, Id} -> Id
    end.

loop(State = #state{last_ts = LastTs, sequence = Seq, machine_id = Mid}) ->
    receive
        {From, next} ->
            Ts = timestamp(),
            {NewSeq, NewTs} =
                case Ts =:= LastTs of
                    true ->
                        Seq1 = (Seq + 1) band 16#FFF, % 12bit 序列号
                        case Seq1 of
                            0 -> wait_next(Ts), {0, timestamp()};
                            _ -> {Seq1, Ts}
                        end;
                    false ->
                        {0, Ts}
                end,
            Id = make_id(NewTs, Mid, NewSeq),
            From ! {id, Id},
            loop(State#state{last_ts = NewTs, sequence = NewSeq})
    end.

timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + (Micro div 1000).

wait_next(Ts) ->
    case timestamp() > Ts of
        true -> ok;
        false -> wait_next(Ts)
    end.

make_id(Ts, Mid, Seq) ->
    ((Ts - ?EPOCH) bsl 22) bor (Mid bsl 12) bor Seq.