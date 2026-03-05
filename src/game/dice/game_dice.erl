%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 3月 2026 下午 10:00
%%%-------------------------------------------------------------------
-module(game_dice).
-author("si").

-include("room.hrl").
-include("dice.hrl").
-include("common.hrl").
%% API
-export([init/0]).
-export([payout_calculation/1]).

-export([settlement/2, deal/0,gen_hash/1]).

init() ->
    supervisor:start_child(room_sup, [?GUEST, ?GAME_TYPE_DICE]),
    supervisor:start_child(room_sup, [?NORMAL, ?GAME_TYPE_DICE]).

deal() ->
    lists:sort([rand:uniform(6) || _ <- lists:seq(1, 3)]).

gen_hash(Points) ->
    RandomStr = game_util:random_string(),
    Timestamp = erlang:system_time(1000),
    Str = lists:flatten([integer_to_list(X) || X <- Points]),
    S = io_lib:format("~p~s~s", [Timestamp, RandomStr, Str]),
    L = list_to_binary(
        S
    ),
    {game_util:sha256_hex(L), L, Timestamp, RandomStr, Str}.

settlement(BetList, Points) ->
    Payout = payout_calculation(Points),
    settlement_do(BetList, Payout).

settlement_do(BetList, Payout) ->
    lists:foldl(fun({BetArea, Amount}, Acc) ->
        Odds = proplists:get_value(BetArea, Payout, -1),
        Amount * Odds + Amount + Acc
                end, 0, BetList).


payout_calculation([Point, Point, Point] = Points) ->
    BettingArea = [
        ?single_number_bet,
        ?pair_bet,
        ?triple_bet,
        ?any_triple_bet,
        ?sum_bet,
        ?combination_bet
    ],
    check_winner_area(BettingArea, Points);

payout_calculation(Points) ->
    BettingArea = [
        ?big,
        ?small,
        ?even_bet,
        ?odd_bet,
        ?single_number_bet,
        ?pair_bet,
        ?triple_bet,
        ?any_triple_bet,
        ?sum_bet,
        ?combination_bet
    ],
    check_winner_area(BettingArea, Points).


check_winner_area([], _Points) -> [];

check_winner_area([?big | BettingArea], Points) ->
    case lists:sum(Points) >= 11 of
        true ->
            [{?big, odds(?big)} | check_winner_area(BettingArea, Points)];
        false ->
            check_winner_area(BettingArea, Points)
    end;

check_winner_area([?small | BettingArea], Points) ->
    case lists:sum(Points) < 11 of
        true ->
            [{?small, odds(?small)} | check_winner_area(BettingArea, Points)];
        false ->
            check_winner_area(BettingArea, Points)
    end;

check_winner_area([?odd_bet | BettingArea], Points) ->
    case lists:sum(Points) rem 2 of
        1 ->
            [{?odd_bet, odds(?odd_bet)} | check_winner_area(BettingArea, Points)];
        _ ->
            check_winner_area(BettingArea, Points)
    end;

check_winner_area([?even_bet | BettingArea], Points) ->
    case lists:sum(Points) rem 2 of
        0 ->
            [{?even_bet, odds(?even_bet)} | check_winner_area(BettingArea, Points)];
        _ ->
            check_winner_area(BettingArea, Points)
    end;

check_winner_area([?single_number_bet | BettingArea], [P, P, P] = Points) ->
    [{{?single_number_bet, P}, odds({?single_number_bet, 3})} | check_winner_area(BettingArea, Points)];
check_winner_area([?single_number_bet | BettingArea], [P, P, P2] = Points) ->
    [{{?single_number_bet, P}, odds({?single_number_bet, 2})},
        {{?single_number_bet, P2}, odds({?single_number_bet, 1})} |
        check_winner_area(BettingArea, Points)];
check_winner_area([?single_number_bet | BettingArea], [P1, P, P] = Points) ->
    [{{?single_number_bet, P}, odds({?single_number_bet, 2})}
        , {{?single_number_bet, P1}, odds({?single_number_bet, 1})} |
        check_winner_area(BettingArea, Points)];
check_winner_area([?single_number_bet | BettingArea], [P1, P2, P3] = Points) ->
    [{{?single_number_bet, P1}, odds({?single_number_bet, 1})},
        {{?single_number_bet, P2}, odds({?single_number_bet, 1})},
        {{?single_number_bet, P3}, odds({?single_number_bet, 1})} |
        check_winner_area(BettingArea, Points)];
check_winner_area([?pair_bet | BettingArea], [P, P, P] = Points) ->
    [{{?pair_bet, P}, odds(?pair_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?pair_bet | BettingArea], [_, P, P] = Points) ->
    [{{?pair_bet, P}, odds(?pair_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?pair_bet | BettingArea], [P, P, _] = Points) ->
    [{{?pair_bet, P}, odds(?pair_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?pair_bet | BettingArea], Points) ->
    check_winner_area(BettingArea, Points);
check_winner_area([?triple_bet | BettingArea], [P, P, P] = Points) ->
    [{{?triple_bet, P}, odds(?triple_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?triple_bet | BettingArea], Points) ->
    check_winner_area(BettingArea, Points);
check_winner_area([?any_triple_bet | BettingArea], [P, P, P] = Points) ->
    [{?any_triple_bet, odds(?any_triple_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?any_triple_bet | BettingArea], Points) ->
    check_winner_area(BettingArea, Points);
check_winner_area([?sum_bet | BettingArea], Points) ->
    SumPoint = lists:sum(Points),
    [{{?sum_bet, SumPoint}, odds({?sum_bet, SumPoint})} | check_winner_area(BettingArea, Points)];
check_winner_area([?combination_bet | BettingArea], [P, P, P] = Points) ->
    check_winner_area(BettingArea, Points);
check_winner_area([?combination_bet | BettingArea], [P1, P, P] = Points) ->
    [{{?combination_bet, {P1, P}}, odds(?combination_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?combination_bet | BettingArea], [P, P, P1] = Points) ->
    [{{?combination_bet, {P, P1}}, odds(?combination_bet)} | check_winner_area(BettingArea, Points)];
check_winner_area([?combination_bet | BettingArea], [P1, P2, P3] = Points) ->
    [{{?combination_bet, {P1, P2}}, odds(?combination_bet)}
        , {{?combination_bet, {P1, P3}}, odds(?combination_bet)},
        {{?combination_bet, {P2, P3}}, odds(?combination_bet)} | check_winner_area(BettingArea, Points)].



odds(?big) -> 1;
odds(?small) -> 1;
odds({?single_number_bet, 1}) -> 1;
odds({?single_number_bet, 2}) -> 2;
odds({?single_number_bet, 3}) -> 3;
odds(?pair_bet) -> 8;
odds(?triple_bet) -> 150;
odds(?any_triple_bet) -> 24;
odds({?sum_bet, 4}) -> 50;
odds({?sum_bet, 17}) -> 50;
odds({?sum_bet, 5}) -> 18;
odds({?sum_bet, 16}) -> 18;
odds({?sum_bet, 6}) -> 14;
odds({?sum_bet, 15}) -> 14;
odds({?sum_bet, 7}) -> 12;
odds({?sum_bet, 14}) -> 12;
odds({?sum_bet, 8}) -> 8;
odds({?sum_bet, 13}) -> 8;
odds({?sum_bet, 9}) -> 6;
odds({?sum_bet, 12}) -> 6;
odds({?sum_bet, 10}) -> 6;
odds({?sum_bet, 11}) -> 6;
odds(?combination_bet) -> 5;
odds(?odd_bet) -> 1;
odds(?even_bet) -> 1.


