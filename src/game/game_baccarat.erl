-module(game_baccarat).
-include("card.hrl").
-include("common.hrl").
-include("msg.hrl").
-define(SHOE_SIZE, 8). %% 1 shoe 8副牌
-define(SHOE_MIN_NUM, 52). %% 小于52要重新换牌


-export([deal/1, try_reshuffle_the_shoe/1]).
-export([init/0]).

-export([gen_hash/2, payout_calculation/2, settlement/2]).

init() ->
    Deck = ?DECK,
    lists:foldl(fun(_, Acc) ->
        Deck ++ Acc
                end, Deck, lists:seq(2, ?SHOE_SIZE)).

try_reshuffle_the_shoe(Deck) ->
    case length(Deck) =< ?SHOE_MIN_NUM of
        true ->
            {true, init()};
        false ->
            false
    end.


deal([PlayerCard1, PlayerCard2, BankerCard1, BankerCard2 | Res]) ->
    PlayerCard = [PlayerCard1, PlayerCard2],
    BankerCard = [BankerCard1, BankerCard2],
    PlayerPoint = get_point(PlayerCard),
    BankerPoint = get_point(BankerCard),
    if PlayerPoint >= 8 orelse BankerPoint >= 8 ->
        {PlayerCard, BankerCard, Res};
        true ->
            {IsPlayerPick, NewPlayerCard, PlayerPickPoint, NewRes} =
                if PlayerPoint >= 6 -> {false, PlayerCard, get_point([PlayerCard2]), Res};
                    true ->
                        [PPickCard | Res1] = Res,
                        {true, PlayerCard ++ [PPickCard], get_point([PPickCard]), Res1}
                end,
            if BankerPoint == 7 -> {NewPlayerCard, BankerCard, NewRes};
                BankerPoint == 6 ->
                    if IsPlayerPick == true andalso (PlayerPickPoint == 6 orelse PlayerPickPoint == 7) ->
                        [BPickCard | Res2] = NewRes,
                        {NewPlayerCard, BankerCard ++ [BPickCard], Res2};
                        true -> {NewPlayerCard, BankerCard, NewRes}
                    end;
                BankerPoint == 5 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [0, 1, 2, 3, 8, 9]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;

                BankerPoint == 4 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [0, 1, 8, 9]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;

                BankerPoint == 3 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [8]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;
                true ->
                    [BPickCard | Res2] = NewRes,
                    {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
            end
    end.


get_point(CardList) ->
    lists:foldl(fun({Point, _}, Acc) ->
        if Point =< 9 -> Acc + Point;
            true -> Point
        end
                end, 0, CardList) rem 10.


gen_hash(PlayerCards, BankerCards) ->
    RandomStr = random_string(),
    Cards = PlayerCards ++ BankerCards,
    Points = [P || {P, _} <- Cards],
    Colors = [C || {_, C} <- Cards],
    Timestamp = erlang:system_time(1000),

    Str = lists:flatten([
        begin
            if is_integer(X) ->
                integer_to_list(X);
                true ->
                    X
            end
        end
        || X <- Points ++ ["Y"] ++ Colors

    ]),
    S = io_lib:format("~p~s~s", [Timestamp, RandomStr, Str]),
    L = list_to_binary(
        S
    ),
    {sha256_hex(L), L, Timestamp, RandomStr, Str}.


random_string() ->
    Len = 8,
    Charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    Bytes = crypto:strong_rand_bytes(Len),
    list_to_binary(
        [lists:nth((X rem length(Charset)) + 1, Charset) || <<X>> <= Bytes]
    ).

sha256_hex(InputBin) ->
    Hash = sha256(InputBin),
    lists:flatten([io_lib:format("~2.16.0B", [X]) || <<X>> <= Hash]).

sha256(InputBin) ->
    crypto:hash(sha256, InputBin).


settlement(BetList, Payout) ->
    lists:foldl(fun({BetArea, Amount}, Acc) ->
        Odds = proplists:get_value(BetArea, Payout),
        if Odds == 0 ->
            Acc;
            true ->
                Amount * Odds + Amount + Acc
        end end, 0, BetList).

payout_calculation(PlayerCards, BankerCards) ->
    PlayerPoint = get_point(PlayerCards),
    BankerPoint = get_point(BankerCards),
    PlayerCardNum = length(PlayerCards),
    BankerCardNum = length(BankerCards),
    AreaList = [?lucky_7, ?super_lucky_7, ?lucky_6, ?lucky_6_2, ?lucky_6_3,
        ?banker_pair, ?player_pair, ?banker, ?player, ?tie],
    lists:map(fun(Area) ->
        Odds = check_winner_area(PlayerPoint, BankerPoint, PlayerCardNum
            , BankerCardNum, PlayerCards, BankerCards, Area),
        {Area, Odds}
              end, AreaList).

check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, BankerCardNum, _PlayerCards, _BankerCards, ?lucky_7) ->
    if BankerPoint > PlayerPoint andalso BankerPoint == 7 ->
        odds({?lucky_7, BankerCardNum});
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, BankerCardNum, _PlayerCards, _BankerCards, ?super_lucky_7) ->
    if BankerPoint > PlayerPoint andalso BankerPoint == 7 andalso BankerCardNum == 3 ->
        odds(?super_lucky_7);
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, BankerCardNum, _PlayerCards, _BankerCards, ?lucky_6) ->
    if BankerPoint > PlayerPoint andalso BankerPoint == 6 ->
        odds({?lucky_6, BankerCardNum});
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, BankerCardNum, _PlayerCards, _BankerCards, ?lucky_6_2) ->
    if BankerPoint > PlayerPoint andalso BankerPoint == 6 andalso BankerCardNum == 2 ->
        odds(?lucky_6_2);
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, BankerCardNum, _PlayerCards, _BankerCards, ?lucky_6_3) ->
    if BankerPoint > PlayerPoint andalso BankerPoint == 6 andalso BankerCardNum == 3 ->
        odds(?lucky_6_3);
        true ->
            0
    end;
check_winner_area(_PlayerPoint, _BankerPoint, _PlayerCardNum, _BankerCardNum, _PlayerCards, BankerCards, ?banker_pair) ->
    case BankerCards of
        [{P, _}, {P, _} | _] ->
            odds(?banker_pair);
        _ -> 0
    end;
check_winner_area(_PlayerPoint, _BankerPoint, _PlayerCardNum, _BankerCardNum, PlayerCards, _BankerCards, ?player_pair) ->
    case PlayerCards of
        [{P, _}, {P, _} | _] ->
            odds(?banker_pair);
        _ -> 0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, _BankerCardNum, _PlayerCards, _BankerCards, ?tie) ->
    if BankerPoint == PlayerPoint ->
        odds(?tie);
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, _BankerCardNum, _PlayerCards, _BankerCards, ?player) ->
    if BankerPoint < PlayerPoint ->
        odds(?player);
        true ->
            0
    end;
check_winner_area(PlayerPoint, BankerPoint, _PlayerCardNum, _BankerCardNum, _PlayerCards, _BankerCards, ?banker) ->
    if BankerPoint > PlayerPoint ->
        odds({?banker, BankerPoint});
        true ->
            0
    end.


%%玩家手牌点数为 7 时获胜。<br>• 两张牌组成 7 → 6倍<br>• 三张牌组成 7 → 15倍
odds({?lucky_7, 2}) -> 6;
odds({?lucky_7, 3}) -> 15;
%%<br>• 三张牌组成 7 且击败庄家 30倍
odds(?super_lucky_7) -> 30;
%%赌庄家赢且点数为6：
%%庄家3张牌6点赢 → 赔 1:12
%%庄家2张牌6点赢 → 赔 1:20
odds({?lucky_6, 2}) -> 20;
odds({?lucky_6, 3}) -> 12;
%%只要庄家2张牌6点赢
odds(?lucky_6_2) -> 22;
odds(?lucky_6_3) -> 50;
odds(?banker_pair) -> 11;
odds(?player_pair) -> 11;
odds(?tie) -> 8;
odds(?player) -> 1;
%% 庄家以6点赢佩服 0.5
odds({?banker, 6}) -> 0.5;
odds({?banker, _}) -> 1.


