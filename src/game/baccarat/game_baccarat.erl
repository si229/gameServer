%% 幸运百家乐

-module(game_baccarat).
-include("card.hrl").
-include("common.hrl").
-include("msg.hrl").
-include("room.hrl").
-define(SHOE_SIZE, 8). %% 1 shoe 8副牌
-define(SHOE_MIN_NUM, 52). %% 小于52要重新换牌


-export([deal/1, try_reshuffle_the_shoe/1]).
-export([init_shoe/0, init/0, get_point/1]).

-export([gen_hash/2, payout_calculation/3, settlement/2]).

-export([try_shuffle_the_shoe/1]).

init_shoe() ->
    Deck = ?DECK,
    Shoe = lists:foldl(fun(_, Acc) ->
        Deck ++ Acc
                       end, Deck, lists:seq(2, ?SHOE_SIZE)),

    card:shuffle(Shoe).

%%    Shoe.

init() ->
    supervisor:start_child(room_sup, [?GUEST, ?GAME_TYPE_BACCARAT_LUCKY]),
    supervisor:start_child(room_sup, [?GUEST, ?GAME_TYPE_BACCARAT_CLASSIC]),
    supervisor:start_child(room_sup, [?NORMAL, ?GAME_TYPE_BACCARAT_LUCKY]),
    supervisor:start_child(room_sup, [?NORMAL, ?GAME_TYPE_BACCARAT_CLASSIC]).

try_reshuffle_the_shoe(Deck) ->
    case length(Deck) =< ?SHOE_MIN_NUM of
        true ->
            {true, init_shoe()};
        false ->
            false
    end.

try_shuffle_the_shoe(Deck) ->
    case length(Deck) =< ?SHOE_MIN_NUM of
        true ->
            init_shoe();
        false ->
            Deck
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
                if PlayerPoint >= 6 ->
                    {false, PlayerCard, get_point([PlayerCard2]), Res};
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
            true -> Acc
        end
                end, 0, CardList) rem 10.


gen_hash(PlayerCards, BankerCards) ->
    RandomStr = game_util:random_string(),
    Cards = PlayerCards ++ BankerCards,
    Points = [P || {P, _} <- Cards],
    Colors = [C || {_, C} <- Cards],
    Timestamp = erlang:system_time(1000),

    Str = lists:flatten([
        begin
            if is_integer(X) ->
                if X < 10 ->
                    ["0" | integer_to_list(X)];
                    true ->
                        integer_to_list(X)
                end;
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
    {game_util:sha256_hex(L), L, Timestamp, RandomStr, Str}.



settlement(BetList, Payout) ->
    lists:foldl(fun({BetArea, Amount}, Acc) ->
        Odds = proplists:get_value(BetArea, Payout, -1),
        Amount * Odds + Amount + Acc
        end, 0, BetList).

payout_calculation(?GAME_TYPE_BACCARAT_CLASSIC, PlayerCards, BankerCards) ->
    game_baccarat_classic:payout_calculation(PlayerCards, BankerCards);
payout_calculation(?GAME_TYPE_BACCARAT_LUCKY, PlayerCards, BankerCards) ->
    game_baccarat_lucky:payout_calculation(PlayerCards, BankerCards).

