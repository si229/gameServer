%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%% 经典百家乐
%%% @end
%%% Created : 23. 2月 2026 上午 10:59
%%%-------------------------------------------------------------------
-module(game_baccarat_classic).
-author("si").
-include("msg.hrl").
%% API
-export([odds/1, payout_calculation/2]).

payout_calculation(PlayerCards, BankerCards) ->
    PlayerPoint = game_baccarat:get_point(PlayerCards),
    BankerPoint = game_baccarat:get_point(BankerCards),
    PlayerCardNum = length(PlayerCards),
    BankerCardNum = length(BankerCards),
    AreaList = [?banker_pair, ?player_pair, ?banker, ?player, ?tie],
    lists:map(fun(Area) ->
        Odds = check_winner_area(PlayerPoint, BankerPoint, PlayerCardNum
            , BankerCardNum, PlayerCards, BankerCards, Area),
        {Area, Odds}
              end, AreaList).

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


%%只要庄家2张牌6点赢
odds(?banker_pair) -> 11;
odds(?player_pair) -> 11;
odds(?tie) -> 8;
odds(?player) -> 1;
odds(?banker) -> 0.95.
