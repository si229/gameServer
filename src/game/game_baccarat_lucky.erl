%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 2月 2026 上午 10:59
%%%-------------------------------------------------------------------
-module(game_baccarat_lucky).
-author("si").
-include("msg.hrl").
%% API
-export([odds/1, payout_calculation/2]).

payout_calculation(PlayerCards, BankerCards) ->
    PlayerPoint = game_baccarat:get_point(PlayerCards),
    BankerPoint = game_baccarat:get_point(BankerCards),
    PlayerCardNum = length(PlayerCards),
    BankerCardNum = length(BankerCards),
    AreaList = [?lucky_7, ?super_lucky_7, ?lucky_6, ?lucky_6_2, ?lucky_6_3,
        ?banker_pair, ?player_pair, ?banker, ?player, ?tie],
    lists:filtermap(fun(Area) ->
        Odds = check_winner_area(PlayerPoint, BankerPoint, PlayerCardNum
            , BankerCardNum, PlayerCards, BankerCards, Area),
        if Odds == 0 -> false;
            true ->
                {true, {Area, Odds}}
        end
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

%%幸运百家乐

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
