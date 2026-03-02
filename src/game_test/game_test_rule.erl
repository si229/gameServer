%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 3月 2026 下午 7:43
%%%-------------------------------------------------------------------
-module(game_test_rule).

-include("msg.hrl").

-export([odds/1]).

-export([draw_plan/0]).

draw_plan()->
    [
        [?banker,{?lucky_7, 2},?banker_pair,?player_pair],
        [?banker,{?lucky_7, 3},?super_lucky_7,?banker_pair,?player_pair],
        [{?banker,6},{?lucky_6, 2},?lucky_6_2,?banker_pair,?player_pair],
        [{?banker,6},{?lucky_6, 3},?lucky_6_3,?banker_pair,?player_pair],
        [?player,{?lucky_7, 2},?banker_pair,?player_pair],
        [?player,{?lucky_7, 3},?super_lucky_7,?banker_pair,?player_pair],
        [?player,{?lucky_6, 2},?lucky_6_2,?banker_pair,?player_pair],
        [?player,{?lucky_6, 3},?lucky_6_3,?banker_pair,?player_pair],
        [?tie,?banker_pair,?player_pair],
        [?banker,?banker_pair,?player_pair],
        [?player,?banker_pair,?player_pair],
        [?player],
        [?banker]
        ].

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
