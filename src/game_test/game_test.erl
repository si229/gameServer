%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 3月 2026 下午 7:45
%%%-------------------------------------------------------------------
-module(game_test).
-author("si").
-include("room.hrl").
-include("msg.hrl").
-define(INIT_CHIPS, 1000000).
%% API
-export([start/2]).

-export([start_payout/1]).

start(Num, Type) ->
    Zone = case Type of
               ?GAME_TYPE_LUCKY -> lists:seq(0, 4);
               _ ->
                   [?banker]
           end,
    RobotList = [{Id, ?INIT_CHIPS, lists:nth(rand:uniform(length(Zone)), Zone)} || Id <- lists:seq(1, 1000)],
    Shoe = game_baccarat:init_shoe(),
    start_do(Shoe, RobotList, Num, {0, 0}, Type).

start_do(_Shoe, _RobotList, Num, {Bet, Profit}, _Type) when Num =< 0 ->
    {Bet, Profit, Profit / Bet};
start_do(Shoe, RobotList, Num, {Bet, Profit}, Type) ->
    NewShoe = game_baccarat:try_shuffle_the_shoe(Shoe),
    {PlayerCards, BankerCards, Res} = game_baccarat:deal(NewShoe),
    Payout = game_baccarat:payout_calculation(Type, PlayerCards, BankerCards),
    BetList = [{Zone, 1} || {_, _, Zone} <- RobotList],
    start_do(Res, RobotList, Num - 1, {1000 + Bet, Profit + game_baccarat:settlement(BetList, Payout)}, Type).


start_payout(Type) ->
    ZoneList = case Type of
                   ?GAME_TYPE_LUCKY -> lists:seq(0, 9);
                   _ ->
                       [?banker, ?player, ?tie, ?banker_pair, ?player_pair]
               end,
    BetInfo = [{Zone, 1} || Zone <- ZoneList],
    case Type of
        ?GAME_TYPE_CLASSIC->
            game_baccarat_classic:get_current_maximum_loss(BetInfo);
        ?GAME_TYPE_LUCKY->
            game_baccarat_lucky:get_current_maximum_loss(BetInfo)
    end.