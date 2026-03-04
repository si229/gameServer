%%%-------------------------------------------------------------------
%%% @author nm_jok
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 3月 2026 14:17
%%%-------------------------------------------------------------------
-module(game_roulette).

-include("room.hrl").
-include("roulette.hrl").
-include("common.hrl").
%% API
-export([init/0]).


init() ->
    supervisor:start_child(room_sup, [?GUEST, ?GAME_TYPE_AMERICAN_ROULETTE]),
    supervisor:start_child(room_sup, [?GUEST, ?GAME_TYPE_FRENCH_ROULETTE]),
    supervisor:start_child(room_sup, [?NORMAL, ?GAME_TYPE_FRENCH_ROULETTE]),
    supervisor:start_child(room_sup, [?NORMAL, ?GAME_TYPE_AMERICAN_ROULETTE]).



