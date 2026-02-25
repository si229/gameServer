%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 2月 2026 上午 8:28
%%%-------------------------------------------------------------------
-module(game_server_room).
-author("si").

-define(ETS_ROADS, ets_roads).
-record(ets_roads, {
    key,
    roads
}).
%% API
-export([init/0, clean/2, add_road/3, get_road/2]).

init() ->
    ets:new(?ETS_ROADS, [public, set, named_table, {keypos, #ets_roads.key}]).

clean(PlayType, GameType) ->
    ets:delete(?ETS_ROADS, {PlayType, GameType}).

add_road(PlayType, GameType, Result) ->
    Key = {PlayType, GameType},
    case ets:lookup(?ETS_ROADS, Key) of
        [#ets_roads{roads = Roads} = E] ->
            ets:insert(?ETS_ROADS, E#ets_roads{roads = [Result | Roads]});
        _ ->
            ets:insert(?ETS_ROADS, #ets_roads{roads = [Result], key = [Result]})
    end.

get_road(PlayType, GameType) ->
    Key = {PlayType, GameType},
    case ets:lookup(?ETS_ROADS, Key) of
        [#ets_roads{roads = Roads}] -> Roads;
        _ ->
            []
    end.