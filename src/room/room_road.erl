%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 2月 2026 上午 8:28
%%%-------------------------------------------------------------------
-module(room_road).
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

add_road(PlayType, GameType, {_PayZone, _PlayerCards, _BankerCards} = Result) ->
    Key = {PlayType, GameType},
    case ets:lookup(?ETS_ROADS, Key) of
        [#ets_roads{roads = Roads} = E] ->
            ets:insert(?ETS_ROADS, E#ets_roads{roads = [Result | Roads]});
        _ ->
            ets:insert(?ETS_ROADS, #ets_roads{roads = [Result], key = Key})
    end.

get_road(PlayType, GameType) ->
    Key = {PlayType, GameType},
    case ets:lookup(?ETS_ROADS, Key) of
        [#ets_roads{roads = Roads}] ->
            [PayZone || {PayZone, _PlayerCards, _BankerCards} <- Roads];
        _ ->
            []
    end.