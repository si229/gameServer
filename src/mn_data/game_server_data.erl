%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2026 上午 11:28
%%%-------------------------------------------------------------------
-module(game_server_data).
-author("si").
-include("user.hrl").
-behavior(mn_climber_table).
-export([table_definitions/0]).

table_definitions() ->
    [
        {user, [
            {disc_copies, [node()]},
            {record_name, user},
            {attributes, record_info(fields, user)},
            {match, #user{ _ = '_'}}
        ]}
    ].