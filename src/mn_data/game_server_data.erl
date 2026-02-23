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
        {user, [{disc_copies, [node()]}, {record_name, user}, {attributes, record_info(fields, user)}, {match, #user{ _ = '_'}}]},
        {email, [{disc_copies, [node()]}, {record_name, email}, {attributes, record_info(fields, email)}, {match, #email{ _ = '_'}}]},
        {t_email, [{disc_copies, [node()]}, {record_name, t_email}, {attributes, record_info(fields, t_email)}, {match, #t_email{ _ = '_'}}]},
        {phone, [{disc_copies, [node()]}, {record_name, phone}, {attributes, record_info(fields, phone)}, {match, #phone{ _ = '_'}}]},
        {t_phone, [{disc_copies, [node()]}, {record_name, t_phone}, {attributes, record_info(fields, t_phone)}, {match, #t_phone{ _ = '_'}}]},

        {user_base_info, [{disc_copies, [node()]}, {record_name, user_base_info}, {attributes, record_info(fields, user_base_info)}, {match, #user_base_info{ _ = '_'}}]}
    ].