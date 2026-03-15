%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 3月 2026 上午 8:31
%%%-------------------------------------------------------------------
-module(game_util).
-author("si").

%% API
-export([random_string/0,sha256_hex/1,sha256/1]).


random_string() ->
    Len = 8,
    Charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    Bytes = crypto:strong_rand_bytes(Len),
    list_to_binary(
        [lists:nth((X rem length(Charset)) + 1, Charset) || <<X>> <= Bytes]
    ).

sha256_hex(InputBin) ->
    Hash = sha256(InputBin),
    list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || <<X>> <= Hash])).

sha256(InputBin) ->
    crypto:hash(sha256, InputBin).
