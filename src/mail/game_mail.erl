%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2026 下午 2:40
%%%-------------------------------------------------------------------
-module(game_mail).
-author("si").
-include("common.hrl").
%% API
-export([send_code/1]).
-export([gen_code/0]).
%%send() ->
%%    Content = io_lib:format("Subject: Baccarat Verification Code \r\nFrom: Baccarat \r\nTo:
%%      \r\n\r\n ~p", [gen_code()]),
%%    gen_smtp_client:send({"mic688769@gmail.com", ["ni.mc1@icloud.com"], iolist_to_binary(Content)},
%%        [{relay, "smtp.gmail.com"}, {username, "mic688769@gmail.com"}, {password, "cjmpxtjzhfzytgqx"}
%%            , {port, 587}
%%            , {tls, always}
%%            , {auth, always}
%%            , {tls_options, [{verify, verify_none}]}
%%        ]).

send_code(Email) ->
    Content = io_lib:format("Subject: Baccarat Verification Code \r\nFrom: Baccarat \r\nTo:
      \r\n\r\n Please use the verification code within 30 minutes. ~p", [gen_code()]),
    gen_smtp_client:send({"mic688769@gmail.com", [Email], iolist_to_binary(Content)},
        [{relay, "smtp.gmail.com"}, {username, "mic688769@gmail.com"}, {password, "cjmpxtjzhfzytgqx"}
            , {port, 587}
            , {tls, always}
            , {auth, always}
            , {tls_options, [{verify, verify_none}]}
        ]).


gen_code() ->
    Len = 6,
    Charset = "0123456789",
    Bytes = crypto:strong_rand_bytes(Len),
    lists:flatten(
        [lists:nth((X rem length(Charset)) + 1, Charset) || <<X>> <= Bytes]
    ).
