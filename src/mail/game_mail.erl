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
-export([send_code/2]).
-export([gen_code/0]).

send_code(Email,Code) ->
    BodyHtml = iolist_to_binary([
        "<html><body>",
        "<p>Please use the verification code within 30 minutes:</p>",
        "<p style='font-size:36px; color:red; font-weight:bold; font-family:Courier New, monospace; text-align:center;'>",
        Code,
        "</p>",
        "</body></html>"
    ]),

    Content = iolist_to_binary([
        "From: Baccarat <", "mic688769@gmail.com", ">\r\n",
        "To:\r\n",
        "Subject: Baccarat Verification Code\r\n",
        "MIME-Version: 1.0\r\n",
        "Content-Type: text/html; charset=UTF-8\r\n",
        "\r\n",
        BodyHtml
    ]),

    gen_smtp_client:send({"mic688769@gmail.com", [Email], Content},
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
    [lists:nth((X rem length(Charset)) + 1, Charset) || <<X>> <= Bytes].

