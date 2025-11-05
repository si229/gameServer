%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 11月 2025 下午 8:35
%%%-------------------------------------------------------------------
-module(proto_lib).
-author("si").
-include("user.hrl").

%% API
-export([
    msg_record/1,
    decode/1,
    encode/1
    ]).



msg_record(1)->#'LoginRequest'{};
msg_record(2)->#'LoginReply'{}.

msg_cmd(#'LoginRequest'{})->1;
msg_cmd(#'LoginReply'{})->2.


msg_mod(Cmd) when Cmd>=1 andalso Cmd =<200->user.


msg_name(Cmd)->
    element(1,msg_record(Cmd)).


decode(<<Cmd:16,B/binary>>)->
    Mod = msg_mod(Cmd),
    Mod:decode_msg(msg_name(Cmd),B).


encode(Msg)->
    Cmd = msg_cmd(Msg),
    Mod = msg_mod(Cmd),
    Bin = Mod:encode_msg(Msg),
    <<Cmd:16,Bin/binary>>.







