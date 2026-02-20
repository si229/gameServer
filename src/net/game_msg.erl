%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 11月 2025 下午 9:31
%%%-------------------------------------------------------------------
-module(game_msg).
-author("si").
-define(LOGIN_CMD,16).
%% API
-export([dispatch_msg/2]).


dispatch_msg(<<Cmd:8,CCmd:8,Binary/binary>>,State)->
    Cmd16 = Cmd bsl 8 +CCmd,
    route(Cmd16,Binary,State).


route(Cmd,Request,State) when Cmd<?LOGIN_CMD->
    case catch msg:handle_msg(Cmd,Request,State) of
        {'EXIT',Reason}->
            skip;
        Maps->

            skip
    end,


    skip.