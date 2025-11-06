%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 11月 2025 下午 8:32
%%%-------------------------------------------------------------------
-module(game_net_check).
-author("si").

%% API
-export([init/2]).

init(Req,Opts)->
    Req1 =cowboy_req:reply(200,#{
        <<"content-type">>=><<"text/plain">>
    },<<"ok">>,Req),
    {ok,Req1,Opts}.