%%%-------------------------------------------------------------------
%% @doc game_server public API
%% @end
%%%-------------------------------------------------------------------

-module(game_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case game_server_sup:start_link() of
        {ok,_} = R->
            game_net:init(),
            game_net:start(),
            R;
        R->
            R
    end .

stop(_State) ->
    ok.

%% internal functions
