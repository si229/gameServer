%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 11月 2025 下午 8:12
%%%-------------------------------------------------------------------
-module(game_net).
-author("si").

%% API
-export([start/0]).

start() ->
    Nets = application:get_env(game_server, net, undefined),
    start_ws(Nets).

start_ws(#{port := Port,type:=Type} = Acc) ->
    SocketOptions = [{port, Port}],
    TransOpts0 = Acc#{socket_opts=>SocketOptions},
    Path = "/",
    Paths = path(Path),
    case Type of
        ws->
            cowboy:start_clear(Type, TransOpts0, Paths);
        wss->
            cowboy:start_tls(Type, TransOpts0, Paths)
    end;
start_ws(_) ->
    skip.


path(Path) ->
    Paths = [
        {Path, game_ws_handler, []},
        {"/check", game_net_check, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    #{env => #{dispatch => Dispatch}}.



