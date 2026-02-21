-module(game_net).

%% API
-export([start/0, stop/0]).
-export([init/0]).

-define(ETS_LISTENERS, ets_listeners).
-record(ets_listeners, {
    type,
    p
}).

init() ->
    ets:new(ets_listeners, [public, set, named_table, {keypos, #ets_listeners.type}]).

start() ->
    Nets = application:get_env(game_server, net, undefined),
    start_ws(Nets).

start_ws(#{socket_opts := SocketOptions, type := Type} = Acc) ->
    TransOpts0 = Acc#{socket_opts => SocketOptions},
    Path = "/ws",
    Paths = path(Path),
    Result = case Type of
                 ws ->
                     cowboy:start_clear(Type, TransOpts0, Paths);
                 wss ->
                     cowboy:start_tls(Type, TransOpts0, Paths)
             end,
    case Result of
        {ok, P} ->
            ets:insert(?ETS_LISTENERS, #ets_listeners{type = Type, p = P});
        _ ->
            error
    end;
start_ws(_) ->
    skip.

stop() ->
    lists:foreach(fun(#ets_listeners{type = Type}) ->
        ranch:stop_listener(Type)
                  end, ets:tab2list(?ETS_LISTENERS)),
    ets:delete_all_objects(?ETS_LISTENERS).

path(Path) ->
    Paths = [
        {Path, game_ws_handler, []},
        {"/check", game_net_check, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    #{env => #{dispatch => Dispatch}}.