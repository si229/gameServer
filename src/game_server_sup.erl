%%%-------------------------------------------------------------------
%% @doc game_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => user_sup,
            start => {user_sup, start_link, []},
            modules =>[user_sup],
            restart=>permanent,
            shutdown=>5000,
            type=>supervisor
        },
        #{
            id => room_sup,
            start => {room_sup, start_link, []},
            modules =>[room_sup],
            restart=>permanent,
            shutdown=>5000,
            type=>supervisor
        },
        #{
            id => game_net_handler,
            start => {game_net_handler, start_link, []},
            modules =>[game_net_handler],
            restart=>permanent,
            shutdown=>5000,
            type=>worker
        }

    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
