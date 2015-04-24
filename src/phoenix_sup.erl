% @private
-module(phoenix_sup).
-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor.
-export([init/1]).

%% API.
-spec start_link(_) -> {ok, pid()}.
start_link(_Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.
init(_Config) ->
  Procs = [],
  {ok, {{one_for_one, 10, 10}, Procs}}.
