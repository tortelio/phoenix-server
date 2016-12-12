%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Phoenix database
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%==============================================================================
%% Definitions
%%==============================================================================
-module(phoenix_db).

%% Includes
-include("phoenix_internal.hrl").

%% API
-export([start/0,
         stop/0]).
-export([bootstrap/1]).

%%==============================================================================
%% Implementation
%%==============================================================================
%%------------------------------------------------------------------------------
%% Public functions
%%------------------------------------------------------------------------------
start() ->
    case mnesia:wait_for_tables(?TABLES, ?TIMEOUT) of
        ok ->
            ok;
        {timeout, _Tables} ->
            ok;
        {error, Results} ->
            {error, Results}
    end.

% TODO
stop() ->
    ok.

bootstrap(i_know_what_i_am_doing_1) ->
    Nodes = [node() | nodes()],
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    mnesia:create_schema(Nodes),
    mnesia:start();

bootstrap(i_know_what_i_am_doing_2) ->
    Nodes = [node() | nodes()],
    Fun = fun(Model) ->
                  erlang:apply(Model, migrate, [up, Nodes])
          end,
    lists:foreach(Fun, ?MODELS).
