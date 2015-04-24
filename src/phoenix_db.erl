-module(phoenix_db).
-include("phoenix_internal.hrl").

-export([start/0, stop/0]).
-export([bootstrap/1]).

start() ->
    case mnesia:wait_for_tables(?TABLES, ?TIMEOUT) of
        ok ->
            ok;
        {timeout, _Tables} ->
            ok;
        {error, Results} ->
            {error, Results}
    end.

stop() -> ok.

bootstrap(i_know_what_i_am_doing) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    Fun = fun(Model) ->
        erlang:apply(Model, migrate, [up, [node()]])
    end,
    lists:foreach(Fun, ?MODELS).
