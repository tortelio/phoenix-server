-module(phoenix).
-export([start/0, stop/0]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

stop() ->
    ok = application:stop(?MODULE).
