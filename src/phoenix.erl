-module(phoenix).
-export([main/1, start/0, stop/0]).

main(_Args) ->
    ok = application:start(?MODULE).

start() ->
  {ok, _} = application:ensure_all_started(?MODULE),
  ok.

stop() ->
  application:stop(?MODULE).
