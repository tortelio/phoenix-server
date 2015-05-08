-module(phoenix).
-export([main/1, start/0, stop/0]).

main(_Args) ->
    ok = application:start(?MODULE).

start() ->
  application:ensure_all_started(?MODULE).

stop() ->
  application:stop(?MODULE).
