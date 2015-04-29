-module(ws_test_client).
-behaviour(websocket_client_handler).

-export([start_link/1, init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-export([login/1]).

start_link(Url) ->
  websocket_client:start_link(Url, ?MODULE, []).

init(_, _) -> {ok, []}.
websocket_handle(_Frame, _, State) -> {ok, State}.
websocket_info(_Frame, _, State) -> {ok, State}.
websocket_terminate(_Close, _, _State) -> ok.

login(UserName) ->
  ok.
