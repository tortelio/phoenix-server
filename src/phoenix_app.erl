-module(phoenix_app).
-include("phoenix_internal.hrl").
-behaviour(application).

-define(APPLICATION_NAME, phoenix).

%% API.
-export([start/2, stop/1]).

%%------------------------------------
%%         Implementation
%%------------------------------------

-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
    {ok, Port} = application:get_env(port),
    start_cowboy(Port),
    ok = phoenix_db:start(),
    phoenix_sup:start_link([]).

-spec stop(_) -> ok.
stop(_State) ->
    stop_cowboy(),
    phoenix_db:stop().

%%------------------------------------
%%             Private
%%------------------------------------

% Starts cowboy
start_cowboy(Port) ->
    Host = '_',
    Routes = [{"/", cowboy_static, {priv_file, ?APPLICATION_NAME, "static/index.html"}},
              {"/assets/[...]", cowboy_static, {priv_dir, ?APPLICATION_NAME, "static/assets"}},
              {"/websocket", ws_handler, []}],

    Dispatch = cowboy_router:compile([{Host, Routes}]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]} ]),
    io:format("~w started on ~p.~n", [?APPLICATION_NAME, Port]).

% Stops cowboy
stop_cowboy() ->
    ok = cowboy:stop_listener(http).

%%------------------------------------
%%               Test
%%------------------------------------

-ifdef(TEST).
-define(TEST_PORT, 18080).

%setup() ->
%    ok = application:set_env(phoenix, port, ?TEST_PORT).
%
%start_test() ->
%    setup(),
%    {ok, _} = application:ensure_all_started(phoenix).
%%    ?assertNot(undefined == whereis(phoenix_sup)).
%%
%%stop_test() ->
%%    ok = application:stop(phoenix),
%%    ?assert(undefined == whereis(phoenix_sup)).
%
%ok_test() ->
%    [2, 1] == lists:reverse([1, 2]).
%
-endif.
