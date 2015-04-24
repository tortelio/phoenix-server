-module(phoenix_app).
-behaviour(application).

-define(APPLICATION_NAME, phoenix).

%% API.
-export([start/2, stop/1]).

-export([data_dir/0]).
%% Implementation

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

data_dir() ->
    {ok, DirSpec} = application:get_env(phoenix, data_dir),
    expand_path(DirSpec).

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

expand_path({priv_dir, RelativeDir}) when is_list(RelativeDir) ->
    filename:join([code:priv_dir(phoenix), RelativeDir]);
expand_path(Dir) when is_list(Dir) ->
    filename:absname(Dir).
