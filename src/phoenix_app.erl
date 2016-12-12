%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% Phoenix application
%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%==============================================================================
%% Definitions
%%==============================================================================
-module(phoenix_app).
-behaviour(application).

%% Includes
-include("phoenix_internal.hrl").

%% API
-export([start/2,
         stop/1]).

%%==============================================================================
%% Implementation
%%==============================================================================
%%------------------------------------------------------------------------------
%% Public functions
%%------------------------------------------------------------------------------
-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
    Config = application:get_all_env(phoenix),

    Port = proplists:get_value(port, Config),

    [net_adm:ping(Node) || Node <- proplists:get_value(nodes, Config, [])],

    ok = start_cowboy(Port),

    phoenix_sup:start_link([]).

-spec stop(_) -> ok.
stop(_State) ->
    ok = stop_cowboy(),
    ok = phoenix_db:stop().

%%------------------------------------------------------------------------------
%% Private functions
%%------------------------------------------------------------------------------
%% Starts cowboy server
start_cowboy(Port) when is_integer(Port) andalso Port > 0 ->
    Routes = [{"/",             cowboy_static,  {priv_file,  ?APPLICATION, "static/index.html"}},
              {"/assets/[...]", cowboy_static,  {priv_dir,   ?APPLICATION, "static/assets"}},
              {"/websocket",    ws_handler,     []}
             ],

    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _Pid} = cowboy:start_http(http, 100,
                                   [{port, Port}],
                                   [{env,  [{dispatch, Dispatch}]}]),

    ok;
start_cowboy(undefined) ->
    throw({missing_config_parameter, port});
start_cowboy(Parameter) ->
    throw({invalid_config_parameter, {port, Parameter}}).

%% Stops cowboy server
stop_cowboy() ->
    cowboy:stop_listener(http).
