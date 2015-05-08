-module(green_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(TEST_PORT, 8080).
%% Test server callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test implementations
-export([full_process/0, full_process/1]).

%% Test server callbacks
all() ->
    [full_process].

init_per_suite(Config) ->
    ok = application:set_env(phoenix, port, ?TEST_PORT),
    phoenix_db:bootstrap(i_know_what_i_am_doing),
    {ok, _} = phoenix:start(),
    {ok, _} = application:ensure_all_started(gun),
    [{phoenix_url, "http://127.0.0.1:" ++ integer_to_list(?TEST_PORT) ++ "/"} | Config].

end_per_suite(_Config) ->
    phoenix:stop(),
    ok.

full_process() ->
    [].
full_process(_Config) ->
    {ok, _ConnPid} = gun:open("127.0.0.1", ?TEST_PORT),
    ok.
