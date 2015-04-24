-module(green_SUITE).
-include_lib("common_test/include/ct.hrl").
%% Test server callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test implementations
-export([full_process/0, full_process/1]).

%% Test server callbacks
all() ->
    [full_process].

init_per_suite(Config) ->
    ok = phoenix:start([], Config),
    ok.

end_per_suite(_Config) ->
    phoenix:stop(),
    ok.

full_process() ->
    ok.
full_process(_Config) ->
    ok.
