-module(ws_SUITE).

-compile(export_all).

all() ->
    [join].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = phoenix_test:setup(),
    {ok, _} = test_ws_client:start_link(#{url => {"localhost", "/websocket"},
                                          port => 8888}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = phoenix_test:teardown(),
    ok = test_ws_client:stop(),
    ok.

join(_Config) ->
    timer:sleep(1000),


    timer:sleep(1000).
