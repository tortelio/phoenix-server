-module(user_SUITE).
-include("phoenix_test.hrl").

-compile(export_all).

all() ->
    [sign_up].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = phoenix_test:setup(),
    {ok, _} = test_ws_client:start_link(#{url => {"localhost", "/websocket"},
                                          port => 8888}),

    %% TODO remove, but necessary because of ws_upgrade
    timer:sleep(1000),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = test_ws_client:stop(),
    ok = phoenix_test:teardown(),
    ok.

sign_up(_Config) ->

    Result1 = phoenix_test:client_sign_up(?USER, ?PASSWORD),
    ?assertMatch({ok, _}, Result1),

    {ok, UserId} = Result1,

    ?assertEqual({ok, UserId}, phoenix_test:client_log_in(?USER, ?PASSWORD)),

    io:format("ALL: ~p~n", [phoenix_user:all()]),

    ?assertEqual({ok, UserId, ?USER, [], []}, phoenix_test:client_get_user_data(UserId)),

    Result4 = phoenix_test:client_add_item(?ITEM_1_TITLE, UserId),
    ?assertMatch({ok, _}, Result4),

    {ok, _ItemId} = Result4,

    ok.
