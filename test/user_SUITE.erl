-module(user_SUITE).
-include("phoenix_test.hrl").

-compile(export_all).

all() ->
    [a_green_road,
     fork_item].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(common, Config) ->
    ok = phoenix_test:setup(),
    Config;

init_per_testcase(fork_item, Config1) ->
    Config2 = init_per_testcase(common, Config1),
    ok = phoenix_test:setup_ws_client(?WS1_CLIENT_CONFIG),
    ok = phoenix_test:setup_ws_client(?WS2_CLIENT_CONFIG),

    {ok, _} = phoenix_user:create(fixtures:user(user1)),
    {ok, _} = phoenix_item:create(fixtures:item(user1_new_item)),

    {ok, _} = phoenix_user:create(fixtures:user(user2)),

    Config2;

init_per_testcase(a_green_road, Config1) ->
    Config2 = init_per_testcase(common, Config1),

    ok = phoenix_test:setup_ws_client(?WS1_CLIENT_CONFIG),
    ok = phoenix_test:setup_ws_client(?WS2_CLIENT_CONFIG),

    Config2.

end_per_testcase(common, _Config) ->
    ok = phoenix_test:teardown(),
    ok;

end_per_testcase(fork_item, Config) ->
    ok = phoenix_test:teardown(?WS1_CLIENT_CONFIG),
    ok = phoenix_test:teardown(?WS2_CLIENT_CONFIG),
    ok = end_per_testcase(common, Config),
    ok;

end_per_testcase(a_green_road, Config) ->
    ok = phoenix_test:teardown_ws_client(?WS1_CLIENT_CONFIG),
    ok = end_per_testcase(common, Config),
    ok.

a_green_road(_Config) ->

%% USER1 -----------------------------------------------------------------------

    %% Sign up
    ?assertEqual(ok, phoenix_test:client_sign_up(?WS1, ?USER, ?PASSWORD)),

    %% Log in
    Result1 = phoenix_test:client_log_in(?WS1, ?USER, ?PASSWORD),
    ?assertMatch({ok, _}, Result1),

    {ok, UserId} = Result1,

    %% Get user data
    ?assertEqual({ok, UserId, ?USER, [], []}, phoenix_test:client_get_user_data(?WS1, UserId)),

    %% Add new item
    Result4 = phoenix_test:client_add_item(?WS1, ?ITEM_TITLE_1, UserId),
    ?assertMatch({ok, _}, Result4),

    {ok, ItemId} = Result4,

    %% Get user data
    ?assertEqual({ok, UserId, ?USER, [#{<<"id">> => ItemId,
                                        <<"title">> => ?ITEM_TITLE_1,
                                        <<"done">> => false,
                                        <<"owner">> => UserId}], []}, phoenix_test:client_get_user_data(?WS1, UserId)),

    %% Modify the item
    ?assertEqual(ok, phoenix_test:client_update_item(?WS1, ItemId, ?ITEM_TITLE_2, false)),

    %% Get user data
    ?assertEqual({ok, UserId, ?USER, [#{<<"id">> => ItemId,
                                        <<"title">> => ?ITEM_TITLE_2,
                                        <<"done">> => false,
                                        <<"owner">> => UserId}], []}, phoenix_test:client_get_user_data(?WS1, UserId)),

    ?assertEqual(ok, phoenix_test:client_update_item(?WS1, ItemId, ?ITEM_TITLE_2, true)),

    %% Get user data
    ?assertEqual({ok, UserId, ?USER, [#{<<"id">> => ItemId,
                                        <<"title">> => ?ITEM_TITLE_2,
                                        <<"done">> => true,
                                        <<"owner">> => UserId}], []}, phoenix_test:client_get_user_data(?WS1, UserId)),

%% USER2 -----------------------------------------------------------------------

    %% Sign up
    ?assertEqual(ok, phoenix_test:client_sign_up(?WS2, ?USER2, ?PASSWORD2)),

    %% Log in
    Result21 = phoenix_test:client_log_in(?WS2, ?USER2, ?PASSWORD2),
    ?assertMatch({ok, _}, Result21),

    {ok, UserId2} = Result21,

    %% Get user data
    ?assertEqual({ok, UserId2, ?USER2, [], [#{<<"id">> => ItemId,
                                              <<"title">> => ?ITEM_TITLE_2,
                                              <<"done">> => true,
                                              <<"owner">> => UserId}]}, phoenix_test:client_get_user_data(?WS2, UserId2)),

%% USER2 -----------------------------------------------------------------------

    %% Delete item
    ?assertEqual(ok, phoenix_test:client_delete_item(?WS1, ItemId)),

    %% Get user data
    ?assertEqual({ok, UserId, ?USER, [], []}, phoenix_test:client_get_user_data(?WS1, UserId)),

    %% Get user data
    ?assertEqual({ok, UserId2, ?USER2, [], []}, phoenix_test:client_get_user_data(?WS2, UserId2)),

    %% Log out
    ?assertEqual(ok, phoenix_test:client_log_out(?WS1, UserId)),
    ?assertEqual(ok, phoenix_test:client_log_out(?WS2, UserId2)),

    ok.

fork_item(_Config) ->

    %% Log in (1)
    ?assertEqual({ok, ?USER_ID}, phoenix_test:client_log_in(?WS1, ?USER, ?PASSWORD)),
    ?assertEqual({ok, ?USER2_ID}, phoenix_test:client_log_in(?WS2, ?USER2, ?PASSWORD2)),

    %% Get user data (2)
    ?assertEqual({ok, ?USER2_ID, ?USER2, [], [#{<<"id">> => ?ITEM_ID,
                                                <<"title">> => ?ITEM_TITLE_1,
                                                <<"done">> => false,
                                                <<"owner">> => ?USER_ID}]}, phoenix_test:client_get_user_data(?WS2, ?USER2_ID)),

    %% Fork item (3)
    ?assertEqual(ok, phoenix_test:client_fork_item(?WS2, ?ITEM_ID)),

    %% Get user data (4)
    Result4 = phoenix_test:client_get_user_data(?WS2, ?USER2_ID),
    ?assertMatch({ok, ?USER2_ID, ?USER2,
                  [#{<<"id">> := _,
                     <<"title">> := ?ITEM_TITLE_1,
                     <<"done">> := false,
                     <<"owner">> := ?USER2_ID}],
                  [#{<<"id">> := ?ITEM_ID,
                     <<"title">> := ?ITEM_TITLE_1,
                     <<"done">> := false,
                     <<"owner">> := ?USER_ID}]}, Result4),
    {ok, _, _, [#{<<"id">> := ItemId4}], _} = Result4,

    %% Modify forked item (5)
    ?assertEqual(ok, phoenix_test:client_update_item(?WS2, ItemId4, ?ITEM_TITLE_2, false)),

    %% Get user data (6)
    ?assertEqual({ok, ?USER2_ID, ?USER2,
                  [#{<<"id">> => ItemId4,
                     <<"title">> => ?ITEM_TITLE_2,
                     <<"done">> => false,
                     <<"owner">> => ?USER2_ID}],
                  [#{<<"id">> => ?ITEM_ID,
                     <<"title">> => ?ITEM_TITLE_1,
                     <<"done">> => false,
                     <<"owner">> => ?USER_ID}]}, phoenix_test:client_get_user_data(?WS2, ?USER2_ID)),

    %% Log out (?)
    ?assertEqual(ok, phoenix_test:client_log_out(?WS1, ?USER_ID)),
    ?assertEqual(ok, phoenix_test:client_log_out(?WS2, ?USER2_ID)),

    ok.
