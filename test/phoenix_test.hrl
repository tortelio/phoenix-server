-include_lib("eunit/include/eunit.hrl").

-define(WS_CLIENT_CONFIG, #{url => {"localhost", "/websocket"},
                            port => 8888}).
-define(WS1, ws_user1).
-define(WS1_CLIENT_CONFIG, ?WS_CLIENT_CONFIG#{name => ?WS1}).

-define(WS2, ws_user2).
-define(WS2_CLIENT_CONFIG, ?WS_CLIENT_CONFIG#{name => ?WS2}).


%% Fixtures
-define(USER, <<"TEST_USER">>).
-define(USER_ID, <<"9c29ea93-55a0-51aa-a3d1-40fe55e30164">>).
-define(PASSWORD, <<"TEST_PASSWORD">>).
-define(ITEM_ID, <<"e904247f-fb8b-5671-a44e-c66a8269ff2a">>).
-define(ITEM_TITLE_1, <<"TEST_ITEM_1_TITLE">>).
-define(ITEM_TITLE_2, <<"TEST_ITEM_2_TITLE">>).

-define(USER2, <<"TEST_USER2">>).
-define(USER2_ID, <<"c75c5edb-cf3a-5360-867d-a9c2016ca975">>).
-define(PASSWORD2, <<"TEST_PASSWORD2">>).

%% Helper macros
-define(B2L(Binary), erlang:binary_to_list(Binary)).
-define(A2B(Atom), erlang:atom_to_binary(Atom, utf8)).
