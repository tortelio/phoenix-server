-module(phoenix_test).
-include("phoenix_test.hrl").

-export([setup/0,
         teardown/0]).

-export([setup_ws_client/1,
         teardown_ws_client/1]).

%% Client calls
-export([client_sign_up/3,
         client_log_in/3,
         client_log_out/2,
         client_get_user_data/2,
         client_add_item/3,
         client_update_item/4,
         client_finish_item/2,
         client_delete_item/2,
         client_fork_item/2
        ]).

setup() ->
    ok = phoenix:start(),
    ok = phoenix_db:bootstrap(i_know_what_i_am_doing).

teardown() ->
    ok = phoenix:stop().

setup_ws_client(Config) ->
    {ok, _} = test_ws_client:start_link(Config),

    %% TODO remove, but necessary because of ws_upgrade
    timer:sleep(500),
    ok.

teardown_ws_client(Config) ->
    ok = test_ws_client:stop(Config).

client_sign_up(Client, Username, Password) ->
    ok = ws_send_4_answer(Client, sign_up, #{name => Username, password => Password}).

client_log_in(Client, Username, Password) ->
    {ok, _UserId} = ws_send_4_answer(Client, log_in, #{name => Username, password => Password}).

client_log_out(Client, UserId) ->
    ok = ws_send_4_answer(Client, log_out, #{user_id => UserId}).

client_get_user_data(Client, UserId) ->
    {ok, #{<<"user">> := #{<<"item_id">> := UserId,
                           <<"name">> := UserName},
           <<"items">> := Items,
           <<"ext_items">> := ExtItems
          }} = ws_send_4_answer(Client, get_user_data, #{user_id => UserId}),

    {ok, UserId, UserName, Items, ExtItems}.

client_add_item(Client, Title, UserId) ->
    {ok, _ItemId} = ws_send_4_answer(Client, add_item, #{title => Title, owner => UserId}).

client_update_item(Client, ItemId, Title, Done) ->
    ok = ws_send_4_answer(Client, update_item, #{item_id => ItemId, title => Title, done => Done}).

client_delete_item(Client, ItemId) ->
    ok = ws_send_4_answer(Client, delete_item, #{item_id => ItemId}).

client_fork_item(Client, ItemId) ->
    ok = ws_send_4_answer(Client, fork_item, #{item_id => ItemId}).

ws_send_4_answer(Client, Type, Value) when is_atom(Type) ->
    ws_send_4_answer(Client, ?A2B(Type), Value);
ws_send_4_answer(Client, Type, Value) when is_binary(Type) ->
    Message = jiffy:encode(#{type => Type, value => Value}),
    case test_ws_client:send_4_answer(Client, Message) of
        Result when is_binary(Result) ->
            case jiffy:decode(Result, [return_maps]) of
                #{<<"type">> := Type, <<"value">> := RValue} -> {ok, RValue};
                #{<<"type">> := Type} -> ok;
                Answer -> throw({impossible, unknown_message, Answer})
            end;
        Answer -> throw({impossible, unhandled_message, Answer})
    end.
