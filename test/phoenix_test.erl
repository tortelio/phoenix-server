-module(phoenix_test).
-include("phoenix_test.hrl").

-export([setup/0,
         teardown/0]).

%% Client calls
-export([client_sign_up/2,
         client_log_in/2,
         client_get_user_data/1,
         client_add_item/2]).

setup() ->
    ok = phoenix:start(),
    ok = phoenix_db:bootstrap(i_know_what_i_am_doing).

teardown() ->
    ok = phoenix:stop().

client_sign_up(Username, Password) ->
    {ok, _UserId} = ?WS(sign_up, #{name => Username, password => Password}).

client_log_in(Username, Password) ->
    {ok, _UserId} = ?WS(log_in, #{name => Username, password => Password}).

client_get_user_data(UserId) ->
    {ok, #{<<"id">> := UserId,
           <<"name">> := UserName,
           <<"items">> := Items,
           <<"ext_items">> := ExtItems
          }} = ?WS(get_user_data, #{user_id => UserId}),

    {ok, UserId, UserName, Items, ExtItems}.

client_add_item(Title, UserId) ->
    {ok, _ItemId} = ?WS(add_item, #{item => #{title => Title}, user_id => UserId}).
