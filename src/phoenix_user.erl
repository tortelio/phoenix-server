-module(phoenix_user).
-include("phoenix_internal.hrl").

-export([create/1, find_by_user_name/1, find_by_user_id/1]).

-export([migrate/2]).

-define(DESCRIPTION_NAME, "DESCRIPTION").

create(UserName) when is_binary(UserName) ->
    UserId = ?GENERATE_TOKEN,
    User = #phoenix_user{id = UserId,
                         name = UserName},
    Fun = fun() ->
        ok = mnesia:write(phoenix_users, User, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, User}.

find_by_user_name(UserName) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_users, #phoenix_user{_ = '_', name = UserName}, read)
    end,
    mnesia:activity(transaction, Fun).

find_by_user_id(UserId) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_users, #phoenix_user{_ = '_', id = UserId}, read)
    end,
    mnesia:activity(transaction, Fun).

migrate(up, Nodes) ->
    mnesia:create_table(phoenix_users,
                        [{attributes, record_info(fields, phoenix_user)},
                         {record_name, phoenix_user},
                         {index, [#phoenix_user.name]},
                         {disc_copies, Nodes}]).
