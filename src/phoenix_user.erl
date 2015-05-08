-module(phoenix_user).
-include("phoenix_internal.hrl").

-export([create/1, find_by_user_name/1, find_by_user_id/1]).
-export([all/0]).

-export([migrate/2]).

create(UserName) when is_binary(UserName) ->
    User = create_user(UserName),
    Log = #phoenix_user_log{id = ?GENERATE_TOKEN,
                            user_id = User#phoenix_user.id,
                            action = register,
                            time = now(),
                            clock = User#phoenix_user.clock},
    Fun = fun() ->
        ok = mnesia:write(phoenix_users, User, write),
        ok = mnesia:write(phoenix_users_log, Log, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, {User#phoenix_user.id, User#phoenix_user.name}}.

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

all() ->
    Fun = fun() ->
        Results = mnesia:match_object(phoenix_users, #phoenix_user{_ = '_'}, read),
        Logs = mnesia:match_object(phoenix_users_log, #phoenix_user_log{_ = '_'}, read),
        {{result, Results}, {log, Logs}}
    end,
    mnesia:activity(transaction, Fun).

migrate(up, Nodes) ->
    {atomic, ok} = mnesia:create_table(phoenix_users,
                        [{attributes, record_info(fields, phoenix_user)},
                         {record_name, phoenix_user},
                         {index, [#phoenix_user.name]},
                         {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(phoenix_users_log,
                        [{attributes, record_info(fields, phoenix_user_log)},
                         {record_name, phoenix_user_log},
                         {index, [#phoenix_user_log.action]},
                         {disc_copies, Nodes}]).

create_user(UserName) ->
    UserId = ?GENERATE_TOKEN,
    #phoenix_user{id = UserId, name = UserName, clock = itc:seed()}.

%%------------------------------------
%%               Test
%%------------------------------------
-ifdef(EUNIT).

create_user_test() ->
    UserName = <<"Tester">>,
    User = create_user(UserName),
    ?assert(is_binary(User#phoenix_user.id)),
    ?assert(User#phoenix_user.name == UserName),
    ?assert(User#phoenix_user.clock == {1,0}).

-endif.
