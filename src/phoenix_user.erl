-module(phoenix_user).
-include("phoenix_internal.hrl").


-export([get_by_id/1]).
-export([sign_up/2,
         log_in/2,
         find_by_id/1, find_by_name/1]).

-export([all/0]).

-export([migrate/2]).

get_by_id(UserId) ->
    case find_by_id(UserId) of
        [] -> throw({impossible, erlang:get_stacktrace()});
        [User] -> User;
        _ -> throw({impossible, erlang:get_stacktrace()})
    end.

sign_up(Name, Password) ->
    case find_by_name(Name) of
        [User] when is_record(User, phoenix_user) ->
            already_registered;
        [] ->
            {ok, _UserId} = create(Name, Password);
        Else ->
            throw({impossible, {phoenix_user, sign_up, Else}})
    end.

create_user_log(Action, User) ->
    #phoenix_user_log{id = ?GENERATE_TOKEN,
                      user_id = User#phoenix_user.id,
                      action = Action,
                      time = ?NOW,
                      clock = User#phoenix_user.clock}.

create(UserName, Password) ->
    User = create_user(UserName, Password),
    UserLog = create_user_log(register, User),

    Fun = fun() ->
        ok = mnesia:write(phoenix_users, User, write),
        ok = mnesia:write(phoenix_users_log, UserLog, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, User#phoenix_user.id}.

log_in(Name, Password) ->
    case find_by_name(Name) of
        [User] when is_record(User, phoenix_user)->
            case erlpass:match(Password, User#phoenix_user.password) of
                true ->
                    {ok, User#phoenix_user.id};
                _ ->
                    bad_password
            end;
        [] ->
            not_registered;
        Else ->
            throw({impossible, {phoenix_user, log_in, Else}})
    end.

find_by_id(UserId) ->
    ?TRANSACTION(mnesia:match_object(phoenix_users, ?FILTER__PH_USER__ID(UserId), read)).

find_by_name(UserName) ->
    ?TRANSACTION(mnesia:match_object(phoenix_users, ?FILTER__PH_USER__NAME(UserName), read)).

all() ->
    ?TRANSACTION(
       begin
           Results = mnesia:match_object(phoenix_users, #phoenix_user{_ = '_'}, read),
           Logs = mnesia:match_object(phoenix_users_log, #phoenix_user_log{_ = '_'}, read),
           {{users, Results}, {logs, Logs}}
       end).

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

% TODO
create_user(UserName, Password) ->
    UserId = ?GENERATE_TOKEN,
    PasswordHash = erlpass:hash(Password),
    #phoenix_user{id = UserId, name = UserName, password = PasswordHash, clock = itc:seed()}.

%%------------------------------------
%%               Test
%%------------------------------------
-ifdef(EUNIT).

create_user_test() ->
    UserName = <<"Tester">>,
    Password = <<"PW">>,
    User = create_user(UserName, Password),
    ?assert(is_binary(User#phoenix_user.id)),
    ?assert(User#phoenix_user.name == UserName),
    ?assert(User#phoenix_user.clock == {1,0}).

-endif.
