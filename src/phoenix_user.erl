-module(phoenix_user).
-include("phoenix_internal.hrl").

-export([sign_up/2,
         log_in/2,
         find_by_name/1, find_by_id/1]).

-export([all/0]).

-export([migrate/2]).

sign_up(Name, Password) ->
    case phoenix_user:find_by_name(Name) of
        not_found ->
            {ok, _UserId} = create(Name, Password);
        User when is_record(User, phoenix_user) ->
            already_registered
    end.

create(UserName, Password) ->
    User = create_user(UserName, Password),
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

    {ok, User#phoenix_user.id}.

log_in(Name, Password) ->
    Result = find_by_name(Name),
    io:format("~p~n", [Result]),
    case Result of
        not_found ->
            not_registered;
        User when is_record(User, phoenix_user)->
            case erlpass:match(Password, User#phoenix_user.password) of
                true ->
                    {ok, User#phoenix_user.id};
                _ ->
                    bad_password
            end
    end.

single_answer([Result|_Rest]) -> Result;
single_answer([]) -> not_found.

find_by_name(Name) -> find_one_by(name, Name).

find_by_id(Id) -> find_one_by(id, Id).

find_one_by(Key, Value)->
    Filter = case Key of
                 name -> #phoenix_user{_ = '_', name = Value};
                 id -> #phoenix_user{_ = '_', id = Value}
             end,
    Fun = fun() ->
        mnesia:match_object(phoenix_users, Filter, read)
    end,
    single_answer(mnesia:activity(transaction, Fun)).


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
    User = create_user(UserName),
    ?assert(is_binary(User#phoenix_user.id)),
    ?assert(User#phoenix_user.name == UserName),
    ?assert(User#phoenix_user.clock == {1,0}).

-endif.
