-module(phoenix_user).
-include("phoenix_internal.hrl").

%% API
-export([sign_up/2,
         log_in/2]).

-export([create/2]).

%% Getters
-export([get_by_id/1,
         get_by_name/1]).

%%==============================================================================
%% Implementation
%%==============================================================================

%% Create ---------------------------------------------------------------------

-ifdef(TEST).
-export([create/1]).
create(User) ->
    ?TRANSACTION(
       begin
           ok = db_phoenix_user:save(User),
           {ok, User}
       end).

-endif.

create(Name, Password) ->
    ?TRANSACTION(
       begin
           User =
           defaults(
             #phoenix_user{
                name = Name,
                password = erlpass:hash(Password)
               }
            ),

           ok = db_phoenix_user:save(User),
           {ok, User}
       end).

%% Sign up ---------------------------------------------------------------------

sign_up(Name, Password) ->
    case get_by_name(Name) of
        undefined ->
            {ok, _} = create(Name, Password),
            ok;
        _User ->
            {error, already_registered}
    end.

%% Log in ----------------------------------------------------------------------

log_in(Name, Password) ->
    case db_phoenix_user:find_by_name(Name) of
        [User] when is_record(User, phoenix_user)->
            case erlpass:match(Password, User#phoenix_user.password) of
                true ->
                    {ok, User#phoenix_user.id};
                _ ->
                    {error, bad_password}
            end;
        [] ->
            {error, not_registered};
        Else ->
            throw({impossible, {phoenix_user, log_in, Else}})
    end.

%% Getters ---------------------------------------------------------------------

get_by_id(UserId) ->
    case db_phoenix_user:find_by_id(UserId) of
        [] -> undefined;
        [User] -> User;
        _Else -> throw({impossible, {phoenix_user, get_by_id}})
    end.

get_by_name(Username) ->
    case db_phoenix_user:find_by_name(Username) of
        [] -> undefined;
        [User] -> User;
        _Else -> throw({impossible, {phoenix_user, get_by_name}})
    end.


%%==============================================================================
%% @private
%%==============================================================================

%% Fill default values ---------------------------------------------------------

defaults(#phoenix_user{id = undefined} = User) ->
    defaults(User#phoenix_user{id = ?GENERATE_TOKEN});
defaults(User) when is_record(User, phoenix_user) ->
    User.

%%==============================================================================
%% Test
%%==============================================================================

-ifdef(EUNIT).
%% TODO
-endif.
