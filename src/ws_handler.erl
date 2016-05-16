-module(ws_handler).
-include("phoenix_internal.hrl").

-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).
-export([terminate/3]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_handle({text, RawMessage}, Req, State) ->
    Reply = case decode_msg(RawMessage) of
                invalid_type -> invalid_type;
                {Type, Value} -> handle_message(Type, Value)
            end,

    {reply, {text, encode_msg(Reply)}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

% Login

%% now this login will register if does not exist
handle_message(sign_up, {[{<<"name">>, Name}, {<<"password">>, Password}]}) ->
    case phoenix_user:sign_up(Name, Password) of
        already_registered ->
            {sign_up, already_registered};
        {ok, UserId} ->
            {sign_up, UserId}
    end;

%% case should use for logging
handle_message(log_in, {[{<<"name">>, Name}, {<<"password">>, Password}]}) ->
    case phoenix_user:log_in(Name, Password) of
        {ok, UserId} ->
            {log_in, UserId};
        Error ->
            {log_in, Error}
    end;

handle_message(log_in, {[{<<"id">>, Id}]}) ->
    case phoenix_user:find_by_id(Id) of
        not_found ->
            {login, not_registered};
        User ->
            {login, User}
    end;

% Get all
handle_message(get_user_data, {[{<<"id">>, Id}]}) ->
    case phoenix_user:find_by_id(Id) of
        not_found ->
            {get_user_data, not_found};
        User ->
            Items = phoenix_item:find_by_owner(User#phoenix_user.id),
            {get_user_data, {User, Items}}
    end;

% Create
handle_message(add_item, {[{<<"id">>, UserId},
                           {<<"details">>, {[{<<"description">>, Description},
                                             {<<"done">>, Done}]}}]}) ->
    {ok, Item} = phoenix_item:create(#phoenix_item_details{description = Description,
                                                           done = Done}, UserId),
    {add_item, Item};

handle_message(update_item, {[{<<"id">>, Id},
                              {<<"details">>, {[{<<"description">>, Description},
                                                {<<"done">>, Done}]}},
                              {<<"owner">>, Owner}]}) ->
    {ok, Item} = phoenix_item:update(#phoenix_item{id = Id,
                                                   details = #phoenix_item_details{description = Description,
                                                                                   done = Done},
                                                   owner = Owner}),
    {update_item, Item};

handle_message(delete_item, Id) ->
    Id = phoenix_item:delete(Id),
    {delete_item, Id};

% Unhandled
handle_message(Type, Value) ->
    io:format("=======================================================~n"),
    io:format("Unhandled request: ~n---------------~nType: ~p~nValue: ~p~n", [Type, Value]),
    io:format("=======================================================~n"),
    unhandled.

% private

decode_msg(Msg) ->
    case jiffy:decode(Msg) of
        {[{<<"type">>, Type}, {<<"value">>, Value}]} ->
            {list_to_atom(binary_to_list(Type)), Value};
        _ ->
            invalid_format
    end.

encode_msg({Type, Value}) ->
    jiffy:encode({[{<<"type">>, Type}, {<<"value">>, to_ejson(Value)}]});
encode_msg(Type) ->
    jiffy:encode({[{<<"type">>, Type}]}).

to_ejson([X]) -> [to_ejson(X)];
to_ejson([X|T]) -> [to_ejson(X)|to_ejson(T)];
%% TODO tuple -> record. is not correct
to_ejson(X) when is_tuple(X) -> record_to_ejson(X);
%% TODO
to_ejson(X) when is_atom(X) -> X;
to_ejson(X) -> X.

%% TODO this is not a simply record
record_to_ejson({User, Items}) when is_record(User, phoenix_user) ->
    {[{<<"id">>, User#phoenix_user.id},
      {<<"name">>, User#phoenix_user.name},
      {<<"items">>, to_ejson(Items)}]};
record_to_ejson(User) when is_record(User, phoenix_user) ->
    {[{<<"id">>, User#phoenix_user.id},
      {<<"name">>, User#phoenix_user.name}]};
record_to_ejson(Item) when is_record(Item, phoenix_item) ->
    {[{<<"id">>, Item#phoenix_item.id},
      {<<"details">>, to_ejson(Item#phoenix_item.details)},
      {<<"owner">>, Item#phoenix_item.owner}]};
record_to_ejson(Details) when is_record(Details, phoenix_item_details) ->
    {[{<<"description">>, Details#phoenix_item_details.description},
      {<<"done">>, Details#phoenix_item_details.done}]}.


%%------------------------------------
%%               Test
%%------------------------------------

-ifdef(EUNIT).

to_json_test() ->
    ?assert(to_ejson([]) == []),
    ?assert(to_ejson([atom]) == [atom]),
    Id = ?GENERATE_TOKEN,
    User = #phoenix_user{name = "Sample", id = Id},
    ?assert(to_ejson(User) == {[{<<"id">>, Id}, {<<"name">>, "Sample"}]}).

-endif.
