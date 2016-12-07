-module(ws_handler).
-include("phoenix_internal.hrl").

-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).
-export([terminate/3]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_handle(Message, Req, State) when is_tuple(Message) ->
    ?INFO("WS server handles message: ~p, ~p~n", [Message, State]),
    try
        {Type, Value} = decode_msg(Message),
        {ok, handle_message(Type, Value)}
    of
        {ok, Reply} -> {reply, {text, encode_msg(Reply)}, Req, State}
    catch
        throw:unhandled_message ->
            ?WARNING("Unhandled message: ~p~n", [Message]),
            {stop, State};
        throw:invalid_message ->
            ?WARNING("Invalid message: ~p~n", [Message]),
            {stop, State};
        Class:Reason ->
            ?WARNING("Unhandled exception: Class: ~p, Reason: ~p, Stacktrace: ~p~n", [Class, Reason, erlang:get_stacktrace()]),
            {stop, State}
    end;

websocket_handle(Data, Req, State) ->
    ?INFO("WS server handle: ~p, ~p, ~p~n", [Data, Req, State]),
    {ok, Req, State, hibernate}.

websocket_info(Info, Req, State) ->
    ?INFO("WS server info: ~p, ~p, ~p~n", [Info, Req, State]),
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
    ok.

%%==============================================================================
%% USERS
%%==============================================================================

% Login

%% now this login will register if does not exist
handle_message(sign_up, #{<<"name">> := Name, <<"password">> := Password}) ->
    case phoenix_user:sign_up(Name, Password) of
        already_registered ->
            {sign_up, already_registered};
        {ok, UserId} ->
            {sign_up, UserId}
    end;

%% case should use for logging
handle_message(log_in, #{<<"name">> := Name, <<"password">> := Password}) ->
    case phoenix_user:log_in(Name, Password) of
        {ok, UserId} ->
            {log_in, UserId};
        Error ->
            {log_in, Error}
    end;

handle_message(log_in, #{<<"user_id">> := Id}) ->
    case phoenix_user:find_by_id(Id) of
        [] ->
            {login, not_registered};
        [User] ->
            {login, User}
    end;

% Get all
handle_message(get_user_data, #{<<"user_id">> := Id}) ->
    case phoenix_user:find_by_id(Id) of
        [] ->
            {get_user_data, not_found};
        [User] ->
            Items = phoenix_item:find_by_owner(User#phoenix_user.id),
            ExtItems = phoenix_item:find_by_not_owner(User#phoenix_user.id),
            {get_user_data, {User, Items, ExtItems}}
    end;

%%==============================================================================
%% ITEMS
%%==============================================================================

% Create
handle_message(add_item, #{<<"user_id">> := UserId,
                           <<"item">> := #{<<"title">> := Title}}) ->

    {ok, Item} = phoenix_item:create(Title, UserId),
    {add_item, Item};

%handle_message(update_item, {[{<<"id">>, Id},
%                              {<<"details">>, {[{<<"description">>, Description},
%                                                {<<"done">>, Done}]}},
%                              {<<"owner">>, Owner}]}) ->
%    {ok, Item} = phoenix_item:update(#phoenix_item{id = Id,
%                                                   details = #phoenix_item_details{description = Description,
%                                                                                   done = Done},
%                                                   owner = Owner}),
%    {update_item, Item};
%
%handle_message(delete_item, {[{<<"id">>, Id}, {<<"owner">>, Owner}]}) ->
%    Id = phoenix_item:delete(Id, Owner),
%    {delete_item, Id};
%
%handle_message(fork_item, {[{<<"id">>, Id}, {<<"owner">>, Owner}]}) ->
%    {ok, _Item, Item2} = phoenix_item:fork(Id, Owner),
%    {fork_item, Item2};

%%==============================================================================
%% OTHER
%%==============================================================================

% Unhandled
handle_message(_Type, _Value) -> throw(unhandled_message).


%%==============================================================================
% private
%%==============================================================================

decode_msg({_, Msg}) ->
    case jiffy:decode(Msg, [return_maps]) of
        #{<<"type">> := Type, <<"value">> := Value} = X->
            ?INFO("Decoded msg: ~p~n", [X]),
            {?B2A(Type), Value};
        DecodedMsg ->
            throw({invalid_message, DecodedMsg})
    end.

encode_msg({Type, Value}) ->
    jiffy:encode(#{type => Type, value => to_ejson(Value)}]});
encode_msg(Type) ->
    jiffy:encode(#{type => Type}).

to_ejson([X]) -> [to_ejson(X)];
to_ejson([X|T]) -> [to_ejson(X)|to_ejson(T)];
%% TODO tuple -> record. is not correct
to_ejson(X) when is_tuple(X) -> record_to_ejson(X);
%% TODO
to_ejson(X) when is_atom(X) -> X;
to_ejson(X) -> X.

%% TODO this is not a simply record
record_to_ejson({User, Items, ExtItems}) when is_record(User, phoenix_user) ->
    {[{<<"id">>, User#phoenix_user.id},
      {<<"name">>, User#phoenix_user.name},
      {<<"items">>, to_ejson(Items)},
      {<<"ext_items">>, to_ejson(ExtItems)}]};
record_to_ejson(User) when is_record(User, phoenix_user) ->
    {[{<<"id">>, User#phoenix_user.id},
      {<<"name">>, User#phoenix_user.name}]};
record_to_ejson(Item) when is_record(Item, phoenix_item) ->
    {[{<<"id">>, Item#phoenix_item.id},
      {<<"title">>, to_ejson(Item#phoenix_item.title)},
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
