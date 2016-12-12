-module(ws_handler).
-include("phoenix_internal.hrl").

-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).
-export([terminate/3]).

-record(state, {active_user :: list()}).

init(Req, []) ->
    {cowboy_websocket, Req, #state{}}.

websocket_handle(Message, Req, State1) when is_tuple(Message) ->
    ?INFO("=====================================================~n"),
    ?INFO("HANDLE MESSAGE: ~p, ~p~n", [Message, State1]),
    try
        {ok, Type, Value} = decode_msg(Message),
        try
            {Reply, State2} = handle_message(Type, Value, State1),
            ?INFO("COMPOSE REPLY: ~p, ~p~n", [Type, Reply]),
            {compose_reply(Type, Reply), State2}
        of
            {ComposedReply, State3} ->
                EncodedReply = encode_msg(ComposedReply),
                ?INFO("REPLY MESSAGE: ~p~nREPLY STATE: ~p~n", [EncodedReply, State3]),
                ?INFO("=====================================================~n"),
                {reply, {text, EncodedReply}, Req, State3}
        catch
            throw:unhandled_message ->
                ?WARNING("Unhandled message: ~p~n", [Message]),
                ?INFO("=====================================================~n"),
                {stop, State1}
        end
    of
        FinalReply -> FinalReply
    catch
        throw:invalid_message ->
            ?WARNING("Invalid message: ~p~n", [Message]),
            ?INFO("=====================================================~n"),
            {stop, State1};

        Class:Reason ->
            ?WARNING("Unhandled exception: Class: ~p, Reason: ~p, Stacktrace: ~p~n",
                     [Class, Reason, erlang:get_stacktrace()]),
            ?INFO("=====================================================~n"),
            {stop, State1}
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

%% User sign up ----------------------------------------------------------------

handle_message(sign_up, #{<<"name">> := Name, <<"password">> := Password}, State) ->
    ok = phoenix_user:sign_up(Name, Password),
    {ok, State};

%% User log in -----------------------------------------------------------------

handle_message(log_in, #{<<"name">> := Name, <<"password">> := Password}, State) ->
    case phoenix_user:log_in(Name, Password) of
        {ok, UserId} ->
            {{ok, UserId}, State#state{active_user = UserId}};
        {error, Reason} ->
            {{error, Reason}, State}
    end;

%% User log out ----------------------------------------------------------------

handle_message(log_out, #{<<"user_id">> := UserId}, #state{active_user = UserId} = State) ->
    {ok, State#state{active_user = undefined}};

handle_message(log_out, #{<<"user_id">> := _UserId}, #state{active_user = _ActiveUserId} = State) ->
    {{error, non_active_user}, State};

%% Get all data ----------------------------------------------------------------

%% Log in again, without direct log in.
handle_message(get_user_data, #{<<"user_id">> := Id} = Value, #state{active_user = undefined} = State) ->
    handle_message(get_user_data, Value, State#state{active_user = Id});
handle_message(get_user_data, #{<<"user_id">> := Id}, State) ->
    case phoenix_user:get_by_id(Id) of
        undefined ->
            {{error, not_found}, State};
        User ->
            Items = db_phoenix_item:find_by_owner(User#phoenix_user.id),
            ExtItems = db_phoenix_item:find_by_not_owner(User#phoenix_user.id),
            {{ok, User, Items, ExtItems}, State}
    end;

%%==============================================================================
%% ITEMS
%%==============================================================================

% Create
handle_message(add_item, #{<<"owner">> := UserId,
                           <<"title">> := Title}, State) ->

    {ok, Item} = phoenix_item:create(Title, UserId),
    {{ok, Item}, State};

handle_message(update_item, #{<<"item_id">> := ItemId,
                              <<"title">> := Title,
                              <<"done">> := Done}, State) ->
    ok = phoenix_item:update(ItemId, Title, Done),
    {ok, State};

handle_message(finish_item, #{<<"item_id">> := ItemId}, State) ->
    ok = phoenix_item:finish(ItemId),
    {ok, State};

handle_message(delete_item, #{<<"item_id">> := ItemId}, State) ->
    ok = phoenix_item:delete(ItemId),
    {ok, State};

handle_message(fork_item, #{<<"item_id">> := ItemId}, #state{active_user = ActiveUserId} = State) ->
    ok = phoenix_item:fork(ItemId, ActiveUserId),

    {ok, State};

%%==============================================================================
%% OTHER
%%==============================================================================

% Unhandled
handle_message(_Type, _Value, _State) -> throw(unhandled_message).


%%==============================================================================
% private
%%==============================================================================

%% Decode WS message -----------------------------------------------------------
decode_msg({_, Msg}) ->
    case jiffy:decode(Msg, [return_maps]) of
        #{<<"type">> := Type, <<"value">> := Value} = X->
            ?INFO("Decoded msg: ~p~n", [X]),
            {ok, ?B2A(Type), Value};
        DecodedMsg ->
            throw({invalid_message, DecodedMsg})
    end.

%% Compose reply ---------------------------------------------------------------

compose_reply(sign_up, ok) ->
    sign_up;
compose_reply(log_in, {ok, UserId}) ->
    {log_in, ?COMPOSE_STRING(UserId)};
compose_reply(log_in, {error, Reason}) ->
    {log_in, Reason};
compose_reply(log_out, ok) ->
    log_out;
compose_reply(get_user_data, {ok, User, Items, ExtItems}) ->
    {get_user_data, ?COMPOSE_USER_DATA(User, Items, ExtItems)};
compose_reply(add_item, {ok, Item}) ->
    {add_item, ?COMPOSE_ITEM(Item)};
compose_reply(update_item, ok) ->
    update_item;
compose_reply(delete_item, ok) ->
    delete_item;
compose_reply(fork_item, ok) ->
    fork_item.

%% Encode reply to WS message --------------------------------------------------

encode_msg({Type, Value}) ->
    ?INFO("Encode value: ~p~n", [Value]),
    jiffy:encode(#{type => Type, value => Value});
encode_msg(Type) ->
    jiffy:encode(#{type => Type}).
