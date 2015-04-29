-module(ws_handler).
-include("phoenix_internal.hrl").

-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).
-export([terminate/3]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_handle({text, Message}, Req, State) ->
    {[Type|Arguments]} = jiffy:decode(Message),
     Reply = handle_json_message(Type, Arguments),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
% Login
handle_json_message({<<"type">>, <<"login">>} = Type, [{<<"name">>, Name}]) ->
    case phoenix_user:find_by_user_name(Name) of
        [{phoenix_user, UserId, UserName}] ->
            ok;
        [] ->
            {ok, {phoenix_user, UserId, UserName}} = phoenix_user:create(Name)
    end,
    jiffy:encode({[Type, {<<"user">>, {[{<<"name">>, UserName}, {<<"id">>, UserId}]}}]});

handle_json_message({<<"type">>, <<"login">>} = Type, [{<<"id">>, Id}]) ->
    case phoenix_user:find_by_user_id(Id) of
        [{phoenix_user, UserId, UserName}] ->
            jiffy:encode({[Type, {<<"user">>, {[{<<"name">>, UserName}, {<<"id">>, UserId}]}}]});
        [] ->
            jiffy:encode({[Type, {<<"notFound">>, false}]})
    end;

% Get all
handle_json_message({<<"type">>, <<"getAll">>} = Type, [{<<"id">>, UserId}]) ->
    Records = phoenix_item:find_by_item_owner(UserId),
    Items = case Records of
        [] -> [];
        _ -> records_to_ejson(Records)
    end,
    jiffy:encode({[Type, {<<"items">>, Items}]});

% Create
handle_json_message({<<"type">>, <<"create">>} = Type, [{<<"item">>, {[{<<"description">>, Description}, {<<"owner">>, Owner}]}}]) ->
    {ok, {phoenix_item, ItemId, {phoenix_item_details, Description, Done}, Owner}} = phoenix_item:create(Description, Owner),
    Item = {[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, Done}, {<<"owner">>, Owner}]},
    jiffy:encode({[Type, {<<"item">>, Item}]});

% Set
handle_json_message({<<"type">>, <<"set">>} = Type, [{<<"id">>, ItemId}, {<<"command">>, <<"switch">>}]) ->
    {ok, Item}= phoenix_item:switch(ItemId),
    jiffy:encode({[Type, {<<"item">>, Item}]});

% Unhandled
handle_json_message(Type, Arguments) ->
    io:format("=======================================================~n"),
    io:format("Unhandled request: ~n---------------~nType: ~p~nArguments: ~p~n", [Type, Arguments]),
    io:format("=======================================================~n"),
    jiffy:encode({[Type]}).

% private


records_to_ejson([{phoenix_item, ItemId, {phoenix_item_details, Description, Done}, Owner}|[]]) ->
    [{[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, Done}, {<<"owner">>, Owner}]}];
records_to_ejson([{phoenix_item, ItemId, {phoenix_item_details, Description, Done}, Owner}|T]) ->
    [{[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, Done}, {<<"owner">>, Owner}]}|records_to_ejson(T)].

%%------------------------------------
%%               Test
%%------------------------------------

-ifdef(TEST).
-define(TEST_PORT, 18080).
int_to_bin(X) -> list_to_binary(integer_to_list(X)).
create_list(N) -> create_list(N, []).
create_list(0, Acc) -> Acc;
create_list(N, Acc) ->
    Item = #phoenix_item{id = erlang:iolist_to_binary([<<"id-">>, int_to_bin(N)]),
                         details = #phoenix_item_details{description = erlang:iolist_to_binary([<<"desc-">>, int_to_bin(N)]),
                                                         done = false},
                         owner = erlang:iolist_to_binary([<<"Tester-">>, int_to_bin(N)])},
    create_list(N-1, [Item|Acc]).
records_to_ejson_test() ->
    Records = create_list(3),
    ?assert([{phoenix_item, <<"id-1">>, {phoenix_item_details, <<"desc-1">>, false}, <<"Tester-1">>},
             {phoenix_item, <<"id-2">>, {phoenix_item_details, <<"desc-2">>, false}, <<"Tester-2">>},
             {phoenix_item, <<"id-3">>, {phoenix_item_details, <<"desc-3">>, false}, <<"Tester-3">>}] == Records),

    Ejson = records_to_ejson(Records),
    ?assert([{[{<<"id">>, <<"id-1">>}, {<<"description">>, <<"desc-1">>}, {<<"done">>, false}, {<<"owner">>, <<"Tester-1">>}]},
             {[{<<"id">>, <<"id-2">>}, {<<"description">>, <<"desc-2">>}, {<<"done">>, false}, {<<"owner">>, <<"Tester-2">>}]},
             {[{<<"id">>, <<"id-3">>}, {<<"description">>, <<"desc-3">>}, {<<"done">>, false}, {<<"owner">>, <<"Tester-3">>}]}] == Ejson).

-endif.
