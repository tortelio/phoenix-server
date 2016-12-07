-module(test_wsc).
-behaviour(websocket_client_handler).

%% API
-export([start_link/1,
         stop/0]).

%% Websocket client interface
-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

start_link(#{url := Url}) ->
    websocket_client:start_link(Url, ?MODULE, []).

stop() ->
    websocket_client:cast(self(), close).

init([], _ConnState) ->
    {ok, null}.

websocket_handle({send_4_answer, From, Message}, _ConnState, null) ->
    io:format("WebSocket handle Send: ~p~n", [Message]),
    {reply, {text, jiffy:encode(Message)}, From};

%% Answer for a send
websocket_handle({text, Message}, _ConnState, Client) when is_pid(Client) ->
    io:format("WebSocket handle Text: ~p~n", [Message]),
    Client ! {answer, jiffy:decode(Message)},
    {noreply, null};

%% getting msg
websocket_handle({text, Message}, _ConnState, null) ->
    io:format("WebSocket handle Text: ~p~n", [Message]),
    {noreply, null};

websocket_handle(close, _ConnState, State) ->
    io:format("WebSocket is closing.~n"),
    {close, <<>>, State}.

websocket_info(X, _ConnState, State) ->
    io:format("WebSocket Info: ~p~n", [X]),
    {reply, {text, <<"ok">>}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n", [State, Code, Payload]),
    ok;

websocket_terminate(X, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih unknown message: ~p", [State, X]),
    ok.
