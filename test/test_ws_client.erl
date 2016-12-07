-module(test_ws_client).
-behaviour(gen_server).

-export([start_link/1,
         stop/0]).
-export([send_4_answer/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link(Config) ->
    {ok, _} = application:ensure_all_started(gun),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_server:call(?MODULE, stop).

send_4_answer(Msg) ->
    gen_server:call(?MODULE, {send_4_answer, Msg}).

-record(state, {client :: pid(), from :: pid()}).

init(#{url := {Url, Path},
       port := Port}) ->
    {ok, Pid} = gun:open(Url, Port),
    X = gun:ws_upgrade(Pid, Path),
    io:format("WS client started: ~p~n", [Pid]),
    {ok, #state{client = Pid}}.

handle_call({send_4_answer, Msg}, From, #state{client = Client} = State) ->
    io:format("Send 4 answer : ~p~n", [Msg]),
    case Msg of
        Msg when is_binary(Msg) ->
            ok = gun:ws_send(Client, {binary, Msg});
        Msg when is_list(Msg) ->
            ok = gun:ws_send(Client, {text, Msg})
    end,
    {noreply, State#state{from = From}};

handle_call(stop, From, State) ->
    {stop, normal, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({gun_up, ConnPid, http}, State) ->
    io:format("test_ws_client handled info: ~p~n", [ConnPid]),
    {noreply, State};

handle_info({gun_ws_upgrade, ConnPid, ok, Headers}, State) ->
    io:format("gun ws upgrade ~p~n", [Headers]),
    {noreply, State};

handle_info({gun_ws, ConnPid, {_, Message}}, #state{from = From} = State) ->
    gen_server:reply(From, Message),
    {noreply, State#state{from = undefined}};

handle_info({gun_down, ConnPid, _, _, _, _}, #state{from = From} = State) ->
    gen_server:reply(From, error),
    {noreply, State#state{from = undefined}};

handle_info(X, State) ->
    io:format("HANDLE_INFO: ~p~n", [X]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
