-module(test_ws_client).
-behaviour(gen_server).

-export([start_link/1,
         stop/1]).
-export([send_4_answer/1,
         send_4_answer/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link(Config) ->
    Name = maps:get(name, Config, ?MODULE),
    {ok, _} = application:ensure_all_started(gun),
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

stop(Config) ->
    Name = maps:get(name, Config, ?MODULE),
    gen_server:call(Name, stop).

send_4_answer(Msg) ->
    send_4_answer(?MODULE, Msg).
send_4_answer(Name, Msg) ->
    gen_server:call(Name, {send_4_answer, Msg}).

-record(state, {client :: pid(), from :: pid()}).

init(#{url := {Url, Path}, port := Port}) ->
    {ok, Pid} = gun:open(Url, Port),
    gun:ws_upgrade(Pid, Path),
    {ok, #state{client = Pid}}.

handle_call({send_4_answer, Msg}, From, #state{client = Client} = State) ->
    case Msg of
        Msg when is_binary(Msg) ->
            ok = gun:ws_send(Client, {binary, Msg});
        Msg when is_list(Msg) ->
            ok = gun:ws_send(Client, {text, Msg})
    end,
    {noreply, State#state{from = From}};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({gun_up, _ConnPid, http}, State) ->
    {noreply, State};

handle_info({gun_ws_upgrade, _ConnPid, ok, _Headers}, State) ->
    {noreply, State};

handle_info({gun_ws, _ConnPid, {_, Message}}, #state{from = From} = State) ->
    gen_server:reply(From, Message),
    {noreply, State#state{from = undefined}};

handle_info({gun_down, _ConnPid, _, _, _, _} = Message, #state{from = From} = State) ->
    gen_server:reply(From, Message),
    {noreply, State#state{from = undefined}};

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
