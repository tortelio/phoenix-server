-include_lib("eunit/include/eunit.hrl").

%% Fixtures
-define(USER, "TEST_USER").
-define(PASSWORD, "TEST_PASSWORD").
-define(ITEM_1_TITLE, "TEST_ITEM_1_TITLE").

%% Helper macros
-define(B2L(Binary), erlang:binary_to_list(Binary)).

-define(WS(Type, Value),
        begin
            (fun(__T, __V) ->
                     __M = jiffy:encode(#{type => __T, value => __V}),
                     case test_ws_client:send_4_answer(__M) of
                         __R when is_binary(__R) ->
                             case jiffy:decode(__R, [return_maps]) of
                                 #{<<"type">> := __RT, <<"value">> := __RV} ->
                                     {ok, __RV};
                                 #{<<"type">> := __RT} ->
                                     ok;
                                 X ->
                                     ct:pal("Unknown response: ~p~n", [X]),
                                     undefined
                             end;
                         X ->
                             ct:pal("Unhandled response: ~p~n", [X]),
                             undefined
                     end
             end)(Type, Value)
        end).
