-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("phoenix_db.hrl").
-include("phoenix_item.hrl").

-define(APPLICATION, phoenix).

-type uuid() :: binary().

-record(phoenix_user, {id :: uuid(), name :: binary(), password :: binary(), clock}).

-define(GENERATE_TOKEN, erlang:list_to_binary(phoenix_helper:uuid("phoenix"))).
-define(NOW, erlang:system_time()).

-define(INFO(Format), io:format(Format)).
-define(INFO(Format, Data), io:format(Format, Data)).
-define(WARNING(Format, Data), io:format(Format, Data)).

-define(B2A(Binary), erlang:binary_to_atom(Binary, utf8)).
-define(L2B(List), erlang:list_to_binary(List)).


-define(COMPOSE_STRING(List), List).
-define(COMPOSE_BOOLEAN(Boolean), Boolean).

-define(COMPOSE_USER_DATA(User, Items, ExtItems),
        #{<<"user">> =>         ?COMPOSE_USER(User),
          <<"items">> =>        [?COMPOSE_ITEM(Item) || Item <- Items],
          <<"ext_items">> =>    [?COMPOSE_ITEM(ExtItem) || ExtItem <- ExtItems]}).

-define(COMPOSE_USER(User),
        #{<<"id">> =>           ?COMPOSE_STRING(User#phoenix_user.id),
          <<"name">> =>         ?COMPOSE_STRING(User#phoenix_user.name)}).

-define(COMPOSE_ITEM(Item),
        #{<<"id">> =>           ?COMPOSE_STRING(Item#phoenix_item.id),
          <<"item_id">> =>      ?COMPOSE_STRING(Item#phoenix_item.item_id),
          <<"title">> =>        ?COMPOSE_STRING(Item#phoenix_item.title),
          <<"done">> =>         ?COMPOSE_BOOLEAN(Item#phoenix_item.done),
          <<"owner">> =>        ?COMPOSE_STRING(Item#phoenix_item.owner)}).

