-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include("phoenix_db.hrl").

-define(APPLICATION, phoenix).

% TODO
-type uuid() :: binary().

-record(phoenix_user, {id :: uuid(), name :: binary(), password :: binary(), clock}).
-record(phoenix_user_log, {id :: uuid(), user_id :: uuid(), action, time, clock}).
-record(user_add_item, {item_id :: uuid()}).
-record(user_delete_item, {item_id :: uuid()}).
-record(user_fork_item, {item_id :: uuid()}).
-record(user_join_item, {item_id :: uuid(), source :: uuid()}).

-record(phoenix_item_details, {description :: binary(), done :: boolean()}).
-record(phoenix_item, {id :: uuid(), title :: list(), description :: list(), clock, owner :: uuid()}).
-record(phoenix_item_log, {id :: uuid(), item_id :: uuid(), action, time, clock}).
-record(item_create, {item}).
-record(item_delete, {item}).
-record(item_update, {details}).
-record(item_fork, {}).

-define(GENERATE_TOKEN, uuid:to_string(uuid:uuid5(uuid:uuid4(), "phoenix"))).
-define(NOW, erlang:system_time()).

-define(INFO(Format, Data), io:format(Format, Data)).
-define(WARNING(Format, Data), io:format(Format, Data)).

-define(B2A(Binary), erlang:binary_to_atom(Binary, utf8)).
