-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type uuid() :: binary().

-record(phoenix_user, {id :: uuid(), name :: binary(), password :: binary(), clock}).
-record(phoenix_user_log, {id :: uuid(), user_id :: uuid(), action, time, clock}).
-record(user_add_item, {item_id :: uuid()}).
-record(user_delete_item, {item_id :: uuid()}).
-record(user_fork_item, {item_id :: uuid()}).
-record(user_join_item, {item_id :: uuid(), source :: uuid()}).

-record(phoenix_item_details, {description :: binary(), done :: boolean()}).
-record(phoenix_item, {id :: uuid(), details :: #phoenix_item_details{}, clock, owner :: uuid()}).
-record(phoenix_item_log, {id :: uuid(), item_id :: uuid(), action, time, clock}).
-record(item_create, {item}).
-record(item_delete, {item}).
-record(item_update, {details}).
-record(item_fork, {}).

-define(GENERATE_TOKEN, list_to_binary(uuid:to_string(uuid:uuid5(uuid:uuid4(), "phoenix")))).

-define(TIMEOUT, 5000).

-define(MODELS, [phoenix_user, phoenix_item]).
-define(TABLES, [phoenix_users, phoenix_items]).
