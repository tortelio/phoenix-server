-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type uuid() :: binary().

-record(phoenix_user, {id :: uuid(), name :: binary()}).
-record(phoenix_user_log, {id :: uuid(), user_id :: uuid(), action, time}).
-record(add, {item_id :: uuid()}).

-record(phoenix_item_details, {description :: binary(), done :: boolean()}).
-record(phoenix_item, {id :: uuid(), details :: #phoenix_item_details{}, owner :: uuid()}).
-record(phoenix_item_log, {id :: uuid(), item_id :: uuid(), action, time}).
-record(create, {item}).
-record(update, {field, value}).

-define(GENERATE_TOKEN, list_to_binary(uuid:to_string(uuid:uuid5(uuid:uuid4(), "phoenix")))).

-define(TIMEOUT, 5000).

-define(MODELS, [phoenix_user, phoenix_item]).
-define(TABLES, [phoenix_users, phoenix_items]).
