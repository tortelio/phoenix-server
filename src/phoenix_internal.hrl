-type uuid() :: binary().

-record(phoenix_user, {id :: uuid(), name :: binary()}).
-record(phoenix_item_details, {description :: binary(), done :: boolean()}).
-record(phoenix_item, {id :: uuid(), details :: #phoenix_item_details{}, owner :: uuid()}).

-define(GENERATE_TOKEN, list_to_binary(uuid:to_string(uuid:uuid5(uuid:uuid4(), "phoenix")))).

-define(TIMEOUT, 5000).

-define(MODELS, [phoenix_user, phoenix_item]).
-define(TABLES, [phoenix_users, phoenix_items]).
