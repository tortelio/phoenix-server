-record(phoenix_item, {item_id :: uuid(),
                       id :: uuid(),
                       title :: list(),
                       description :: list(),
                       done = false :: boolean(),
                       clock,
                       owner :: uuid()}).
-record(phoenix_item_log, {id :: uuid(), item_id :: uuid(), action, time, clock}).
-record(item_create, {item}).
-record(item_update, {item}).
-record(item_delete, {item}).
-record(item_fork, {item}).
