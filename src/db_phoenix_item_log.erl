-module(db_phoenix_item_log).
-include("phoenix_internal.hrl").

-export([save/1]).
-export([migrate/2]).

save(ItemLog) when is_record(ItemLog, phoenix_item_log) ->
    mnesia:write(phoenix_items_log, ItemLog, write).

migrate(up, Nodes) ->
    mnesia:create_table(phoenix_items_log,
                        [{attributes, record_info(fields, phoenix_item_log)},
                         {record_name, phoenix_item_log},
                         {index, [#phoenix_item_log.time]},
                         {disc_copies, Nodes}]).
