-module(db_phoenix_item).
-include("phoenix_internal.hrl").

-export([save/1,
         delete/1]).
-export([find_by_id/1,
         find_by_owner/1,
         find_by_not_owner/1]).

-export([migrate/2]).

save(Item) when is_record(Item, phoenix_item) ->
    mnesia:write(phoenix_items, Item, write).

delete(ItemId) ->
    mnesia:delete(phoenix_items, ItemId, write).

find_by_id(ItemId) ->
    ?TRANSACTION(mnesia:match_object(phoenix_items, ?FILTER__ITEM__ID(ItemId), read)).

find_by_owner(UserId) ->
    ?TRANSACTION(mnesia:match_object(phoenix_items, ?FILTER__ITEM__OWNER(UserId), read)).

find_by_not_owner(UserId) ->
    Fun = fun() ->
                  Constraits = fun(#phoenix_item{owner = Owner} = Item, Acc) when Owner /=  UserId ->
                                       [Item|Acc];
                                  (_Item, Acc) ->
                                       Acc
                               end,
                  mnesia:foldl(Constraits, [], phoenix_items)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {aborted, Reason}
    end.

migrate(up, Nodes) ->
    mnesia:create_table(phoenix_items,
                        [{attributes, record_info(fields, phoenix_item)},
                         {record_name, phoenix_item},
                         {index, [#phoenix_item.owner]},
                         {disc_copies, Nodes}]).
