-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2, find_by_item_owner/1, get_all_items/0, switch/1]).

-export([migrate/2]).

create(Description, Owner) ->
    ItemId = ?GENERATE_TOKEN,
    Item = #phoenix_item{id = ItemId,
                         details = #phoenix_item_details{description = Description,
                                                         done = false},
                         owner = Owner},
    Fun = fun() ->
        ok = mnesia:write(phoenix_items, Item, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, Item}.

find_by_item_owner(Owner) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_items, #phoenix_item{_ = '_', owner = Owner}, read)
    end,
    mnesia:activity(transaction, Fun).

get_all_items() ->
    Fun = fun() ->
        mnesia:match_object(phoenix_items, #phoenix_item{_ = '_'}, read)
    end,
    mnesia:activity(transaction, Fun).

switch(ItemId) ->
    Fun = fun() ->
        [{phoenix_item, ItemId, {phoenix_item_details, Description, Done}, _Owner} = Item] = mnesia:read(phoenix_items, ItemId, write),
        mnesia:write(phoenix_items, Item#phoenix_item{details=#phoenix_item_details{description=Description, done=not Done}}, write),
        {ok, {[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, not Done}]}}
    end,
    mnesia:activity(transaction, Fun).

migrate(up, Nodes) ->
    mnesia:create_table(phoenix_items,
                        [{attributes, record_info(fields, phoenix_item)},
                         {record_name, phoenix_item},
                         {index, [#phoenix_item.owner]},
                         {disc_copies, Nodes}]).
