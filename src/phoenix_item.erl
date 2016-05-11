-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2, find_by_owner/1, get_all/0, update/1]).

-export([migrate/2]).

create(Details, Owner) ->
    [User] = mnesia:activity(transaction, fun() -> mnesia:read(phoenix_users, Owner, read) end),
    UserClock = itc:event(User#phoenix_user.clock),
    ItemSeed = itc:seed(),
    Now = now(),
    ItemId = ?GENERATE_TOKEN,
    Item = #phoenix_item{id = ItemId,
                         details = Details,
                         clock = ItemSeed,
                         owner = Owner},
    UpdatedUser = User#phoenix_user{clock = UserClock},
    ItemLog = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                item_id = ItemId,
                                action = #item_create{item = Item},
                                time = Now,
                                clock = ItemSeed},
    UserLog = #phoenix_user_log{id = ?GENERATE_TOKEN,
                                user_id = Owner,
                                action = #user_add_item{item_id = ItemId},
                                time = Now,
                                clock = UserClock},
    Fun = fun() ->
        ok = mnesia:write(phoenix_items, Item, write),
        ok = mnesia:write(phoenix_users, UpdatedUser, write),
        ok = mnesia:write(phoenix_items_log, ItemLog, write),
        ok = mnesia:write(phoenix_users_log, UserLog, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, Item}.

find_by() -> find(#phoenix_item{_ = '_'}).
find_by(owner, Owner) -> find(#phoenix_item{owner = Owner, _ = '_'}).
find(Filter) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_items, Filter, read)
    end,
    mnesia:activity(transaction, Fun).

find_by_owner(Owner) -> find_by(owner, Owner).

get_all() -> find_by().

update_with_log(Item, Item2) ->
    UpdatedItem = update_item(Item, Item2),
    Log = #phoenix_item_log{id = ?GENERATE_TOKEN,
                            item_id = UpdatedItem#phoenix_item.id,
                            action = #item_update{details = UpdatedItem#phoenix_item.details},
                            time = now(),
                            clock = UpdatedItem#phoenix_item.clock},
    {UpdatedItem, Log}.

update_item(Item, Item2) ->

    Item#phoenix_item{details = Item2#phoenix_item.details,
                      clock = itc:event(Item#phoenix_item.clock)}.
%update_item(Item, {desciption, Description}) ->
%    Item#phoenix_item{details = #phoenix_item_details{description = Description},
%                      clock = itc:event(Item#phoenix_item.clock)};
%update_item(Item, {done, Done}) ->
%    Item#phoenix_item{details = #phoenix_item_details{done = Done},
%                      clock = itc:event(Item#phoenix_item.clock)}.

update(Item) ->
    Fun = fun() ->
        [Item2] = mnesia:match_object(phoenix_items, #phoenix_item{id = Item#phoenix_item.id, _ = '_'}, read),
        {UpdatedItem, Log} = update_with_log(Item2, Item),
        mnesia:write(phoenix_items, UpdatedItem, write),
        mnesia:write(phoenix_items_log, Log, write),
        {ok, UpdatedItem}
    end,
    mnesia:activity(transaction, Fun).

migrate(up, Nodes) ->
    mnesia:create_table(phoenix_items_log,
                        [{attributes, record_info(fields, phoenix_item_log)},
                         {record_name, phoenix_item_log},
                         {index, [#phoenix_item_log.time]},
                         {disc_copies, Nodes}]),
    mnesia:create_table(phoenix_items,
                        [{attributes, record_info(fields, phoenix_item)},
                         {record_name, phoenix_item},
                         {index, [#phoenix_item.owner]},
                         {disc_copies, Nodes}]).
