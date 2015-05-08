-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2, find_by_item_owner/1, get_all_items/0, switch/1]).

-export([migrate/2]).

create(Description, Owner) ->
    [User] = mnesia:activity(transaction, fun() -> mnesia:read(phoenix_users, Owner, read) end),
    io:format("user: ~p~n", [User]),
    UserClock = itc:event(User#phoenix_user.clock),
    ItemSeed = itc:seed(),
    Now = now(),
    ItemId = ?GENERATE_TOKEN,
    Item = #phoenix_item{id = ItemId,
                         details = #phoenix_item_details{description = Description,
                                                         done = false},
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
        [{phoenix_item, ItemId, {phoenix_item_details, Description, Done}, Clock, _Owner} = Item] = mnesia:read(phoenix_items, ItemId, write),
        UpdatedClock = itc:event(Clock),
        UpdatedItem = Item#phoenix_item{details = #phoenix_item_details{
                                                     description = Description,
                                                     done = not Done},
                                        clock = UpdatedClock},
        ItemLog = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                    item_id = ItemId,
                                    action = #item_update{field = done,
                                                          value = not Done},
                                    time = now(),
                                    clock = UpdatedClock},
        mnesia:write(phoenix_items, UpdatedItem, write),
        mnesia:write(phoenix_items_log, ItemLog, write),
        {ok, {[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, not Done}]}}
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
