-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2, find_by_id/1, find_by_owner/1, find_by_not_owner/1, get_all/0, update/1, delete/2, fork/2]).

-export([migrate/2]).

create(Details, Owner) ->
    User = phoenix_user:find_by_id(Owner),
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

fork(ItemId, Owner) ->
    [RawItem] = find_by_id(ItemId),

    User2 = phoenix_user:find_by_id(Owner),
    UpdatedUser2 = User2#phoenix_user{clock = itc:event(User2#phoenix_user.clock)},

    User = phoenix_user:find_by_id(RawItem#phoenix_item.owner),
    UpdatedUser = User#phoenix_user{clock = itc:event(User#phoenix_user.clock)},

    {Clock1, Clock2} = itc:fork(RawItem#phoenix_item.clock),
    Item = RawItem#phoenix_item{clock = Clock1},
    ItemLog = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                item_id = ItemId,
                                action = #item_fork{},
                                time = now(),
                                clock = Item#phoenix_item.clock},
    UserLog = #phoenix_user_log{id = ?GENERATE_TOKEN,
                                 user_id = User#phoenix_user.id,
                                 action = #user_fork_item{item_id = Item#phoenix_item.id},
                                 time = now(),
                                 clock = User#phoenix_user.clock},

    Item2 = #phoenix_item{id = ?GENERATE_TOKEN,%Item#phoenix_item.id,
                          details = Item#phoenix_item.details,
                          clock = Clock2,
                          owner = Owner},
    Item2Log = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                 item_id = ItemId,
                                 action = #item_create{item = Item},
                                 time = now(),
                                 clock = Item2#phoenix_item.clock},
    User2Log = #phoenix_user_log{id = ?GENERATE_TOKEN,
                                 user_id = Owner,
                                 action = #user_add_item{item_id = Item2#phoenix_item.id},
                                 time = now(),
                                 clock = User2#phoenix_user.clock},
    Fun = fun() ->
            ok = mnesia:write(phoenix_users, User, write),
            ok = mnesia:write(phoenix_items, Item, write),
            ok = mnesia:write(phoenix_items_log, ItemLog, write),
            ok = mnesia:write(phoenix_users_log, UserLog, write),
            ok = mnesia:write(phoenix_users, User2, write),
            ok = mnesia:write(phoenix_items, Item2, write),
            ok = mnesia:write(phoenix_items_log, Item2Log, write),
            ok = mnesia:write(phoenix_users_log, User2Log, write)
          end,
    mnesia:activity(transaction, Fun),
    {ok, Item, Item2}.

join(ItemId, Owner) ->
    [RawItem] = find_by_id_and_owner(ItemId, Owner).

find_by() ->
    find(#phoenix_item{_ = '_'}).

find_by_id_and_owner(Id, Owner) ->
    find(#phoenix_item{id = Id, owner = Owner, _ = '_'}).

find_by(owner, Owner) ->
    find(#phoenix_item{owner = Owner, _ = '_'});

find_by(id, Id) ->
    find(#phoenix_item{id = Id, _ = '_'}).

find(Filter) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_items, Filter, read)
    end,
    mnesia:activity(transaction, Fun).

find_by_owner(Owner) -> find_by(owner, Owner).
find_by_not_owner(Owner) ->
    Fun = fun() ->
                  Constraits = fun(Item, Acc) when Item#phoenix_item.owner /=  Owner ->
                                       [Item|Acc];
                                  (Item, Acc) ->
                                       Acc
                               end,
                  mnesia:foldl(Constraits, [], phoenix_items)
          end,
    case mnesia:transaction(Fun) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {aborted, Reason}
    end.
find_by_id(Id) -> find_by(id, Id).

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
                  [Item2] = mnesia:match_object(phoenix_items, #phoenix_item{id = Item#phoenix_item.id,
                                                                             owner = Item#phoenix_item.owner,
                                                                             _ = '_'}, read),
                  {UpdatedItem, Log} = update_with_log(Item2, Item),
                  mnesia:write(phoenix_items, UpdatedItem, write),
                  mnesia:write(phoenix_items_log, Log, write),
                  {ok, UpdatedItem}
          end,
    mnesia:activity(transaction, Fun).

delete(ItemId, Owner) ->
    Fun = fun() ->
                  [Item] = mnesia:match_object(phoenix_items, #phoenix_item{id = ItemId,
                                                                            owner = Owner,
                                                                            _ = '_'}, read),
                  mnesia:delete_object(phoenix_items, Item, write),
                  Log = #phoenix_user_log{id = ?GENERATE_TOKEN,
                                          user_id = Item#phoenix_item.owner,
                                          action = #user_delete_item{item_id = ItemId},
                                          time = now(),
                                          clock = Item#phoenix_item.clock},
                  mnesia:write(phoenix_items_log, Log, write),
                  ItemId
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
