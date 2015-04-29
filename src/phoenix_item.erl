-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2, find_by_item_owner/1, switch/1]).
-export([all/0]).

-export([migrate/2]).

create(Description, Owner) ->
    Item = create_item(Description, Owner),
    UserLog = #phoenix_user_log{id = ?GENERATE_TOKEN,
                                user_id = Owner,
                                action = #add{item_id = Item#phoenix_item.id},
                                time = now()},
    ItemLog = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                item_id = Item#phoenix_item.id,
                                action = #create{item = Item},
                                time = now()},
    Fun = fun() ->
        ok = mnesia:write(phoenix_items, Item, write),
        ok = mnesia:write(phoenix_users_log, UserLog, write),
        ok = mnesia:write(phoenix_items_log, ItemLog, write)
    end,
    mnesia:activity(transaction, Fun),

    {ok, Item}.

find_by_item_owner(Owner) ->
    Fun = fun() ->
        mnesia:match_object(phoenix_items, #phoenix_item{_ = '_', owner = Owner}, read)
    end,
    mnesia:activity(transaction, Fun).

all() ->
    Fun = fun() ->
        Results = mnesia:match_object(phoenix_items, #phoenix_item{_ = '_'}, read),
        Logs = mnesia:match_object(phoenix_items_log, #phoenix_item_log{_ = '_'}, read),
        {{result, Results},{log, Logs}}
    end,
    mnesia:activity(transaction, Fun).

switch(ItemId) ->
    Fun = fun() ->
        [{phoenix_item, ItemId, {phoenix_item_details, Description, Done}, _Owner} = Item] = mnesia:read(phoenix_items, ItemId, write),
        Done2 = not Done,
        Item2 = Item#phoenix_item{details = #phoenix_item_details{
                                              description = Description,
                                              done = Done2}},
        Log = #phoenix_item_log{id = ?GENERATE_TOKEN,
                                item_id = ItemId,
                                action = #update{field = done, value = Done2},
                                time = now()},
        ok = mnesia:write(phoenix_items, Item2, write),
        ok = mnesia:write(phoenix_items_log, Log, write),

        {ok, {[{<<"id">>, ItemId}, {<<"description">>, Description}, {<<"done">>, not Done}]}}
    end,
    mnesia:activity(transaction, Fun).

migrate(up, Nodes) ->
    {atomic, ok} = mnesia:create_table(phoenix_items,
                        [{attributes, record_info(fields, phoenix_item)},
                         {record_name, phoenix_item},
                         {index, [#phoenix_item.owner]},
                         {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(phoenix_items_log,
                        [{attributes, record_info(fields, phoenix_item_log)},
                         {record_name, phoenix_item_log},
                         {disc_copies, Nodes}]).

create_item(Description, Owner) ->
    ItemId = ?GENERATE_TOKEN,
    Details = #phoenix_item_details{description = Description, done = false},
    #phoenix_item{id = ItemId, details = Details, owner = Owner}.

%%------------------------------------
%%               Test
%%------------------------------------
-ifdef(EUNIT).

create_item_test() ->
    Description = <<"Write an awesome test!">>,
    Owner = <<"Tester">>,
    Item = create_item(Description, Owner),
    ?assert(is_binary(Item#phoenix_item.id)),
    ?assert(Item#phoenix_item.details#phoenix_item_details.description == Description),
    ?assert(Item#phoenix_item.owner == Owner).

-endif.
