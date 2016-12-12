-module(phoenix_item).
-include("phoenix_internal.hrl").

-export([create/2,
         update/3,
         delete/1,
         fork/2]).

-export([defaults/1]).


%%==============================================================================
%% Implementation
%%==============================================================================

%% Create ----------------------------------------------------------------------

-ifdef(TEST).
-export([create/1]).

create(Item) ->
    ?TRANSACTION(
       begin
           ItemLog = phoenix_item_log:create(create, Item),

           ok = db_phoenix_item:save(Item),
           ok = db_phoenix_item_log:save(ItemLog),

           {ok, Item}
       end).

-endif.

create(Title, Owner) ->
    ?TRANSACTION(
       begin
           Item = do_create(Title, Owner),
           ItemLog = phoenix_item_log:create(create, Item),

           ok = db_phoenix_item:save(Item),
           ok = db_phoenix_item_log:save(ItemLog),

           {ok, Item}
       end).

do_create(Title, Owner) ->
    defaults(
      #phoenix_item{
         title = Title,
         owner = Owner
        }
     ).

%% Modify item -----------------------------------------------------------------

update(ItemId, Title, Done) ->
    ?TRANSACTION(
       begin
           OriginalItem = get_by_id(ItemId),
           UpdatedItem = do_update(OriginalItem, Title, Done),
           ItemLog = phoenix_item_log:create(update, UpdatedItem),

           ok = db_phoenix_item:save(UpdatedItem),
           ok = db_phoenix_item_log:save(ItemLog)
       end).

do_update(OriginalItem, Title, Done) ->
    ClockEvent = itc:event(OriginalItem#phoenix_item.clock),
    OriginalItem#phoenix_item{title = Title,
                              done = Done,
                              clock = ClockEvent}.

%% Finish ----------------------------------------------------------------------

finish(ItemId) ->
    ?TRANSACTION(
       begin
           OriginalItem = get_by_id(ItemId),
           FinishedItem = do_finish(OriginalItem),
           ItemLog = phoenix_item_log:create(finish, FinishedItem),

           ok = db_phoenix_item:save(FinishedItem),
           ok = db_phoenix_item_log:save(ItemLog)
       end).

do_finish(#phoenix_item{done = false} = OriginalItem) ->
    ClockEvent = itc:event(OriginalItem#phoenix_item.clock),
    OriginalItem#phoenix_item{done = true,
                              clock = ClockEvent};
do_finish(#phoenix_item{done = true}) ->
    throw({error, try_to_finish_a_finished_item}).

%% Fork ------------------------------------------------------------------------

fork(ItemId, NewOwner) ->
    ?TRANSACTION(
       begin
           OriginalItem = get_by_id(ItemId),

           {Clock1, Clock2} = itc:fork(OriginalItem#phoenix_item.clock),

           Item1 = OriginalItem#phoenix_item{clock = Clock1},
           Item1Log = phoenix_item_log:create(fork, Item1),

           %% TODO refactor
           Item2 = defaults(
                     Item1#phoenix_item{
                       id = undefined,
                       owner = NewOwner,
                       clock = Clock2
                      }),
           Item2Log = phoenix_item_log:create(create, Item2),

            ok = db_phoenix_item:save(Item1),
            ok = db_phoenix_item_log:save(Item1Log),

            ok = db_phoenix_item:save(Item2),
            ok = db_phoenix_item_log:save(Item2Log),

            ok
       end).

get_by_id(ItemId) ->
    case db_phoenix_item:find_by_id(ItemId) of
        [] -> undefined;
        [Item] -> Item;
        _ -> throw({impossible, more_items})
    end.

%% Delete ---------------------------------------------------------------------

delete(ItemId) ->
    ?TRANSACTION(
       begin
           Item = get_by_id(ItemId),
           ItemLog = phoenix_item_log:create(delete, Item),

           ok = db_phoenix_item:delete(ItemId),
           ok = db_phoenix_item_log:save(ItemLog),

           ok
       end).

%%==============================================================================
%% @private
%%==============================================================================

defaults(#phoenix_item{id = undefined} = Item) ->
    defaults(Item#phoenix_item{id = ?GENERATE_TOKEN});
defaults(#phoenix_item{id = Id, item_id = undefined} = Item) ->
    defaults(Item#phoenix_item{item_id = Id});
defaults(#phoenix_item{clock = undefined} = Item) ->
    defaults(Item#phoenix_item{clock = itc:seed()});
defaults(Item) when is_record(Item, phoenix_item) ->
    Item.
