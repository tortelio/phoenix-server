-module(phoenix_item_log).
-include("phoenix_internal.hrl").

-export([create/2]).

create(Action, Item) ->
    defaults(
      #phoenix_item_log{
         item_id = Item#phoenix_item.id,
         action = compose_action(Action, Item),
         clock = Item#phoenix_item.clock
        }
     ).

compose_action(create, Item) ->
    #item_create{item = Item};
compose_action(update, Item) ->
    #item_update{item = Item};
compose_action(fork, Item) ->
    #item_fork{item = Item};
compose_action(delete, Item) ->
    #item_delete{item = Item};

compose_action(Action, Item) ->
    throw({unknown_action, Action, Item}).

%%==============================================================================
%% @private
%%==============================================================================

defaults(#phoenix_item_log{id = undefined} = ItemLog) ->
    defaults(ItemLog#phoenix_item_log{id = ?GENERATE_TOKEN});
defaults(#phoenix_item_log{time = undefined} = ItemLog) ->
    defaults(ItemLog#phoenix_item_log{time = ?NOW});
defaults(ItemLog) when is_record(ItemLog, phoenix_item_log) ->
    ItemLog.
