%% Database depended header

-define(TIMEOUT, 5000).

-define(MODELS, [db_phoenix_user, db_phoenix_item, db_phoenix_item_log]).
-define(TABLES, [phoenix_users, phoenix_items]).


-define(FILTER_ANY_USER, #phoenix_user{_ = '_'}).
-define(FILTER_ANY_USER(Part), #phoenix_user{_ = '_', Part}).
-define(FILTER__USER__ID(Id), ?FILTER_ANY_USER(id = Id)).
-define(FILTER__USER__NAME(Name), ?FILTER_ANY_USER(name = Name)).

-define(FILTER_ANY_ITEM(Part), #phoenix_item{_ = '_', Part}).
-define(FILTER_ANY_ITEM(Part1, Part2), #phoenix_item{_ = '_', Part1, Part2}).
-define(FILTER__ITEM__ID(Id), ?FILTER_ANY_ITEM(id = Id)).
-define(FILTER__ITEM__OWNER(Owner), ?FILTER_ANY_ITEM(owner = Owner)).

-define(TRANSACTION(Exprs), mnesia:activity(transaction, fun() -> Exprs end)).
