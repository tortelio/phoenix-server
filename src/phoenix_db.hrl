%% Database depended header

-define(TIMEOUT, 5000).

-define(MODELS, [phoenix_user, phoenix_item]).
-define(TABLES, [phoenix_users, phoenix_items]).


-define(FILTER__PH_USER__ID(Id), ?ANY(id = Id)).
-define(FILTER__PH_USER__NAME(Name), ?ANY(name = Name)).
-define(ANY(Part), #phoenix_user{_ = '_', Part}).

-define(TRANSACTION(Exprs), mnesia:activity(transaction, fun() -> Exprs end)).
