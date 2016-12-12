-module(phoenix_helper).
-include("phoenix_internal.hrl").

-export([uuid/1]).

uuid(InitialValue) ->
    uuid:to_string(uuid:uuid5(uuid:uuid4(), InitialValue)).
