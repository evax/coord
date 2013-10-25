-module(coord_service).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {init, 3},
     {nodeup, 2},
     {nodedown, 2}
    ];
behaviour_info(_) ->
    undefined.
