-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
  [].

update(Node, Links, Map) ->
  NewMapEntry = lists:keydelete(Node,1,Map),
  [{Node, Links} | NewMapEntry].

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    false -> [];
    % return a list of nodes if you find a match of tuple
    {_,ReturnedListofNodes} -> ReturnedListofNodes

  end.

all_nodes(Map) ->
  lists:foldl(fun({Node, Links}, NewList) ->
                  lists:flatten([Node, Links | NewList])
              end, [], Map).

% Note that the representation of the map should not be known by the users of
% a map.
