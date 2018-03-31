-module(map).
-compile(export_all).

% returns an empty map
new() -> [].

% updates the Map to reflect that Node has directional links to all nodes in the list Links
% the old entry is removed
update(Node, Links, Map) ->
  NewMap = lists:keydelete(Node, 1, Map),
  [{Node, Links} | NewMap].

% returns list of nodes directly reachable from Node
reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
      {Node, Links} -> Links;
      false ->  []
  end.

% return a list of all nodes in the map
all_nodes([]) -> [];
all_nodes([{Node, Links} | T ]) ->
  List = [Node, Links | all_nodes(T)],
  lists:usort(lists:flatten(List)).
