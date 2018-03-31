-module(dij).
-export([table/2,route/2, update/4, iterate/3 ]).


% Uses sorting to determine the length of the shortest path to the node
% return 0 if the node is not found
entry(Node,Sorted) ->
    case lists:keyfind(Node,1,Sorted) of
	{_,Length,_} ->
	    Length;
	false -> 0 %  coz no direct access
    end.

% Replaces the entry for Node with a new entry
replace(Node, N, Gateway, Sorted) ->
    NewEntry = lists:keydelete(Node,1,Sorted),
    lists:keysort(2,[{Node, N, Gateway} | NewEntry]).

% Update "Sorted"
update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if
	N < Length ->
	    replace(Node,N,Gateway,Sorted);
	true -> Sorted
    end.

% Cnstructs a table with a sorted list of cities , hops and gateways
iterate(Sorted, Map, Table) ->
    case Sorted of
	[] ->
	    Table;
	[{_,inf,_} | _] ->
	    Table;
	[Node,_, Gateway| Tail] ->
    case lists:keyfind(Node,1,Map) of
      {_,ReachableNodes} -> % for each of these nodes update the Sorted list.
        NewSortedList = lists:foldl(fun(ElementX,SortedList) ->
          update(ElementX,1,Gateway,SortedList) end,Tail, ReachableNodes);
      false ->
        NewSortedList = Tail
	    end, %iterate until all nodes are updated in the Soted list
	    iterate(NewSortedList,Map,[{Node,Gateway} | Table])
    end.

% Generate Routing table
table(Gateways, Map) ->
  Nodes = map:all_nodes(Map),
  InfiniteL = lists:map(fun(Node) -> {Node, inf, unknown} end, Nodes),
  GatewayList = lists:map(fun(Gateway) -> {Gateway, 0, Gateway} end, Gateways),
  CompileL = lists:append(InfiniteL, GatewayList),
  SortedL = lists:keysort(2, CompileL),
  iterate(SortedL, Map, []).

    % AllNodes = map:all_nodes(Map),
    % InitSortedList = lists:keysort(2, lists:map(fun(Node) ->
		% 					case lists:member(Node, Gateways) of
		% 					    true ->
		% 						{Node, 0, Node};
		% 					    false ->
		% 						{Node, inf, unknown}
		% 					end
		% 				end, AllNodes)),
    % iterate(InitSortedList, Map, []).


% Find suitable gateway in the routing table
route(Node, Table) ->
  Search = lists:keyfind(Node,1,Table),
  case  Search of
    {_, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
    end.
