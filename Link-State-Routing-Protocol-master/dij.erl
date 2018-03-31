-module(dij).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).


% returns the length of the shortest path to the Node
entry(Node, Sorted) ->
    Entry = lists:keysearch(Node, 1, Sorted),
    case Entry of
      {value, {Node, Length, _Gateway}} ->
        Length;
      false ->
        0
    end.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway
replace(Node, N, Gateway, Sorted) ->
    Entry = lists:keysearch(Node, 1, Sorted),
    case Entry of
      {value, {Node, _OldLength, _OldGateway}} ->
  	    NewList = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
  	    lists:keysort(2, NewList);
    	false ->
    	    Sorted
        end .

% Compare new Length N with existing Lenght
update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted), % get the length of the existing path
    if N < Length ->
	    replace(Node, N, Gateway, Sorted);
       true ->
	    Sorted
    end.

% Compute Routing table
% if no more entries in the sorted list then the given routing table is complete
iterate([], _Map, Table) ->
    Table;

% If the first entry is a dummy entry with infinite path to a city
% the sorted list is also of infinite length and the given routing table is complete.
iterate([{_Node, inf, _Gateway}|_T], _Map, Table) ->
    Table;

% Otherwise  take the first entry in the sorted list, find the nodes in the
% map reachable from this entry and for each of these nodes update the Sorted list.
iterate([{Node, Length, Gateway}|T], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    NewList = lists:foldl(fun(N, Sorted) -> update(N, Length + 1, Gateway, Sorted) end, T, Reachable),
    iterate(NewList, Map, [{Node, Gateway}|Table]).

% construct a routing table given the gate-ways and a map.
table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    InfList = lists:map(fun(Node) -> {Node, inf, unknown} end, AllNodes),
    SortedList = lists:foldl(fun(Node, L) -> update(Node, 0, Node, L) end, InfList, Gateways),
    iterate(SortedList, Map, []).

route(Node, Table) ->
    Entry = lists:keysearch(Node, 1, Table),
    case Entry of
	{value, {Node, Gateway}} ->
	    {ok, Gateway};
	false ->
	    notfound
    end.
