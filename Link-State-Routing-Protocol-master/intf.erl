-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

  % add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    case lists:member({Name, Ref, Pid}, Intf) of
	true ->
	    Intf;
	false ->
	    [{Name, Ref, Pid} | Intf]
    end.
  % remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

% ind the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
	{Name,_,Pid} ->
	    {ok, Pid};
	false ->
	    notfound
    end.

% find the reference given a name and return {ok,Ref} or notfound.
ref(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
	{Name, Ref,_} ->
	    {ok, Ref};
	false ->
	    notfound
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref,2,Intf) of
	{Name, Ref,_} ->
	    {ok, Name};
	false ->
	    notfound
    end.

list(Intf) ->
    list(Intf, []).
list(Intf, Final) ->
    case Intf of
	[] ->
	    Final;
	[{Name, _, _} | Tail] -> list(Tail, [Name | Final])
    end.

% To know the state of each directly connected links(map changes) flood link state sms to all nodes
broadcast(Message, Intf) ->
    lists:foreach(fun({_,_,Pid}) ->
			  Pid ! Message
      end,
		  Intf).
