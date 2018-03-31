-module(routy1).
-export([start/2, stop/1,init/1, router/6]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = interfaces:new(),
  Map = map:new(),
  Table = dij:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
	    Intf1 = interfaces:add(Node, Ref, Pid, Intf),
      io:format("~w: Interface: ~w ~n", [Name, Intf1]),
	    router(Name, N, Hist, Intf1, Table, Map);


    {remove, Node} ->
      {ok, Ref} = interfaces:ref(Node, Intf),
      erlang:demonitor(Ref),
	    Intf1 = interfaces:remove(Node, Intf),
	    router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
	    {ok, Down} = interfaces:name(Ref, Intf),
	    io:format("~w: exit recived from ~w~n", [Name, Down]),
	    Intf1 = interfaces:remove(Down, Intf),
	    router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
	    From ! {status, {Name, N, Hist, Intf, Table, Map}},
	    router(Name, N, Hist, Intf, Table, Map);
    stop ->
      ok
    end.

% Handles link-state message
    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          interfaces:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          io:format("~w: Map: ~w ~n", [Name, Map1]),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;


% orders the router to update its routing table.

    update ->
      Table1 = dij:table(interfaces:list(Intf), Map),
      io:format("~w: Table ~w ~n", [Name, Table1]),
	    router(Name, N, Hist, Intf, Table1, Map);

% orders the router to broadcast a link-state message
    broadcast ->
	    Message = {links, Name, N, interfaces:list(Intf)},
	    interfaces:broadcast(Message, Intf),
	    router(Name, N+1, Hist, Intf, Table, Map);

% Handle a message when it has actually arrived to the final destination.
    {route, Name, From, Message} ->
	    io:format("~w: Message Ressage ~w ~n", [Name, Message]),
	    router(Name, N, Hist, Intf, Table, Map);

  % Drop packet if routing entry or an interface of a gateway are not found
    {route, To, From, Message} ->
	    io:format("~w: Routed Message (~w)", [Name, Message]),
	    case dij:route(To, Table) of
        {ok, Gateway} ->
          case interfaces:lookup(Gateway, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              io:format("~w:interfaces notfound (~w)", [Name, Message]),
              ok
          end;
        notfound ->
          io:format("~w:Gateway notfound (~w)", [Name, Message]),
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);
% local user can initiate the routing of a
% message without knowing the name of the local router
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
	    router(Name, N, Hist, Intf, Table, Map);

    stop ->
	    ok;


    end.
