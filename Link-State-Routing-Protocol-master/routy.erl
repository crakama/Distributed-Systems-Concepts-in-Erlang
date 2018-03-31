-module(routy).
-compile(export_all).

% start and register router process under a unique name
start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

% stop process
% make node unreachable
% takes in the registered name
stop(Node) ->
  Node ! stop,
  unregister(Node).

% initiate a new router
% the initial parameters are empty values
init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dij:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).


router(Name, N, Hist, Intf, Table, Map) ->
  receive

    % message has arrived to final destination
    {route, Name, From, Message} ->
        io:format("~w: received message ~w ~n", [Name, Message]),
        router(Name, N, Hist, Intf, Table, Map);

    % if message is not ours we should forward it
    % if a suitable gateway exist we forward the message to the gateway
    % if not we drop the packet
    {route, To, From, Message} ->
        io:format("~w: routing message (~w)", [Name, Message]),
        case dij:route(To, Table) of
          {ok, Gw} ->
            case intf:lookup(Gw, Intf) of
              {ok, Pid} ->
                  Pid ! {route, To, From, Message};
              notfound ->
                  ok
            end;
          notfound ->
                ok
        end,
        router(Name, N, Hist, Intf, Table, Map);


      % initiate routing of a message without knowing the name of the local router
      {send, To, Message} ->
          self() ! {route, To, Name, Message},
          router(Name, N, Hist, Intf, Table, Map);

        % link-state message tagged with a message number R
        % router checks if it is an old or new message
        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
              {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
            old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;

    {add, Node, Pid} ->
        % use monitor to see if a node is unreachable
        Ref = erlang:monitor(process, Pid),             % send request monitor of type "process", monitor a process with Pid
        Intf1 = intf:add(Node, Ref, Pid, Intf),
        router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
        {ok, Ref} = intf:ref(Node, Intf),
        erlang:demonitor(Ref),                          % turn of monitoring
        Intf1 = intf:remove(Node, Intf),
        router(Name, N, Hist, Intf1, Table, Map);

    % node is unreachable
    {'DOWN', Ref, process, _, _} ->
        {ok, Down} = intf:name(Ref, Intf),
        io:format("~w: exit received from ~w~n", [Name, Down]),
        Intf1 = intf:remove(Down, Intf),                          % remove entry from interface
        router(Name, N, Hist, Intf1, Table, Map);

      % status request
      {status, From} ->
          From ! {status, {Name, N, Hist, Intf, Table, Map}},
          router(Name, N, Hist, Intf, Table, Map);

        % update the routing table
        % done manually when receiving a link-state message or when map changes
        update ->
            Table1 = dij:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);

        % broadcast a link-state message
        % should be done when a link is added
        % updates the map
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);

    stop ->
        ok
  end.

statusRequest(Pid) ->
  Pid ! {status, self()},
  receive
    {status, Status} ->
      io:format("Status is: ~w~n", Status)
  end.
