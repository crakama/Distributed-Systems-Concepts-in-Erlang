-module(gms3Old).
-export([start/1,start/2]).
-define(timeout, 500).
-define(arghh,100).

  % Initialize a process that is the first node hence be the leader of the group
  % give it(itself/initialized process) an empty list of peers
  % let it know that its master(itself) is the only node in the group

  start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id,Rnd, Self) end)}.

  init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).

  % A starting node that should join an existing group
  % send a {join, Master, self()} message to a node in the group
  % Wait for an invitation(delivered as a view message)

  start(Id, Grp) ->
    % Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init_join(Id,Grp, Self) end)}.

  init_join(Id, Grp, Master) ->
    % random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Master, Self},

    receive
      {view,N, [Leader|Slaves], Group} ->
        % io:format("InitSlave: ~w: Last~n", [Last]),

        Master ! {view, Group},
        erlang:monitor(process, Leader),
        slave(Id, Master, Leader, N+1, {view,N, [Leader|Slaves], Group}, Slaves, Group)

     after ?timeout ->
       Master ! {error, "no reply from leader"}

      end.

  % {mcast, Msg}: a message either from its own master or from a peer node.
  % {join, Wrk, Peer}: a request-join message, from a peer or the master
  % lists:append(Slaves, [Peer]) new node is added at the end of the list of peers.
  % Important, cz the new node has to be the last one to see the view message
  % that we send out

  leader(Id, Master, N, Slaves, Group) ->
    receive
      {mcast, Msg} ->
        bcast(Id, {msg, N, Msg}, Slaves),
        Master ! Msg,
        leader(Id, Master, N+1, Slaves, Group);

      {join, Wrk, Peer} ->
        UpdatedSlaves = lists:append(Slaves, [Peer]),
        UpdatedGroup = lists:append(Group, [Wrk]),
        bcast(Id, {view, N,  [self()|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves),
        Master ! {view, UpdatedGroup},
        leader(Id, Master, N+1, UpdatedSlaves, UpdatedGroup);

      stop ->
        ok
      end.

% moves to an election state if it detects that a leader has died
    slave(Id, Master, Leader, N, Last, Slaves, Group) ->
      receive
        {mcast, Msg} ->
          Leader ! {mcast, Msg},
          slave(Id, Master, Leader, N, Last, Slaves, Group);

        {join, Wrk, Peer} ->
          Leader ! {join, Wrk, Peer},
          slave(Id, Master, Leader, N, Last, Slaves, Group);

        {msg, I, _} when I < N ->
          % io:format( "Slave: ~w (~w) duplicate discard message~n", [Id,self()]),
          slave(Id, Master, Leader, N, Last, Slaves, Group);

        {msg, N, Msg} ->
          Master ! Msg,           % Foward NewLast to master
          slave(Id, Master, Leader, N+1, {msg, N, Msg},  Slaves, Group);

        % {view, I, _} when I < N ->
        %     slave(Id, Master, Leader, N, Last, Slaves, Group);

        {view, N, [Leader|UpdatedSlaves], UpdatedGroup} ->
          Master ! {view, UpdatedGroup},
          slave(Id, Master, Leader, N+1, {view, N, [Leader|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves, UpdatedGroup);

        {'DOWN', _Ref, process, Leader, _Reason} ->
          election(Id, Master, N, Last, Slaves, Group);

        stop ->
          ok
        end.

    election(Id, Master, N, Last, Slaves, [_|Group]) ->
      Self = self(),
      case Slaves of
        [Self|Rest] -> % means you are the leader
          bcast(Id, Last, Rest),
          bcast(Id, {view, N, Slaves, Group}, Rest),
          Master ! {view, Group},
          leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
          erlang:monitor(process, Leader),
          slave(Id, Master, Leader, N, Last, Rest, Group)
       end.

    %  bcast(Id, Msg, Nodes) ->
    %    F = fun(Node) ->
    %            Node ! Msg,
    %            crash(Id)
    %        end,
    %    lists:foreach(F, Nodes).
    bcast(Id, Msg, Nodes) ->
      lists:foreach(fun(Node) ->  Node ! Msg,crash(Id)end,Nodes).

     crash(Id) ->
       case random:uniform(?arghh) of
         ?arghh ->
           io:format("leader ~w: crashed~n", [Id]),
           exit(no_luck);
         _ ->
           ok
         end.
