-module(gms1).
-export([start/1,start/2]).

% Initialize a process that is the first node hence be the leader of the group
% give it(itself/initialized process) an empty list of peers
% let it know that its master(itself) is the only node in the group

start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) ->
  leader(Id, Master, [], [Master]).

% Starting a node that should join an existing group
% send a {join, Master, self()} message to a node in the group
% Wait for an invitation(delivered as a view message)

start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      slave(Id, Master, Leader, Slaves, Group)
    end.

% {mcast, Msg}: a message either from its own master or from a peer node.
% {join, Wrk, Peer}: a request-join message, from a peer or the master
% lists:append(Slaves, [Peer]) new node is added at the end of the list of peers.
% Important, cz the new node has to be the last one to see the view message
% that we send out

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);

    {join, Wrk, Peer} ->
      UpdatedSlaves = lists:append(Slaves, [Peer]),
      UpdatedGroup = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|UpdatedSlaves], UpdatedGroup}, UpdatedSlaves),
      Master ! {view, UpdatedGroup},
      leader(Id, Master, UpdatedSlaves, UpdatedGroup);

    stop ->
      ok
    end.


  slave(Id, Master, Leader, Slaves, Group) ->
    receive
      {mcast, Msg} ->
        Leader ! {mcast, Msg},
        slave(Id, Master, Leader, Slaves, Group);

      {join, Wrk, Peer} ->
        Leader ! {join, Wrk, Peer},
        slave(Id, Master, Leader, Slaves, Group);

      {msg, Msg} ->
        Master ! Msg,
        slave(Id, Master, Leader, Slaves, Group);

      {view, [Leader|UpdatedSlaves], UpdatedGroup} ->
        Master ! {view, UpdatedGroup},
        slave(Id, Master, Leader, UpdatedSlaves, UpdatedGroup);
      stop ->
        ok
      end.

% Send multicast message to each of the processes in a list.
  % bcast(_, Msg, Nodes) ->
  %   F = fun(Node) ->
  %           Node ! Msg
  %       end,
  %   lists:map(F, Nodes).

  bcast(Id, Msg, Nodes) ->
    F = fun(Node) ->
            Node ! Msg
        end,
    lists:foreach(F, Nodes).
