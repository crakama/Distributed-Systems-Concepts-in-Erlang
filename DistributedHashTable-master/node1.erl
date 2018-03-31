-module(node1).
-export([start/1,start/2]).
-define(Stabilize, 1000).
-define(Timeout, 100000).

% Either start the first node in the ring or connect to an existing ring

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

% Set our predecessor to nil, connect to our successor
%  Schedule the stabilizing procedure; - making sure that stabilize message is sent to ourselves.

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
	node(Id, Predecessor, Successor).

% first node thus set our successor pointer to ourself
connect(Id, nil) ->
	{ok, {Id,self()}};

% connecting to an existing ring so send a "key" message to the 1st Node(W1/successor)
% and wait for a reply
connect(_Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},

	receive
		{Qref, Succ_key} ->
			{ok,{Succ_key,Peer}}
	after ?Timeout ->
								 io:format("Time out: no response~n",[])
	end.

% sends a stabilize message to the process after a specific time
  schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(Id, CurrentPred, Successor) ->
	receive
		{key, Qref, Peer} ->                  % a new node/peer needs to know our key
			Peer ! {Qref, Id},
			node(Id, CurrentPred, Successor);

		{notify, ProposedPred} ->                     % a new node informs us of its existence
			Pred = notify(ProposedPred, Id, CurrentPred),
			node(Id, Pred, Successor);

		{request, NewPeer} ->                  % a predecessor needs to know our predecessor
			request(NewPeer, CurrentPred),
			node(Id, CurrentPred, Successor);

		{status, Current_Pred} ->            % wait 4 our successor inform us about its predecessor
			Succ = stabilize(Current_Pred, Id, Successor),
			node(Id, CurrentPred, Succ);

		 stabilize ->                        % if received "stabilize" message, call stabilize/1
			stabilize(Successor),
			node(Id, CurrentPred, Successor);

		probe ->
			create_probe(Id, Successor),
      node(Id, CurrentPred, Successor);

      {probe, Id, Nodes, T} ->           % check if the ring is actually connected
  			remove_probe(T, Nodes),
  			node(Id, CurrentPred, Successor);

      {probe, Ref, Nodes, T} ->
  			forward_probe(Ref, T, Nodes, Id, Successor),
  			node(Id, CurrentPred, Successor);

		status ->
				io:format("NodeID= ~w   Pred=~w Succ=~w ~n", [Id,CurrentPred,Successor]),
			node(Id, CurrentPred, Successor);

		stop ->
			stop
	end.


 stabilize({_, Succ_pid}) -> % send a request message to its successor
   Succ_pid ! {request, self()}.

 request(NewPeer, CurrentPred) -> % picks up {request, self()} message from node func
   case CurrentPred of
     nil ->
       NewPeer ! {status, nil};
     {Pred_key, Pred_pid} ->
       NewPeer ! {status, {Pred_key, Pred_pid}}
   end.

 create_probe(Id, {_, Succ_pid}) ->
   Time = erlang:system_time(micro_seconds),
   Succ_pid ! {probe, Id, [Id], Time}.

 remove_probe(T, Nodes) ->
     Duration = erlang:system_time(micro_seconds)-T,
     RevNodes = lists:reverse(Nodes),
     io:format("Time in microseconds: ~w, Nodes: ~w~n", [Duration,RevNodes]).

 forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
     Spid ! {probe, Ref, [Id|Nodes], T}.

 % Splits ProposedPred to
 % nil ->  if CurrentPred is nill then return the ProposedPred as the NEW CurrentPred
 % true -> If ProposedPredkey finds that is prposing to itself as pred(=) , or less than succ but greater than CurrentPred
 % false -> If neither of the above satisfies, then current pred will remain CurrentPred,
 % and that CurrentPred is the first node in the ring(1st Node ID is always => Id)

 notify(ProposedPred, Id, CurrentPred) ->
   {ProposedPredkey, ProposedPredpid} = ProposedPred,
    case CurrentPred of
         nil ->             % if CurrentPred is nill then return the ProposedPred as the NEW CurrentPred
            {ProposedPredkey, ProposedPredpid};
        {CurrentPred_key, _} ->
            case key:between(ProposedPredkey, CurrentPred_key, Id) of
                true ->
                    {ProposedPredkey, ProposedPredpid};
                false ->
                    CurrentPred
            end
    end.


%-------------------------------------------------------------------------------------------------------------------%
% stabilize/1 is called when a process receives a "stabilize" message
% True = if Key(CurrentPred_key) < To(Succ_key), and > From ProposedPredId, then CurrentPred_key is Succ of ProposedPredId                                   %
% False =  Otherwise, the successor should know that New node(ProposedPredId) is the legitimate Predesessor%
%-------------------------------------------------------------------------------------------------------------------%

stabilize(CurrentPred, ProposedPredId, Successor) ->
    {Succ_key, Succ_pid} = Successor,
    case CurrentPred of
        nil ->
            Succ_pid ! {notify, {ProposedPredId, self()}}, % if no CurrentPred, notify Succ of our existance
            Successor;

        {ProposedPredId, _} ->
            Successor;                         % Do nothing if Succ points back to us

        {Succ_key, _} ->                       % Notify the Successor of our existence if it points to itself
            Succ_pid ! {notify, {ProposedPredId, self()}},
            Successor;

        {CurrentPred_key, CurrentPred_pid} ->
            case key:between(CurrentPred_key, ProposedPredId, Succ_key) of
                true ->
                    CurrentPred_pid ! {request, self()},
                    CurrentPred;
                false ->
                    Succ_pid ! {notify, {ProposedPredId, self()}},
                    Successor
            end
    end.







  % --------------------------- {CurrentPred_key, CurrentPred_pid} -> ------------------------------------------------%
  % If it points to another Node(CurrentPred), If this Node < Succ, Check if  Node < CurrentPred,                     %
  % If true,the Node should send message to its NEW successor(CurrentPred) n waits to receive {status, Current_Pred}  %
  % If false i.e ,o slide us in between him & CurrPred ,so as to become its pred
