-module(node2).
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
  Store = storage:create(),
	node(Id, Predecessor, Successor,Store).

% first node thus set our successor pointer to ourself
connect(Id, nil) ->
	{ok, {Id,self()}};

% connecting to an existing ring so send a "key" message to the node that
% we have been given and wait for a reply
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

node(Id, Predecessor, Successor,Store) ->
	receive
		{key, ClientQref, Peer} ->                  % a peer needs to know our key
			Peer ! {ClientQref, Id},
			node(Id, Predecessor, Successor,Store);

		{notify, New} ->                     % a new node informs us of its existence
			{Pred, Keep} = notify(New, Id, Predecessor,Store),
			node(Id, Pred, Successor, Keep);

		{request, Peer} ->                  % a predecessor needs to know our predecessor
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);

		{status, Current_Pred} ->            % wait 4 our successor inform us about its predecessor
			Succ = stabilize(Current_Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);
    % stabilize(Current_Pred, Id, Successor)

		 stabilize ->                        % if received "stabilize" message, call stabilize/1
       stabilize(Successor),
       node(Id, Predecessor, Successor,Store);

		probe ->
			create_probe(Id, Successor),
      node(Id, Predecessor, Successor,Store);

      {probe, Id, Nodes, T} ->           % check if the ring is actually connected
  			remove_probe(T, Nodes),
  			node(Id, Predecessor, Successor,Store);

      {probe, Ref, Nodes, T} ->
  			forward_probe(Ref, T, Nodes, Id, Successor),
  			node(Id, Predecessor, Successor,Store);

      {add, Key, Value, ClientQref, Client} ->
        Added = add(Key, Value, ClientQref, Client,
        Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added);

      {lookup, Key, ClientQref, Client} ->
        lookup(Key, ClientQref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store);

      {handover, Elements} -> % Delegate responsibility
        Merged = storage:merge(Store, Elements), % storage:merge(Store, Elements) add a list of key-value pairs to a store
        node(Id, Predecessor, Successor, Merged);

		status ->
				io:format("NodeID= ~w   Pred=~w Succ=~w ~n", [Id,Predecessor,Successor]),
			node(Id, Predecessor, Successor,Store);

		stop ->
			stop
	end.


 stabilize({_, Succ_pid}) -> % send a request message to its successor
   Succ_pid ! {request, self()}.

 request(Peer, Predecessor) -> % picks up {request, self()} message
   case Predecessor of
     nil ->
       Peer ! {status, nil};
     {Pred_key, Pred_pid} ->
       Peer ! {status, {Pred_key, Pred_pid}}
   end.

 create_probe(Id, {_, Succ_pid}) ->
     Succ_pid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

 remove_probe(T, Nodes) ->
     Duration = erlang:system_time(micro_seconds)-T,
     RevNodes = lists:reverse(Nodes),
     io:format("Time: ~w, Nodes: ~w~n", [Duration,RevNodes]).

 forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
     Spid ! {probe, Ref, [Id|Nodes], T}.

 % Case closed if Node(Prodecessor) is set to nill
 % Proposal to be a proper predecessor true--> CurrentPredecessor is > Nodekey(Predecessor)
 % false means Nodekey > CurrentPredecessor, so it will become CurrentPredecessor

 notify({Node_Key, Node_Pid}, Id, Predecessor,Store) ->
    case Predecessor of
        nil ->
          Keep = handover(Id, Store, Node_Key, Node_Pid),
          {{Node_Key, Node_Pid}, Keep};

        {Pred_key, _} ->
            case key:between(Node_Key, Pred_key, Id) of
                true ->
                  Keep = handover(Id, Store, Node_Key, Node_Pid),
                  {{Node_Key, Node_Pid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.


%-------------------------------------------------------------------------------------------------------------------%
% stabilize/1 is called when a process receives a "stabilize" message
% --------------------------- {CurrentPred_key, CurrentPred_pid} -> ------------------------------------------------%
% If it points to another Node(CurrentPred), If this Node < Succ, Check if  Node < CurrentPred,                     %
% If true,the Node should send message to its NEW successor(CurrentPred) n waits to receive {status, Current_Pred}  %
% If false i.e ,o slide us in between him & CurrPred ,so as to become its pred                                      %
% ------------------------------------------------------------------------------------------------------------------%
%-------------------------------------------------------------------------------------------------------------------%

stabilize(CurrentPred, Id, Successor) ->
    {Succ_key, Succ_pid} = Successor,
    case CurrentPred of
        nil ->
            Succ_pid ! {notify, {Id, self()}}, % if no CurrentPred, notify Succ of our existance
            Successor;

        {Id, _} ->
            Successor;                         % Do nothing if Succ points back to us

        {Succ_key, _} ->                       % Notify of our existence if it points to itself
            Succ_pid ! {notify, {Id, self()}},
            Successor;

        {CurrentPred_key, CurrentPred_pid} ->
            case key:between(CurrentPred_key, Id, Succ_key) of
                true ->
                    CurrentPred_pid ! {request, self()},
                    CurrentPred;
                false ->
                    Succ_pid ! {notify, {Id, self()}},
                    Successor
            end
    end.

  add(Node_Key, Value, ClientQref, Client, Id, {Pred_key, _}, {_, Succ_pid}, Store) ->
    case key:between(Node_Key, Pred_key, Id) of
      true ->
        Client ! {ClientQref, ok}, % If responsible, send SMS to client
        storage:add(Node_Key, Value, Store);
      false ->
        Succ_pid ! {add, Node_Key, Value, ClientQref, Client},
        Store
    end.

  lookup(Node_Key, ClientQref, Client, Id, {Pred_key, _}, Successor, Store) ->
    case key:between(Node_Key, Pred_key, Id) of
      true ->
        Result = storage:lookup(Node_Key, Store),
        Client ! {ClientQref, Result};
      false ->
        {_, Succ_pid} = Successor,
        Succ_pid ! {lookup, Node_Key, ClientQref, Client}
      end.

% a store contains the range (P key, Id], that is from (not including P key to (including) Id.
  handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest}, % that will be used to delegate responsibility
    Keep.
