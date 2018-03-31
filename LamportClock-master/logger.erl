-module(logger).
-export([start/1, stop/1]).


start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
Logger ! stop.

% init(_) ->
% loop().

% Initalize logger with a timestamp(Clock list) of all nodes.
init(Nodes) ->
    Clock = time:clock(Nodes),
    Queue = [],
    loop(Clock, Queue).

  % Sort the hold-back queue.
sort(Queue) ->
  F = fun({_, TimeA, _}, {_, TimeB, _}) ->
              if
                  TimeA > TimeB ->
                      false;
                  true ->
                      true
              end
          end,
      lists:sort(F, Queue).

% Log message if Time is small (=< Clock),
% Otherwise add the message to a queue
queue(Queue, Clock) ->
  F = fun({From, Time, Msg}, NewQueue) ->
            case time:safe(Time, Clock) of
                true ->
                    log(From, Time, Msg),
                    NewQueue;
                false ->
                    [{From, Time, Msg}|NewQueue]
            end
        end,
    lists:foldl(F, [], Queue).


% Receive messages. Return an updated clock. Sort Logs,
loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            SortedQueue = sort([{From, Time, Msg}|Queue]),
            ProcessedQueue = queue(SortedQueue, UpdatedClock),
            loop(UpdatedClock, ProcessedQueue);
        stop ->
            ok
    end.


log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [From, Time,Msg]).
