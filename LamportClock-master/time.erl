-module(time).
-export([zero/0,inc/1,merge/2,leq/2, clock/1,update/3, safe/2]).

zero() ->
0.
% Do an increment
inc(T) ->
  T + 1.
% Find the maximum value
merge(Ti, Tj) ->
max(Ti, Tj).

% true if Ti is less than or equal to Tj
leq(Ti,Tj) ->
Ti =< Tj.

%Return a clock that can keep track of the nodes.
% A turple containing a node and an initial Lamport
% value  4rm a lst of Nodes
clock(Nodes) ->
    F = fun(Node) ->
            {Node, zero()}
        end,
    lists:map(F, Nodes).

% return a clock that has been updated
% given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
    Result = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    lists:keysort(2, Result).

% Is it safe to log an event that happened at a given time, true or false.
safe(Time, Clock) ->
        [{_,MinTime}|_] = Clock,
        leq(Time, MinTime).
