-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() ->
    init(Name, Logger, Seed, Sleep, Jitter) end).

  stop(Worker) ->
    Worker ! stop.

  init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
      {peers, Peers} ->
        Clock = time:zero(),
        loop(Name, Log, Peers, Sleep, Jitter, Clock);
      stop ->
        ok
    end.

  % Send messages only to peers that have been created
  peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

%Use the max counter plus increment
loop(Name, Log, Peers, Sleep, Jitter, LocalTimeCounter)->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      MaxTime = time:merge(Time,LocalTimeCounter),
      IncTime = time:inc(MaxTime),
      Log ! {log, Name, IncTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter,IncTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}

  after Wait ->
    Selected = select(Peers),
    InitEventTime = time:inc(LocalTimeCounter),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, InitEventTime, Message},
    jitter(Jitter),
    Log ! {log, Name, InitEventTime, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter,InitEventTime)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) ->
  ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
