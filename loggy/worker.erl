-module(worker).
-export([start/5, stop/1]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, 1);
		stop ->
			ok
	end.

loop(Name, Log, Peers, Sleep, Jitter, Localtime)->
	Wait = random:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			UpdatedTime = erlang:max( Localtime, Time ) + 1,
			Log ! {log, Name, UpdatedTime, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, UpdatedTime);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
		after Wait ->
			Selected = select(Peers),
			Time = Localtime + 1,
			Delay = random:uniform(Jitter),
			Message = {hello, Delay},
			Selected ! {msg, Time, Message},
			timer:sleep(Delay),
			Log ! {log, Name, Time, {sending, Message}},
			loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).
