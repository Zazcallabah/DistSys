-module(logger).
-export([start/1, stop/1, removeandloglessthann/3]).

start(Nodes) ->
	spawn(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init( Nodes ) ->
	loop( lists:map( (fun(A) -> {A, []} end, Nodes ) ).

loop(Queue) ->
	receive
		{log, From, Time, Msg} ->
			case lists:keysearch( From, 1, Queue ) of
				{value, {}} ->
					
				false ->
					UpdatedQueue = [{From,[{Time,Msg}]}|Queue],
			end
			
			%% add {time, msg} into Queue.from.2
			N = findlowestcommon( From, Time, UpdatedQueue ),
			PrunedQueue = removeandloglessthann( N, UpdatedQueue,
				(fun({Source,TimeStamp,Message}) ->
					log(Source,TimeStamp,Message)
				end) ),
			loop(PrunedQueue);
		stop ->
			ok
	end.
	
findlowestcommon( From, PingTime, Queue ) ->
	2.
			%% find lowest common (use Time value given)

	%% queue = [ {from, [{n,msg},...]},...]
removeandloglessthann( N, Queue, Log ) ->
	PrunedQueue = lists:map( (fun(A) -> N end), Queue ),
	%% map over queue (  [ [ {from, n, msg}, ...] , [] ]
			%% save any n > N
			%% Log all n <= N
	Queue.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [From, Time, Msg]).
