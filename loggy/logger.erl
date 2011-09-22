-module(logger).
-export([start/1, stop/1]).
-export([ remove_and_log_less_than_n/3,inject_message_into_queue/4,find_lowest_common/2, map_message_entries/4, loop/2]).

start(Nodes) ->
	spawn(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init( Nodes ) ->
	loop(
		lists:map( (fun(A) -> {A, []} end), Nodes ),
		fun log/3 
		).
	
inject_message_into_queue( Queue, From, Time, Msg ) ->
	lists:map( (fun({Name, Lst}) ->
		if
			Name =:= From -> {From, [{Time,Msg}|Lst]};
			true -> {Name, Lst}
		end
		end),
		Queue ).

loop(Queue, Log) ->
	receive
		{log, From, Time, Msg} ->
			UpdatedQueue = inject_message_into_queue( Queue, From, Time, Msg ),
			N = find_lowest_common( Time, UpdatedQueue ),
			PrunedQueue = remove_and_log_less_than_n( N, UpdatedQueue, Log ),
			loop(PrunedQueue, Log);
		stop ->
			ok
	end.

find_lowest_common( CurrentMin, [{_, []} |_] ) ->
	0;
find_lowest_common( CurrentMin, [{From, List}|[]] ) ->
	{N,_} = lists:nth(1,List),
	erlang:min( CurrentMin, N);
find_lowest_common( CurrentMin, [{From, List}|T] ) ->
	{N,_} = lists:nth(1,List),
	Min = erlang:min( CurrentMin, N),
	find_lowest_common( Min, T ).
 
remove_and_log_less_than_n( N, Queue, Log ) ->
	PrunedQueue = lists:map( (fun({From,Lst}) -> 
		UpdatedLst = lists:filter( (fun(Entry) ->
			map_message_entries( Log,From,N,Entry) end),
			Lst),
		{From,UpdatedLst}
	end) , Queue ),
	PrunedQueue.

map_message_entries( Log, From, N, {TimeStamp, Msg} ) when TimeStamp < N ->
	Log( From, TimeStamp, Msg ),
	false;
map_message_entries( Log, From, N, {TimeStamp, Msg} ) ->
	true.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [From, Time, Msg]).
