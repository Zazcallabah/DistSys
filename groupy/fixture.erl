-module(fixture).
-export([manual/0]).
-include_lib("eunit/include/eunit.hrl").

start(Link,N) ->
	worker:start(N,gms,600, Link, 2000 ).

manual()->
	Leader = worker:start( 0, gms, 500, 3000 ),
	ll( 1, 250, 60, [Leader]).
	
ll( N, Wait, End, Bag )->
	timer:sleep(  random:uniform(100) ),
	receive
		A -> ok
	after Wait ->
		if
			N =:= End ->
				ok;
			N rem 8 =:= 1 ->
				Target = oneof(Bag),
				NewBag = [start(Target,N)|Bag],
				ll( N+1, Wait, End, NewBag );
			N rem 10 =:= 5 ->
				NewBag = killoneof( Bag ),
				ll( N+1, Wait, End, NewBag );
			true ->
				ll( N+1, Wait, End, Bag)
		end
	end.
	
oneof( Bag ) ->
	N = random:uniform( length(Bag) ),
	lists:nth(N, Bag).
killoneof( Bag ) ->
	N = random:uniform( length(Bag) ),
	remove(N,Bag).

remove(_, []) -> []; 
remove(1, [_|T]) -> T; 
remove(N, [H|T]) -> [H | remove(N-1, T)]. 
	
recipient( Count ) ->
	receive
		{msg} ->
			Count ! {one},
			recipient( Count )
	end.

count(N) ->
	receive
		{one} ->
			count( N + 1 );
		{q, Sender} ->
			Sender ! N,
			count( N )
	end.

broadcast_sends_message_to_all_recipients_test() ->
	Counter = spawn(fun() -> count(0) end),
	R1 = spawn( fun() -> recipient( Counter ) end),
	R2 = spawn( fun() -> recipient( Counter ) end),
	R3 = spawn( fun() -> recipient( Counter ) end),
	R4 = spawn( fun() -> recipient( Counter ) end),
	gms:bcast( 0, {msg}, [R1,R2,R3,R4] ),
	timer:sleep( 100 ),
	Counter ! {q, self()},
	receive
		Assert ->
			4 = Assert
	end.

leader_in_joining_state_sends_view_test() ->
	MyPid = self(),
	Leader = spawn(fun() -> gms:joining(0,0,MyPid,[] ) end),
	Leader ! {ok, state},
	receive
		{view, state, Leader, [Pid]} ->
		Pid = self(),
		Leader ! stop;
		_ -> 0 = 1
		after 100 -> 0 = 1
	end.
		