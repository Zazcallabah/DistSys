-module(fixture).
-export([manual/0]).
-include_lib("eunit/include/eunit.hrl").

manual()->
	Leader = worker:start( 0, gms, 500, 3000 ),
	S1 = worker:start(1,gms,200,Leader,3000),
	S2 = worker:start(2,gms,400,Leader,2000),
	S3 = worker:start(3,gms,240,Leader,4000),
	S4 = worker:start(4,gms,500,Leader,1000),
	S5 = worker:start(5,gms,100,Leader,3000),
	timer:sleep(5000),
	Leader ! stop,
	timer:sleep(5000)
	.

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
		