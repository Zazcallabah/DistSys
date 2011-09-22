-module(fixture).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").


trackincoming( Hits ) ->
	receive
		{hit, From, Time, {received, Msg} } ->
			case lists:keyfind( Msg, 3, Hits ) of
				false ->
					1 = 2;
				{F,T,M}->
					trackincoming( lists:delete({F,T,M},Hits) )
			end;
			
		{hit, From, Time, {sending, Msg} } ->
			trackincoming( [ {From, Time, Msg} | Hits] )
	end.


logstart( Nodes ) ->
	Assert = spawn(fun() -> trackincoming([]) end),
	logger:loop(
		lists:map( (fun(A) -> {A, []} end), Nodes ),
		(fun(From, Time, Msg) -> Assert ! {hit, From, Time, Msg} end)
		).

run() ->
	Log = spawn( (fun () -> logstart([john, paul, ringo, george] ) end) ),
	A = worker:start(john, Log, 13, 2000, 100),
	B = worker:start(paul, Log, 34,8000, 100),
	C = worker:start(ringo, Log, 45, 2000, 100),
	D = worker:start(george, Log, 19, 4000, 100),
	A ! {peers, [B, C, D]},
	B ! {peers, [A, C, D]},
	C ! {peers, [A, B, D]},
	D ! {peers, [A, B, C]},
	timer:sleep( 9000 ),
	Log ! stop,
	A ! stop,
	B ! stop,
	C ! stop,
	D ! stop.

injectmessageworkssimple_test() ->
	Q = [
	{one, []},
	{two, []}
	],
	
	Newlist = logger:inject_message_into_queue(Q,one,4,helo),
	[
	{one, [{4,helo}]},
	{two, []}
	] = Newlist.
	
injectmessageworksmulty_test() ->
	Q = [
	{one, [{2,no}]},
	{two, [{5,no}]}
	],
	
	Newlist = logger:inject_message_into_queue(Q,one,4,helo),
	[
	{one, [{4,helo},{2,no}]},
	{two, [{5,no}]}
	] = Newlist.

ensureloggerremovesnothingfromemptylist_test() ->
	[] = logger:remove_and_log_less_than_n(0,[],(fun(_,_,_)-> 1 = 2 end)).

ensureloggerremovesnothingfromnonemptylist_test() ->
	[{one,[{1,msg}]}] =
		logger:remove_and_log_less_than_n(0,[{one,[{ 1, msg}]}],(fun(_,_,_)-> 1 = 2 end)).

ensureloggerremovesonefromsimplelist_test() ->
	[{one,[{1,msg}]}] =
		logger:remove_and_log_less_than_n(1,[{one,[{1, msg},{0, msg}]}],(fun(_,_,_)-> [] end)).

canremovelastmsgforsource_test() ->
	[{one,[]}] =
		logger:remove_and_log_less_than_n(4,[{one,[{ 1, msg}]}],(fun(_,_,_)-> [] end)).

fno( From, N, Msg ) ->
	1=2.
fsame( From, N, Msg ) ->
	far = From,3=N,thar=Msg.
fsame2( From, N, Msg ) ->
	far2 = From,2=N,thar2=Msg.

mapmessagescorrectly_test() ->
	logger:map_message_entries( fun fno/3, home, 3, {44, helo} ),
	logger:map_message_entries( fun fsame/3, far, 3, {3,thar} ),
	logger:map_message_entries( fun fsame2/3, far2, 3, {2,thar2} ).

verifyFindlowestcommon_test() ->
	Q = [
	{one, [{12,no},{3,no},{1,no}]},
	{two, [{5,no},{2,no}]}
	],
	5 = logger:find_lowest_common(12,Q).