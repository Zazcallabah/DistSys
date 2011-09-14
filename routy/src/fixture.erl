-module(fixture).
-include_lib("eunit/include/eunit.hrl").

ensurenewreturnsemptylist_test() ->
	[] = map:new().

ensurerandomerlangstuff_test() ->
	1 = length([test]).

callingupdatewithemptymapaddsnewentry_test() ->
	List = map:update(home,[work,school],[]),
	1 = length(List),
	[H|[]] = List,
	{home,[work,school]} = H.

callingupdateexistingmapaddsnewentry_test() ->
	List = map:update(home,[work,school],[{work,[school]}]),
	2 = length(List),
	A = lists:keyfind(home,1,List),
	{home, [work,school]} = A.

callingupdateexistingmapreplacesroutedata_test() ->
	List = map:update(home,[work],[{home, [school]},{work, [home]}]),
	A = lists:keyfind(home,1,List),
	{home,[work]} = A.

unreachable_nodes_are_not_reachable_test() ->
	List = [{home, [school,work,lol]}, {lol,[school]}, {school,[work,lol]}],
	Res = map:reachable(lol2, List),
	0 = length(Res).

reachable_nodes_are_reachable_test() ->
	List = [{home, [school,work,lol]}, {lol,[school]}, {school,[work,lol]}],
	Res = map:reachable(lol, List),
	1 = length( Res ),
	[school|_] = Res.

djik_insert_function_test() ->
	D = dijkstra:insert({one, 4, lol}, [{two, 2, lol},{three, 3, lol},{extra, 5, lol}]),
	{one, 4, lol} = lists:nth(3, D),
	E = dijkstra:insert({strange, 1, lol}, D),
	{strange, 1, lol} = lists:nth(1, E).
						
djik_replace_test() ->
	D = dijkstra:replace(two, 1, ggg, [{one,2,g},{two,3,g},{three,54,g},{four,100,g}]),
	{two,1,ggg} = lists:nth(1, D),
	4 = length( D ).
	
djik_entry_test() ->
	54 = dijkstra:entry(three, [{one,2,g},{two,3,g},{three,54,g},{four,100,g}]).


djik_route_find_test() ->
	{ok,4} = dijkstra:route(one, [{two,2},{one,4},{three,2},{six,2},{five,6}]).


djik_make_table_test() ->
	A = [
					{sweden, [norway, denmark, france]},
					{norway, [denmark, egypt, britain]},
					{denmark, [norway, sweden, germany]},
					{france, [germany, denmark, spain]},
					{egypt, [france, denmark, spain]},
					{britain, [denmark, france]},
					{germany, [sweden, egypt, britain]},
					{spain, [norway, sweden, france,germany]}
		],
	Lol = dijkstra:table([sweden,spain], A),
	{sweden,sweden}=lists:keyfind(sweden, 1, Lol),
	{germany,spain}=lists:keyfind(germany,1,Lol),
	Lol2 = dijkstra:table([france,britain,norway],A),
	{sweden,france}=lists:keyfind(sweden,1,Lol2),
	{egypt,norway}=lists:keyfind(egypt,1,Lol2).


map_tests_from_pm_test() ->
[{berlin,[london,paris]}] = map:update(berlin, [london, paris], []),
[london,paris] = map:reachable(berlin, [{berlin,[london,paris]}]),
[] = map:reachable(london, [{berlin,[london,paris]}]),
[paris,london,berlin]=map:all_nodes([{berlin,[london,paris]}]),
[{berlin, [madrid]}]=map:update(berlin, [madrid], [{berlin,[london,paris]}]).

djik_update_test() ->
	T1 = dijkstra:update(three,20,gg,[{one,2,g},{two,3,g},{three,54,g},{four,100,g}]),
	{three,20,gg} = lists:nth(3,T1),
	T2 = dijkstra:update(two,44,ggg,T1),
	{two,3,g} = lists:nth(2, T2).

djik_tests_from_pm_test()->
[]=	dijkstra:update(london, 2, amsterdam, []).

djik_tests_from_pm2_test()->
[{london,2,paris}]=dijkstra:update(london, 2, amsterdam, [{london, 2, paris}]).

djik_tests_from_pm3_test()->
[{london,1,stockholm}, {berlin, 2, paris}]= dijkstra:update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]).

dijk_tests_from_pm4_test() ->	
[{berlin,paris},{paris, paris}] =
	dijkstra:iterate(
	  [
	   {paris, 0, paris},
	   {berlin, inf, unknown}
	  ],
	  [
	   {paris, [berlin]}
	  ],
	  []).

dijk_tests_table_pm_test() ->

[{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}] =   dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).
