-module(routy).
-export([start/1, stop/1]).

start(Name) ->
	io:format( "registering ~w~n", [Name] ),
	register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
	Intf = intf:new(),
	Map = map:new(),
	Table = dijkstra:table(Intf, Map),
	Hist = hist:new(Name),
	io:format( "starting router ~w, [h:~w,t:~w,m:~w,i:~w]~n", [Name,Hist,Table,Map,Intf]),
	router(Name, 0, Hist, Intf, Table, Map).

prettyprint( Name, Msg, L ) ->
	prettyprint( [{str,Name,Msg}|L] ).
prettyprint( [] ) -> [];
prettyprint( [{str,N,V}|T] ) ->
	io:format( " ~w: ~s~n", [N,V] ),
	prettyprint( T );
prettyprint( [{N,V}|T] ) ->
	io:format( "    ~w: ~w~n", [N,V] ),
	prettyprint( T );
prettyprint( [H|T] ) ->
	io:format( "    ~w~n", [H] ),
	prettyprint( T ).

router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = intf:add(Node, Ref, Pid, Intf),
			prettyprint( Name, "added interface", [{node,Node},{pid,Pid},{ref,Ref}]),
			self() ! broadcast,
			router(Name, N, Hist, Intf1, Table, Map);
		{remove, Node} ->
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = intf:remove(Node, Intf),
			prettyprint( Name, "removed interface", [{node,Node},{ref,Ref}]),
			self() ! broadcast,
			router(Name, N, Hist, Intf1, Table, Map);
		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = intf:name(Ref, Intf),
			prettyprint( Name, "exit recieved, removing intf.", [{node,Down}]),
			Intf1 = intf:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		{status, {From,N2,Hist2,Intf2,Table2,Map2}} ->
			prettyprint( Name, "status response", [{from,From},{n,N2},{hist,Hist2},{intf,Intf2},
			{table,Table2},{map,Map2}]),
			router(Name, N, Hist, Intf, Table, Map);			
		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			prettyprint( Name, "status request", [{from,From},{n,N},{hist,Hist},{intf,Intf},
			{table,Table},{map,Map}]),
			router(Name, N, Hist, Intf, Table, Map);
		{route, Name, From, Message} ->
			prettyprint( Name, "received message", [{from,From},{str,message,Message}]),
			router(Name, N, Hist, Intf, Table, Map);
		{route, To, From, Message} ->
			prettyprint( Name, "routing message",
			[{from,From},{to,To},{Message},{table,Table}]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case intf:lookup(Gw, Intf) of
						{ok, Pid} -> Pid ! {route, To, From, Message},
						prettyprint( Name, "routed to gateway", [{gw, Gw},{str,m, Message}] );
						notfound ->
							prettyprint( Name, "interface not found for gw",
							[{gateway, Gw},{str,m,Message}]),
							ok
					end;
				notfound ->
					prettyprint( Name, "route not found for message",[{m,Message}]),
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);
		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			prettyprint(Name,"send message", [{to,To}]),
			router(Name, N, Hist, Intf, Table, Map);
		broadcast ->
			Message = {links, Name, N, intf:list(Intf)},
			prettyprint(Name,"broadcast interfaces",[{n,N}]),
			intf:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);
		update ->
			Table1 = dijkstra:table(intf:list(Intf), Map),
			prettyprint( Name, "update table", [{newtble,Table1}]),
			router(Name, N, Hist, Intf, Table1, Map);
		{links, Node, R, Links} ->
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					intf:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					prettyprint(Name,"incoming link update",[{indata,Hist1},{map,Map1}] ),
					self() ! update,
					router(Name, N, Hist1, Intf, Table, Map1);
				old -> router(Name, N, Hist, Intf, Table, Map)
			end;
		stop ->
			ok
	end.