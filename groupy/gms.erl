-module(gms).
-export([start/1, start/2]).
-export([bcast/3, joining/5]).

-define(crashchance, 150). %% one-in-N

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id,Rnd, Self) end).

init(Id,Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	erlang:register(leader,self()),
	leader(Id, Master,0, []).

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id,Rnd, Grp, Self) end).
	
init(Id,Rnd, Grp, Master) ->
	Self = self(),
	random:seed(Rnd,Rnd,Rnd),
	Grp ! {join, Self},
	receive
		{view, N, State, Leader, Peers} ->
			erlang:monitor(process, Leader),
			Master ! {ok, State},
			slave(Id, Master, Leader, N+1, {view, N, State,Leader,Peers}, Peers)
		after 2000 ->
			Master ! {error, "no reply from leader"}
	end.
	
slave(Id, Master, Leader, N, Last, Peers) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Peers);
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, N, Last, Peers);
		{msg, I, _} when I < N ->
			slave(Id, Master, Leader, N, Last, Peers);
		{msg, N, Msg} ->
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, N+1, {msg,N,Msg}, Peers);
		{view, N, A, B, UpdatedPeerList} ->
			slave(Id, Master, Leader,N+1,{view,N,A,B,UpdatedPeerList}, UpdatedPeerList);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id,Master,N,Last,Peers);
		stop ->
			ok;
		Error ->
			io:format("gms ~w: slave n:~w, strange message ~w~n", [Id,N, Error])
	end.
	
election(Id, Master,N,Last, [Leader|Rest] ) ->
	io:format("--~w: electing new leader ~w |~w|~w ~n",[Id, Leader,N,Last]),
	if
		Leader == self() ->
			erlang:register(leader,self()),
			bcast( Id, Last, Rest ),
			case Last of
				{view, M, _,_,_} ->
					leader(Id,Master,M+1,Rest);
				{msg,M,_} ->
					leader(Id,Master,M+1,Rest)
			end;
		true ->
			erlang:monitor(process, Leader),
			slave( Id, Master,Leader,N,Last,Rest)
	end.
	
leader(Id, Master,N, Peers) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg,N, Msg}, Peers),
			Master ! {deliver, Msg},
			leader(Id, Master,N+1, Peers);
		{join, Peer} ->
			Master ! request,
			joining(Id,N, Master, Peer, Peers);
		stop ->
			ok;
		Error ->
			io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
	end.
	
joining(Id,N, Master, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view,N, State, self(), Peers2}, Peers2),
			leader(Id, Master,N+1, Peers2);
		stop ->
			ok
	end.
	
bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).
	
crash(Id) ->
	case random:uniform(?crashchance) of
		?crashchance ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.