-module(gms).
-export([start/1, start/2]).
-export([bcast/3, joining/4]).

-define(crashchance, 50). %% one-in-N

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id,Rnd, Self) end).

init(Id,Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	leader(Id, Master, []).

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id,Rnd, Grp, Self) end).
	
init(Id,Rnd, Grp, Master) ->
	Self = self(),
	random:seed(Rnd,Rnd,Rnd),
	Grp ! {join, Self},
	receive
		{view, State, Leader, Peers} ->
			erlang:monitor(process, Leader),
			Master ! {ok, State},
			slave(Id, Master, Leader, Peers)
		after 1000 ->
			Master ! {error, "no reply from leader"}
	end.
	
slave(Id, Master, Leader, Peers) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Peers);
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, Peers);
		{msg, Msg} ->
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, Peers);
		{view, _, _, UpdatedPeerList} ->
			slave(Id, Master, Leader, UpdatedPeerList);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id,Master,Peers);
		stop ->
			ok;
		Error ->
			io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
	end.
	
election(Id, Master, [Leader|Rest] ) ->
	io:format("--~w: electing new leader ~w~n",[Id, Leader]),
	if
		Leader == self() ->
			leader(Id,Master,Rest);
		true ->
			erlang:monitor(process, Leader),
			slave( Id, Master,Leader,Rest)
	end.
	
leader(Id, Master, Peers) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Peers),
			Master ! {deliver, Msg},
			leader(Id, Master, Peers);
		{join, Peer} ->
			Master ! request,
			joining(Id, Master, Peer, Peers);
		stop ->
			ok;
		Error ->
			io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
	end.
	
joining(Id, Master, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, State, self(), Peers2}, Peers2),
			leader(Id, Master, Peers2);
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