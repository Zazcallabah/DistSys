-module(node).
-export([stabilize/3, received_notify/3]).
-export([start/1,start/2]).

-define(stabilizeinterval, 1000).
-define(Timeout, 5000).

start(Id) ->
	start(Id,nil).
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id,Peer) end).

init(Id,Peer) ->
	Predecessor = nil,
	{ok,Successor} = connect(Id,Peer),
	schedule_stabilize(),
	node( Id, Predecessor, Successor ).

connect( Id, nil ) ->
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
		after ?Timeout ->
			io:format("Timed out.")
	end.
	
	
node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = received_notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			received_request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		probe ->
			create_probe(Successor,Id),
			node(Id,Predecessor,Successor);
		{probe, Id, Nodes, T} ->
			remove_probe( T, Nodes ),
			node(Id,Predecessor,Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref,Nodes,T,Id,Successor),
			node(Id,Predecessor,Successor);
		stabilize ->
			ask( Successor ),
			node(Id, Predecessor, Successor )
	end.
	
remove_probe( {Ms,S,Mys}, Nodes ) ->
	{CMs,CS,CMys} = erlang:now(),
	io:format("probe'd: ~w:~w:~w ms | ~w~n",[CMs-Ms,CS-S,CMys-Mys,Nodes]).
	
create_probe({Skey,Spid},Id)->
	Spid ! {probe, Id, [{Id,self()}], erlang:now()}.

forward_probe( Ref, Nodes, T, Id, {Skey, Spid})->
	Spid ! {probe, Ref, [{Id,self()}|Nodes],T}.


received_notify({Nkey, Npid},Id,Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between( Nkey, Pkey, Id) of
				true ->
					{Nkey, Npid};
				false ->
					Predecessor
			end
	end.
	
received_request( Peer, Predecessor ) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

ask( {Key, Pid} ) ->
	Pid ! {request, self()}.

send_notify( SuccessorPid, Id ) ->
	SuccessorPid ! {notify, {Id, self()}}.
	
	
stabilize( Pred, Id, Successor ) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			send_notify( Spid, Id ),
			Successor;
		{Id, _} ->
			Successor;
		{Skey, _} ->
			send_notify( Spid, Id ),
			Successor;
		{Xkey, Xpid} ->
			case key:between( Xkey, Id, Skey) of
				false ->
					send_notify( Spid, Id ),
					Successor;
				true ->
					ask( Pred ),
					Pred
			end
	end.
	
schedule_stabilize() ->
	timer:send_interval( ?stabilizeinterval, self(), stabilize ).