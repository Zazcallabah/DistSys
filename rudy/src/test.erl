-module(test).
-export([bench/2]).

bench(Host, Port) ->
	Start = now(),
	run(1000, Host, Port),
	Finish = now(),
	timer:now_diff(Finish, Start).

run( N, Host, Port )->
	if
		N == 0 ->
			ok;
		true ->
			request( Host, Port),
			run(N-1, Host, Port)
	end.

request(Host,Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	Recv = gen_tcp:connect(Host, Port, Opt),
	case Recv of
		{ok, Server} ->
			comms( Server );
		{error, Code} ->
			request(Host,Port)
	end.

comms( Server ) ->
	gen_tcp:send(Server, http:get("http://www.kth.se/student/program-kurser/kurshemsidor/kurser-ict/ID2201/HT11-1/2.34722/rudy-a-small-web-server-1.184822?l=en_UK")),
	Recv = gen_tcp:recv(Server, 0),
	case Recv of
		{ok, _} ->
			ok;
		{error, Error} ->
			io:format("test: error: ~w~n", [Error])
	end.
