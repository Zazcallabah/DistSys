-module(test).
-export([start/1, stop/1, init/1]).

start(Name) ->
	register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
	router(Name).

router(Name) ->
	io:format("output ~w~n",[Name]),
	receive
		{msg, Msg} ->
			io:format(Msg,[]),
			router(Name);
		{test, Name} ->
			io:format(Name,[]),
			router(Name);
		stop ->
			ok
	end.