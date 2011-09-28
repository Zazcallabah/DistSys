-module(gui).
-export([start/2]).

start( Id, _ ) ->
	spawn(fun() -> loop(Id) end).



loop( Id ) ->
	receive
		{color, Color} ->
			io:format("~w is ~w~n",[Id,Color]),
			loop(Id);
		stop -> ok
	end.
