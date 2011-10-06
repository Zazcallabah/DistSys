-module(key).
-export([generate/0, between/3]).


generate() ->
	random:uniform(1000000000).
	
between( _, From, To ) when From =:= To ->
	true;
between( Key, From, To ) when From < To ->
	if
		Key > From, Key =< To ->
			true;
		true ->
			false
	end;
between( Key, From, To ) ->
	if
		Key =< From, Key > To ->
			false;
		true ->
			true
	end.
