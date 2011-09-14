-module(hist).
-export([new/1, update/3]).

new(Name) ->
    [{Name, 0}].

%% you can update the set of messages and the 
%% returned value is either a new set or the 
%% atom old.
    
update(Name, X, Msg)->
    case lists:keysearch(Name, 1, Msg) of
	{value, {Name, Y}} ->
	    if 
		X > Y -> 
		    {new, [{Name, X}|lists:keydelete(Name, 1, Msg)]};
		true -> 
		    old
	    end;
	false ->
	    {new, [{Name, X}|lists:keydelete(Name, 1, Msg)]}
    end.
