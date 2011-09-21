-module(fixture).
-include_lib("eunit/include/eunit.hrl").

ensureloggerremovesnothingfromemptylist_test() ->
	[] = logger:removeandloglessthann(0,[],(fun(_)-> 1 = 2 end)).

ensureloggerremovesnothingfromnonemptylist_test() ->
	[[{one,1,msg}]] =
		logger:removeandloglessthann(0,[[{one, 1, msg}]],(fun(_)-> 1 = 2 end)).

ensureloggerremovesonefromsimplelist_test() ->
	[[{one,1,msg}]] =
		logger:removeandloglessthann(0,[[{one, 1, msg},{one, 0, msg}]],(fun(_)-> [] end)).
