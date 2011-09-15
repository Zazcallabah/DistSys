-module(setup).
-export([menu/0, start/0]).

start() ->
	routy:start(toronto),
	routy:start(montreal),
	routy:start(edmonton),
	routy:start(calgary),
	routy:start(winnipeg),
	routy:start(hamilton),
	
	toronto ! {add, calgary, {calgary, 'canada@130.229.181.78'}},
		calgary ! {add, toronto, {toronto, 'canada@130.229.181.78'}},
	toronto ! {add, montreal, {montreal, 'canada@130.229.181.78'}},
		montreal ! {add, toronto, {toronto, 'canada@130.229.181.78'}},
	toronto ! {add, edmonton, {edmonton, 'canada@130.229.181.78'}},
	hamilton ! {add, calgary, {toronto, 'canada@130.229.181.78'}},
	winnipeg ! {add, calgary, {calgary, 'canada@130.229.181.78'}},
	edmonton ! {add, hamilton, {hamilton, 'canada@130.229.181.78'}},
	edmonton ! {add, winnipeg, {winnipeg, 'canada@130.229.181.78'}}.
	
menu() ->
	case io:fread("cmd target p1 p2: ", "~a ~a ~a ~a") of
		{ok, [add, Target, P1, P2]}->
			Target ! {add, P1, {P1, P2}};
		{ok, [i, Target, _, _]} ->
			Target ! {status, self()};
		{ok, [u, Target, _, _]} ->
			Target ! update;
		{ok, [b, Target, _, _]} ->
			Target ! broadcast;
		{ok, [send, Target, To, Msg]} ->
			Target ! {send, To, Msg };
		{ok, [exit, _,_,_]} ->
			exit(0);
		{ok, _} ->
			io:format("bad origin~n");
		{error, What} ->
			io:format("error ~w ~n", [What])
	end,
	menu().



	

