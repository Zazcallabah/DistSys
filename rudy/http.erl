-module(http).
-export([parse_request/1]).
-export([ok/1]).
-export([get/1]).
-export([init/1]).
-export([start/1, stop/0]).

parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) ->
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13,10|R3] = R2,
	{{get, URI, Ver}, R3}.

request_uri([32|R0])->
	{[], R0};
request_uri([C|R0])->
	{Rest, R1} = request_uri(R0),
	{[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
	{v10, R0}.

headers([13,10|R0]) ->
	{[],R0};
headers(R0)->
	{Header,R1} = header(R0),
	{Rest,R2} = headers(R1),
	{[Header|Rest], R2}.

header([13,10|R0])->
	{[],R0};
header([C|R0]) ->
	{Rest, R1} = header(R0),
	{[C|Rest], R1}.

message_body(R) ->
	{R, []}.

ok(Body) ->
	"HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI)->
	"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

init(Port)->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen),
			gen_tcp:close(Listen),
			ok;
		{error, Error} ->
			error
	end.

handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			request(Client),
			handler(Listen);
		{error, Error} ->
			error
	end.

request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			Request = parse_request(Str),
			Response = reply(Request),
			gen_tcp:send(Client, Response);
		{error, Error} ->
			io:format("rudy:error: ~w~n",[Error])
	end,
	gen_tcp:close(Client).

reply({{get, URI, Version}, Headers, Body}) ->
	timer:sleep(40),
	http:ok(Body).

start(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
	exit(whereis(rudy), "time to die").