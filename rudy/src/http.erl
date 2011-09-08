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
	"GET " ++ URI ++ " HTTP/1.1\r\n"
"Host: www.kth.se\r\n" ++
"Connection: keep-alive\r\n" ++
"User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.220 Safari/535.1\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ++
"Accept-Encoding: gzip,deflate,sdch\r\n" ++
"Accept-Language: en,sv;q=0.8\r\n" ++
"Accept-Charset: UTF-8,*;q=0.5\r\n" ++
"Cookie: tmpPersistentuserId=a849cbf9f2f83c19086bab5816531086; _pk_ref.4.8810=%5B%22%22%2C%22%22%2C1315231642%2C%22http%3A%2F%2Fwww.google.se%2Furl%3Fsa%3Dt%26source%3Dweb%26cd%3D2%26ved%3D0CB4QFjAB%26url%3Dhttp%253A%252F%252Fwww.kth.se%252Fstudent%252Fkurser%252Fkurs%252FID2201%253Fl%253Den%26rct%3Dj%26q%3Dkth%2520distrubuerade%2520system%26ei%3DltdkTvj7J4rCtAbV0Pz2CQ%26usg%3DAFQjCNFGWjCg1W6wEVW_jE4TV3ZfLnlAfA%26sig2%3DGk5klyuqy59YmAEN3QOBjw%22%5D; _pk_id.4.8810=1f50c76f19de15a7.1300795928.5.1315231642.1315226301; _pk_ref.5.8810=%5B%22%22%2C%22%22%2C1315324861%2C%22https%3A%2F%2Flogin.kth.se%2Flogin%3Fservice%3Dhttps%253A%252F%252Fwww.kth.se%252Fsocial%252Faccounts%252Flogin%252F%253Fnext%253Dhttps%25253A%25252F%25252Fwww.kth.se%25252Fstudent%25252Fminasidor%25252Flogin.jsp%22%5D; _pk_id.5.8810=084661ea173a5298.1304930658.2.1315324893.1304930744; JSESSIONID=79F6BC6AAE8B861030F4570307D7C763; __utma=154244322.523922043.1292599135.1315324843.1315469067.32; __utmc=154244322; __utmz=154244322.1315469067.32.24.utmcsr=delicious.com|utmccn=(referral)|utmcmd=referral|utmcct=/search\r\n" ++
 "\r\n" ++
"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.".

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