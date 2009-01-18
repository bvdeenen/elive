
-module(world).
-compile(export_all).

start(N) ->
	Pid = spawn(fun() -> loop([]) end),
	register( world, Pid),
	start_one(N),
	Pid.

rpc(Pid, Request) ->
	%io:format("world:rpc(~p,~p)~n", [Pid, Request]),
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			{'World responds', Response}
	end.		

loop(Beasts) ->
	receive 
		{birth, Pid} ->
			io:format("new birth of ~p~n", [Pid]),
			loop([Pid|Beasts]);
		
		die ->
			io:format("world is dead~n", []),
			kill_beasts(Beasts);
		Any ->
			io:format("world received ~p~n", [Any]),
			loop(Beasts)
	end.

kill_beasts([]) ->
	io:format("all beasts are dead~n");

kill_beasts(Beasts) ->
	[Pid|Rest] = Beasts,
	Pid ! die,
	kill_beasts(Rest).

start_one(0) -> void;

start_one(N) ->
	io:format("starting beest ~p ~n", [N]),
	Pid = spawn( one, start, []),
	io:format("beest ~p started with pid ~p", [N, Pid]),
	start_one(N-1) .

