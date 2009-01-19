
-module(world).
-compile(export_all).

start(N) ->
	Pid = spawn(fun() -> loop(startup) end),
	register( world, Pid),
	start_one(N) .
	

%% rpc for every beast
rpc(Pid, Request) ->
	%io:format("world:rpc(~p,~p)~n", [Pid, Request]),
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			{'World responds', Response}
	end.		

%% message from beasts
loop(startup) ->
	process_flag(trap_exit, true),
	loop([]);

loop(Beasts) ->
	%%io:format("Beasts is now ~p~n", [Beasts]),
	receive 
		{'EXIT', Pid, Why } ->
			io:format("beast ~p has died ~p~n", [Pid, Why]),
			loop(Beasts);

		{birth, Pid} ->
			io:format("new birth of ~p~n", [Pid]),
			link(Pid),
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
	io:format("Sending 'die' to ~p~n", [Pid]), 
	Pid ! die,
	kill_beasts(Rest).

start_one(0) -> void;

start_one(N) ->
	io:format("starting beest ~p ~n", [N]),
	Pid = spawn( one, start, []),
	io:format("beest ~p started with pid ~p", [N, Pid]),
	start_one(N-1) .

