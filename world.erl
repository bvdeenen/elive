
-module(world).
-compile(export_all).

start(N) ->
	Pid = spawn(fun world:loop/0),
	register( world, Pid),
	start_one(N),
	Pid.

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.		

loop() ->
	receive 
		Any ->
			io:format("world received ~p~n", [Any]),
			loop()
	end.

start_one(0) -> void;

start_one(N) ->
	io:format("starting beest ~p ~n", [N]),
	spawn( one, start, []),
	start_one(N-1) .

