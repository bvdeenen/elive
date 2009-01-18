
-module(world).
-export([start/1]).

start(0) -> void;

start(N) ->
	io:format("starting beest ~p ~n", [N]),
	spawn( fun() -> one:start() end),
	start(N-1) .

