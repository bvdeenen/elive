-module(tr).
-compile(export_all).

init() ->
	Pid = spawn_link( fun () -> h() end),
	receive
		after 600 ->
			Pid ! message
	end,
	receive
		after 400 ->
			Pid ! message
	end,
	io:format("init finished~n"),
	B=1/0.

h() ->
	A =
	receive
		M -> M
	after 500 ->
		timeout
	end,
	io:format("A=~p~n", [A]),
	h().

