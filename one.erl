%% ---
%% loops and prints
%%---
-module(one).

-export([start/0]).

start() -> 
	     spawn(fun() -> counter(0) end).

cancel(Pid) -> Pid ! cancel.

counter(N) -> 
	io:format("hi ~p ~n", [N]),
    receive    
		cancel->
			void
		after 500 ->
			counter(N+1)
    end.

