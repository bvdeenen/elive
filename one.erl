%% --- one beast
%%---
-module(one).

-export([start/0]).

start() -> 
	 spawn(fun() -> counter(startup) end).

counter(4) -> 
	void; 

counter(startup) ->
	world ! {birth, self() },
	counter(0) ;

counter(N) -> 
	% talk to the world
	io:format("one:counter ~p ~p~n", [self(), N]),
    receive    
		die ->
			io:format("received die message~n");

		Message ->
			io:format("stopping for Message ~p~n", [Message] ),
			void
		after 1500 ->
			counter(N+1)
    end.

