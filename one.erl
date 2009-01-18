%% ---
%% loops and prints
%%---
-module(one).

-export([start/0]).

start() -> 
	 OnePid= spawn(fun() -> counter(startup) end),
	 spawn(fun() -> loop(OnePid) end).

% message handler
loop(OnePid) ->
	receive
		die -> 
			io:format("one ~p is killed~n", [self()] ),
			OnePid ! die;
		Any ->
			io:format("one ~p receive ~p~n", [self(), Any]),
			loop(OnePid)
	end.

cancel(Pid) -> Pid ! cancel.

counter(10) -> 
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

