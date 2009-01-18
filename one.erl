%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(one).
-export([start/0]).

start() -> 
	     spawn(fun() -> counter(0) end).

cancel(Pid) -> Pid ! cancel.

counter(N) -> 
	io:format("hi ~g ~n", [N]),
    receive    
		cancel->
			void
		after 500 ->
			counter(N+1)
    end.

