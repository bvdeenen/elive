-module(statistics_process).

-export([start/0]).

-include("state.hrl").

start() ->
	receive
		quit ->
			exit(normal)
	after 1000 -> true
	end,
	world ! {self(), give_pids},
	receive
		{_, Pids} ->
			io:format("~p balls~n", [length(Pids)])
	end,
	%% ask all balls for state
	lists:map(fun(Pid) -> Pid ! {self(), get_state} end, Pids),
	State = collect_states( Pids),
	S=?WORLDSIZE * ?WORLDSIZE div (?GRIDSIZE * ?GRIDSIZE),
	Grid=grid(State, array:new([ {size, S}, {default,{0, []}}])),
	%% notify all comm processes of Grid
	lists:map(fun(St) -> notify(St, Grid) end, State),
	world ! {grid_info, Grid},
	start().

notify({Pid, _Pos,  _Size}, Grid) ->
	Pid ! {self(), grid_info, Grid}.
	
grid([], Grid) -> Grid;

grid([{Pid, {X,Y}, Size}| Tail], Grid) ->
	I=?gridindex(X,Y),
	%% io:format("I=~p~n", [I]),
	{O, Pids} = array:get(I, Grid),
	grid(Tail, array:set(I, {O+Size, [Pid|Pids]}, Grid)).
	
	
collect_states([]) -> [] ;

collect_states([Pid|T]) ->
	receive
		{Pid, info, State} ->
			[{Pid, State#state.pos, State#state.size} | collect_states(T)]
	after 20 ->
		%% io:format("~p died in between ~n", [Pid]),
		collect_states(T)
	end.

	
