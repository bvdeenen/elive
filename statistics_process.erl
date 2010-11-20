-module(statistics_process).

-export([start/1]).

-include("state.hrl").

start(World) ->
	receive
	after 1000 -> true
	end,
	World ! {self(), give_pids},
	receive
		{World, Pids} ->
			io:format("~p balls~n", [length(Pids)])
	end,
	%% ask all balls for state
	lists:map(fun(Pid) -> Pid ! {self(), get_state} end, Pids),
	State = collect_states( Pids),
	S=?WORLDSIZE * ?WORLDSIZE div (?GRIDSIZE * ?GRIDSIZE),
	Grid=grid(State, array:new([ {size, S}, {default,0}])),
	%% notify all comm processes of Grid
	lists:map(fun(St) -> notify(St, Grid) end, State),
	World ! {grid_info, Grid},
	start(World).

notify({Pid, _Pos,  _Size}, Grid) ->
	Pid ! {self(), grid_info, Grid}.
	
grid([], Grid) -> Grid;

grid([{_Pid, {X,Y}, Size}| Tail], Grid) ->
	I=gridindex(X,Y),
	%% io:format("I=~p~n", [I]),
	O=
	try 
		array:get(I, Grid)
	catch
		_:Error -> io:format("array:get error ~p, with I=~p (X,Y)=(~p~p) and array size=~p~n", 
			[Error,I, X,Y, array:size(Grid)]),
		0 %% zero
	end	,
	grid(Tail, array:set(I, O+Size, Grid)).
	
	
collect_states([]) -> [] ;

collect_states([Pid|T]) ->
	receive
		{Pid, info, State} ->
			[{Pid, State#state.pos, State#state.size} | collect_states(T)]
	after 20 ->
		%% io:format("~p died in between ~n", [Pid]),
		collect_states(T)
	end.

	
