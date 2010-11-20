-module(elive).
-export([init/0]).

-import(crypto, [rand_uniform/2, start/0]).
-import(statistics_process).
-import(grazer).

-include("state.hrl").


init() ->
	crypto:start(),
	I= gs:start(),
	W= gs:window(I,[{title,"Ball"},{width,?WORLDSIZE},{height,?WORLDSIZE},{map,true}]),
	Canvas= gs:canvas(W,[{width,?WORLDSIZE},{height,?WORLDSIZE},{bg,white}]),
	gs:create(button, quit, W, [{label, {text,"Quit Elive"}},{x,5}, {y, 5}]),
	Pids = ball:create_balls(Canvas, self(), _Nballs=4),

	grazer:create_grazers(Canvas, self(), _NGrazers=8),

	World=self(),
	spawn_link(fun() -> statistics_process:start(World) end),
	loop(Canvas, Pids, false),
	gs:stop().

	
loop(Canvas, Pids, GridInfo) ->
	receive
		{Pid, give_pids} -> 
			Pid ! {self(), Pids},
			loop(Canvas, Pids, GridInfo);
		{grid_info, NewGrid} ->
			loop(Canvas, Pids, NewGrid);
		{Pid, grid_info} ->
			Pid ! {grid_info, GridInfo} ,
			loop(Canvas, Pids, GridInfo);
		{gs,_,destroy,_,_} ->
			quit_balls(Pids);
		{gs,quit,click,_,_} ->
			quit_balls(Pids);
		{ _Pid, split, OldState } ->
			%% io:format("~p asking for clone,~n~p~n", [_Pid, OldState]),
			Pid1=ball:clone_ball(Canvas, OldState) ,
			case Pid1 of
				false -> loop(Canvas, Pids, GridInfo)	;
				Pid1 ->  loop(Canvas, [Pid1 | Pids] , GridInfo)
			end	;
		{old_age_death, Pid} ->
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids), GridInfo);
		{'EXIT', Pid, normal } ->	
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids), GridInfo);
		Message ->
			io:format("Canvas got unexpected ~p~n", [Message]),
			loop(Canvas, Pids, GridInfo)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).


%% vim:tw=0
