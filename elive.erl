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
	process_flag(trap_exit, true),
	register(world, self()),
	Canvas= gs:canvas(W,[{width,?WORLDSIZE},{height,?WORLDSIZE},{bg,white}]),
	gs:create(button, quit, W, [{label, {text,"Quit Elive"}},{x,5}, {y, 5}]),
	Pids = ball:create_balls(Canvas, _Nballs=40),
	grazer:create_grazers(Canvas, _NGrazers=5),
	Sp=spawn_link(fun() -> statistics_process:start() end),
	loop(Canvas, Pids, false),
	Sp ! quit,
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
		{ _Pid, split_grazer, OldState } ->
			%% io:format("~p asking for clone of Grazer ,~n~p~n", [_Pid, OldState]),
			grazer:clone_grazer(Canvas, OldState) ,
			loop(Canvas, Pids, GridInfo);
		{'EXIT', Pid, ball_old_age_death} ->
			loop(Canvas, lists:delete(Pid, Pids), GridInfo);
		{'EXIT', Pid, {eaten, State}} ->
			{X,Y}=State#state.pos,
			GridIndex=?gridindex(X,Y),
			%% io:format("~p died because it was eaten~n", [Pid]),
			GN=
			try
				{V, CellPids}=array:get(GridIndex, GridInfo),
				%% TODO must also lower V appropriate value
				array:set(GridIndex, {V, lists:delete(Pid, CellPids)}, GridInfo)
			catch
				_:_ -> GridInfo
			end,	
			loop(Canvas, lists:delete(Pid, Pids), GN);
		{'EXIT', _Pid, starved} ->
			%% io:format("grazer ~p starved~n", [Pid]),
			loop(Canvas, Pids, GridInfo);
		{'EXIT', Pid, normal } ->	
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids), GridInfo);
		Message ->
			io:format("Canvas got unexpected ~p~n", [Message]),
			loop(Canvas, Pids, GridInfo)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die_normal end, Pids).


%% vim:tw=0
