-module(elive).
-export([init/0]).

-import(crypto, [rand_uniform/2, start/0]).
-import(statistics_process).

-include("state.hrl").


init() ->
	crypto:start(),
	I= gs:start(),
	W= gs:window(I,[{title,"Ball"},{width,?WORLDSIZE},{height,?WORLDSIZE},{map,true}]),
	Canvas= gs:canvas(W,[{width,?WORLDSIZE},{height,?WORLDSIZE},{bg,yellow}]),
	gs:create(button, quit, W, [{label, {text,"Quit Elive"}},{x,5}, {y, 5}]),
	Pids = ball:create_balls(Canvas, self(), _Nballs=4),
	World=self(),
	spawn_link(fun() -> statistics_process:start(World) end),
	loop(Canvas, Pids),
	gs:stop().

	
loop(Canvas, Pids) ->
	receive
		{Pid, give_pids} -> 
			Pid ! {self(), Pids},
			loop(Canvas, Pids);
		{gs,_,destroy,_,_} ->
			quit_balls(Pids);
		{gs,quit,click,_,_} ->
			quit_balls(Pids);
		{ _Pid, split, OldState } ->
			%% io:format("~p asking for clone,~n~p~n", [_Pid, OldState]),
			Pid1=ball:clone_ball(Canvas, OldState) ,
			case Pid1 of
				false -> loop(Canvas, Pids)	;
				Pid1 ->  loop(Canvas, [Pid1 | Pids] )
			end	;
		{old_age_death, Pid} ->
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids));
		{'EXIT', Pid, normal } ->	
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids));
		Message ->
			io:format("Canvas got unexpected ~p~n", [Message]),
			loop(Canvas, Pids)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).


%% vim:tw=0
