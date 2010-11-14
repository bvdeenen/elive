-module(elive).
-export([init/0]).

-import(crypto, [rand_uniform/2, start/0]).

-include("state.hrl").


init() ->
	crypto:start(),
	I= gs:start(),
	W= gs:window(I,[{title,"Ball"},{width,600},{height,600},{map,true}]),
	Canvas= gs:canvas(W,[{width,600},{height,600},{bg,yellow}]),
	gs:create(button, quit, W, [{label, {text,"Quit Elive"}},{x,5}, {y, 5}]),
	Pids = ball:create_balls(Canvas, self(), _Nballs=4),
	World=self(),
	spawn_link(fun() -> statistics_process(World) end),
	loop(Canvas, Pids),
	gs:stop().

statistics_process(World) ->
	receive
	after 5000 -> true
	end,
	World ! {self(), give_pids},
	receive
		{World, Pids} ->
			io:format("~p balls~n", [length(Pids)])
	end,
	statistics_process(World).
	
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
			Pid1=ball:clone_ball(Canvas, OldState) ,
			case Pid1 of
				false -> loop(Canvas, Pids)	;
				Pid1 ->  loop(Canvas, [Pid1 | Pids] )
			end	;
		{old_age_death, Pid} ->
			%% io:format("~p died of old age~n", [Pid]),
			loop(Canvas, lists:delete(Pid, Pids));
		Message ->
			io:format("Canvas got unexpected ~p~n", [Message]),
			loop(Canvas, Pids)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).


%% vim:tw=0
