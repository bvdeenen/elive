-module(ball).

-import(crypto, [rand_uniform/2, start/0]).
-include("state.hrl").
-compile(export_all).

create_balls(_, _World, 0) ->
	[];

create_balls( Canvas, World, Nballs) ->
	[create_ball(Canvas, World) | create_balls(Canvas, World, Nballs-1)].

create_ball(Canvas, World) ->
	Pos = { rand_uniform(200,400), rand_uniform(200, 400) },
	create_ball(Canvas, World, Pos).


%% each ball consists of three processes:
%% BallProcess: the process that makes a new generation every xx ms.
%% ball_communicator: the process that communicates with others and the Canvas
%% the ask_neighbour_info process that periodically checks the neighbours
create_ball(Canvas, World, {X,Y}) ->
	State=#state{pos={X,Y}},
	Size = State#state.size, %% uses default value from recrd
	Color= State#state.color,
	Ball=gs:oval(Canvas,[{coords,[{X-Size, Y-Size}, {X+Size,Y+Size}]},{fill,Color}]),
	BallCommunicator=spawn_link( fun() -> ball_communicator(State) end),
	BallProcess=spawn_link( fun() -> ball(Ball, World, State#state{comm_pid=BallCommunicator}) end ),
	BallCommunicator ! { set_ball_process, BallProcess},
	BallCommunicator.


ball(Ball, World, OldState  ) ->
	{X,Y}=OldState#state.pos,
	Size = OldState#state.size + OldState#state.dsize,
	gs:config(Ball,[{coords,[{X-Size, Y-Size}, {X+Size, Y+Size}]}, 
		{fill, OldState#state.color}]),

	NewState = 
	receive
		die ->
			exit(normal);
		{grid_info, Grid } ->
			handle_grid_info(World, Grid, OldState);
		{neighbour_info, QuadrantInfo} when OldState#state.dsize > 0 ->
			%% io:format("~p Received neighbour_info= ~p ~n", [self(), QuadrantInfo]),
			Limit=20,
			%% QuadrantSum=lists:sum(QuadrantInfo),
			AllQuadrantsFull = lists:all( fun(QI) -> QI > Limit end, QuadrantInfo) ,
			if
				AllQuadrantsFull ->
					%% io:format("~p stopped growing~n", [self()]),
					OldState#state{dsize=0, color=green};
				true ->
					OldState#state{quadrants=QuadrantInfo}
			end;
		{neighbour_info, QuadrantInfo} when OldState#state.dsize =:= 0 ->
			%% io:format("~p Received neighbour_info= ~p ~n", [self(), QuadrantInfo]),
			Limit=20,
			%% QuadrantSum=lists:sum(QuadrantInfo),
			AllQuadrantsFull = lists:all( fun(QI) -> QI > Limit end, QuadrantInfo) ,
			if
				AllQuadrantsFull =:= false ->
					DefaultState=#state{},
					io:format("~p started growing again~n", [self()]),
					OldState#state{dsize=DefaultState#state.dsize, 
						color=DefaultState#state.color};
				true ->
					OldState#state{quadrants=QuadrantInfo}
			end
	after OldState#state.generation_interval ->
		OldState
	end,
	%% NewState is now set

	%% check if the size has reached the maximum value
	NewState1=
	if
		Size > 20 -> NewState#state{dsize=0, color=purple};
		true -> NewState
	end,

	NewState2=NewState1#state{
		size=Size,
		generation=OldState#state.generation+1}, 
	%% tell BallCommunicator our new state
	NewState2#state.comm_pid ! {update_state, NewState2},
	R2=rand_uniform(0,100) ,
	if 	
		NewState2#state.generation  < NewState2#state.generation_die ;
		R2 < 95 ->
			ball(Ball, World, NewState2);
		true->
			gs:destroy(Ball),
			World ! {old_age_death, NewState2#state.comm_pid },
			exit(normal)
	end.  

ball_communicator(OldState) ->
	NewState=
	receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			OldState#state.ball_process ! die,
			exit(normal);
		{set_ball_process, Pid} ->
			OldState#state{ball_process=Pid};
		{Pid, get_state} ->
			Pid ! {self(), info, OldState},
			OldState;
		{neighbour_info, QuadrantInfo} when OldState#state.ball_process =/= undefined->
			OldState#state.ball_process ! { neighbour_info, QuadrantInfo },
			OldState;
		{update_state, State} ->
			State#state{ball_process=OldState#state.ball_process};
		{_PidStatProcess, grid_info, Grid } ->
			OldState#state.ball_process ! {grid_info, Grid},
			OldState;
		Message ->
			io:format("ball_communicator ~p received unexpected ~p, ~nState=~p~n", 
				[self(), Message, OldState])
	end,
	ball_communicator(NewState).
	

handle_grid_info(World, Grid, State) ->
	{X,Y} = State#state.pos,
	I=gridindex(X,Y),
	V=array:get(I, Grid),
	
	R=rand_uniform(0,100) ,
	if 
		State#state.generation >  State#state.generation_split ,
		X > ?GRIDSIZE,
		Y > ?GRIDSIZE,
		X < ?WORLDSIZE - ?GRIDSIZE,
		Y < ?WORLDSIZE - ?GRIDSIZE,
		V < 100,
		R > 75  ->
			World ! { self(), split, State };
		true ->
			true
	end,
	State.		

	



clone_ball(Canvas, OldState) ->
	F=fun() -> rand_uniform(20, 50) end,
	I=rand_uniform(0,3),
	{X,Y}=OldState#state.pos,

	{DX,DY}=
			case I of 
				0 -> {-F(), -F() } ;
				1 -> {-F(),  F() } ;
				2 -> { F(), -F() } ;
				3 -> { F(),  F() } ;
				true -> { 0, 0}
			end	,

	World = self(),
	Pid=
	if 
		DX =/= no_clone ->
			P=create_ball(Canvas, World, {X+DX, Y+DY}),
			%% io:format("new ball at ~p,~p~n", [X+DX, Y+DY]),
			P;
		true ->
			false
	end,
	Pid.






%% vim:tw=0
