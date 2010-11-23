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
create_ball(Canvas, World, {X,Y}) ->
	State=#state{pos={X,Y}},
	Size = State#state.size, %% uses default value from recrd
	Color= State#state.color,
	Ball=gs:oval(Canvas,[{coords,[{X-Size, Y-Size}, {X+Size,Y+Size}]},{fill,Color}]),
	BallCommunicator=spawn_link( fun() -> ball_communicator(State) end),
	BallProcess=spawn_link( fun() -> ball(Ball, World, 
		State#state{comm_pid=BallCommunicator}, false) end ),
	BallCommunicator ! { set_ball_process, BallProcess},
	BallCommunicator.


ball(Ball, World, State, OldState  ) ->
	
	{X,Y}=State#state.pos,
	Size = State#state.size + State#state.dsize,
	if 
		OldState =/= false,
		OldState#state.pos =/= State#state.pos;
		OldState#state.size =/= State#state.size;
		OldState#state.color =/= State#state.color ->
			gs:config(Ball,[{coords,[{X-Size, Y-Size}, {X+Size, Y+Size}]}, 
				{fill, State#state.color}]);
		true ->
			true
	end,		

	NewState = 
	receive
		die ->
			die(Ball, World, State);
		{grid_info, Grid } ->
			handle_grid_info(World, Grid, State)
	after State#state.generation_interval ->
		State
	end,
	%% NewState is now set

	%% check if the size has reached the maximum value
	NewState1=
	if
		Size > 20 -> NewState#state{dsize=0, color=purple};
		true -> NewState
	end,

	%% increment generation counter
	NewState2=NewState1#state{
		size=Size,
		generation=State#state.generation+1}, 
	%% tell BallCommunicator our new state
	NewState2#state.comm_pid ! {update_state, NewState2},

	%% death ?
	R2=rand_uniform(0,100) ,
	if 	
		%% keep living if not too old
		%% otherwise 5% chance of dying
		NewState2#state.generation  < NewState2#state.generation_die ;
		R2 < 95 ->
			ball(Ball, World, NewState2, State);
		true-> %% ball dies
			die(Ball, World, NewState2)
	end.  


die(Ball, World, State) ->
	gs:destroy(Ball),
	World ! {old_age_death, State#state.comm_pid },
	State#state.comm_pid ! die,
	exit(normal).
	
handle_grid_info(World, Grid, State) ->
	{X,Y} = State#state.pos,
	I=?gridindex(X,Y),
	{V, _Pids} =array:get(I, Grid),
	
	R=rand_uniform(0,100) ,
	if 
		V > 100 ->
			State#state{dsize=0, color=green};
			
		State#state.generation >  State#state.generation_split ,
		X > ?GRIDSIZE,
		Y > ?GRIDSIZE,
		X < ?WORLDSIZE - ?GRIDSIZE,
		Y < ?WORLDSIZE - ?GRIDSIZE,
		V < 100,
		R > 75  -> %% 25% chance of cloning
			World ! { self(), split, State },
			State;
		true ->
			State
	end.


clone_ball(Canvas, OldState) ->
	F=fun() -> rand_uniform(20, ?GRIDSIZE) end,
	I=rand_uniform(0,4),

	{DX,DY}=
	case I of 
		0 -> {-F(), -F() } ;
		1 -> {-F(),  F() } ;
		2 -> { F(), -F() } ;
		3 -> { F(),  F() } ;
		true -> { 0, 0}
	end	,
	{X,Y}=OldState#state.pos,
	create_ball(Canvas, self(), {X+DX, Y+DY}).


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
	

%% vim:tw=0
