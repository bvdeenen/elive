-module(grazer).

-import(crypto, [rand_uniform/2, start/0]).
-include("state.hrl").
-compile(export_all).

create_grazers(_, _World, 0) ->
	[];

create_grazers( Canvas, World, Ngrazers) ->
	[create_grazer(Canvas, World) | create_grazers(Canvas, World, Ngrazers-1)].

create_grazer(Canvas, World) ->
	F=fun()->rand_uniform(?GRIDSIZE, ?WORLDSIZE-?GRIDSIZE) end,
	Pos = { F(), F() },
	create_grazer(Canvas, World, Pos).


coords(State) ->
	W = State#gstate.wsize,
	L = State#gstate.lsize,
	%% zero rotation shape
	Shape0=[{L,-W}, {L+W/2,0}, {L,W}, {-L,W}, {-L,-W}, {L,-W}],
	Cos=math:cos(State#gstate.direction),
	Sin=math:sin(State#gstate.direction),
	%% function for rotation of shape over State#gstate.direction radians.
	PolarRotation = fun({X,Y}) -> {X*Cos-Y*Sin, -X*Sin -Y*Cos} end,
	
	{X,Y}=State#gstate.pos,
		
	Coords=lists:map(fun({X1,Y1}) -> 
		{X1+X, Y1+Y} end, lists:map(PolarRotation, Shape0)),
	Coords.
	
	
%% each grazer consists of three processes:
%% GrazerProcess: the process that makes a new generation every xx ms.
%% grazer_communicator: the process that communicates with others and the Canvas
create_grazer(Canvas, World, {X,Y}) ->
	D=math:pi()/360.0*rand_uniform(0,360),
	State=#gstate{pos={X,Y}, direction=D},
	Color= State#gstate.color,
	Grazer=gs:polygon(Canvas,[ {coords,coords(State)},{fill,Color}]),
	GrazerCommunicator=spawn_link( fun() -> grazer_communicator(State) end),
	GrazerProcess=spawn_link( fun() -> grazer(Grazer, World, 
		State#gstate{comm_pid=GrazerCommunicator}, false) end ),
	GrazerCommunicator ! { set_grazer_process, GrazerProcess},
	GrazerCommunicator.

grazer(Grazer, World, State, _OldState  ) ->
	
	gs:config(Grazer, [{coords, coords(State)}, {fill, State#gstate.color}, raise]),

	NewState3 = 
	receive
		die ->
			exit(normal);
		{grid_info, Grid } when Grid =/= false ->
			handle_grid_info(World, Grid, State)
	after State#gstate.generation_interval ->
		State
	end,
	NewState = eat_one(World, NewState3),
	if 
		State#gstate.generation rem 5 =:= 0 ->
			World ! {self(), grid_info};
		true ->
			true
	end,		
	%% NewState is now set
	Cos=math:cos(NewState#gstate.direction),
	Sin=math:sin(NewState#gstate.direction),

	{X,Y}=State#gstate.pos,
	X1=X+State#gstate.speed * Cos,
	Y1=Y-State#gstate.speed * Sin,

	RandDeltaDirection=fun() -> ((-100+rand_uniform(0,201))*0.01) * 0.1 end,
	%% increment generation counter
	NewState2=NewState#gstate{
		pos={X1,Y1},
		direction = NewState#gstate.direction + RandDeltaDirection(),
		generation=State#gstate.generation+1}, 
	%% tell GrazerCommunicator our new gstate
	NewState2#gstate.comm_pid ! {update_state, NewState2},
	grazer(Grazer, World, NewState2, State).

%% received grid_info from World
handle_grid_info(_World, Grid, State) ->

	{X,Y} = State#gstate.pos,
	I=?gridindex(X,Y),
	S=array:size(Grid),

	{V, Pids} =
	if 
		I>=0, I<S -> array:get(I, Grid);
		true -> {0,[]}
	end,

	if 
		V =/= 0 , Pids =/= []->
			S2=determine_direction(X,Y,V,Grid, State),
			S2#gstate{grid_ball_pids=Pids};
		true ->
			State
	end.	


determine_direction(X, Y, _V, Grid, State) ->
	Offsets=[
		{0, 1, 0}, {1, 1, -1}, {2, 0, -1}, {3, -1, -1},
		{4, -1, 0}, {5, -1, 1}, {6, 0, 1}, {7, 1, 1} ],
	L=array:size(Grid),	
	%% io:format("X=~p, Y=~p, V=~p, Grid=~p, State=~p~n", [X, Y, V, Grid, State]),
	F=fun({Dir, Xo, Yo}) ->
		X1=X+Xo * ?GRIDSIZE,
		Y1=Y+Yo * ?GRIDSIZE,
		I=?gridindex(X1, Y1),
		{V2, _Pids}=
		if
			I >=0, I<L ->
				array:get(I,Grid);
			true ->
				{0,[]}
		end,
		{Dir, V2}
	end,
	A=lists:map(F, Offsets),
	FSort=fun({_Dir1, W1}, {_Dir2,W2} ) -> W1 >= W2 end,

	{Dir, _B}=lists:nth(1, lists:sort(FSort, A)),
	NewDirection=math:pi() * 2 * Dir / 8, %% 4 quadrants, 

	%% NDir is a change of the original direction towards NewDirection
	NDir=delta_dir(NewDirection, State),
		
	%%io:format("~p changed direction by ~p degrees~n", [self(),  
		%% (NDir-State#gstate.direction) / math:pi() * 180]),

	State#gstate{direction=NDir}.

delta_dir(NewDirection, State)->
	OldDir=State#gstate.direction,
	DeltaDir=round(180*(NewDirection - OldDir)/math:pi())
		rem 360, %% between -359 and  + 359
	
	DDir = if
		DeltaDir < -180 -> DeltaDir + 360;
		DeltaDir > 180  -> DeltaDir - 360;
		true            -> DeltaDir
	end,

	OldDir + 
	math:pi() * 2 / 360 * 
	if 
		DDir < -90 -> -30;
		DDir < -45 -> -10;
		DDir >  45 ->  10;
		DDir >  90 ->  30;
		true       ->  0 
	end.	
	
eat_one(_, State) when State#gstate.grid_ball_pids =:= [] ->
	State;

eat_one(World, State) ->
	Pids=State#gstate.grid_ball_pids,
	{X,Y} = State#gstate.pos,
	GridIndex=?gridindex(X,Y),
	I=rand_uniform(1,1+length(Pids)),
	Pid=lists:nth(I, Pids),
	%% io:format("~p eating ~p~n", [self(), Pid]),
	Pid ! die,
	World ! {eaten, GridIndex, Pid},
	State#gstate{grid_ball_pids=lists:delete(Pid, Pids)}.

clone_grazer(Canvas, OldState) ->
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
	{X,Y}=OldState#gstate.pos,
	create_grazer(Canvas, self(), {X+DX, Y+DY}).


grazer_communicator(OldState) ->
	NewState=
	receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			OldState#gstate.grazer_process ! die,
			exit(normal);
		{set_grazer_process, Pid} ->
			OldState#gstate{grazer_process=Pid};
		{Pid, get_state} ->
			Pid ! {self(), info, OldState},
			OldState;
		{update_state, State} ->
			State#gstate{grazer_process=OldState#gstate.grazer_process};
		{_PidStatProcess, grid_info, Grid } ->
			OldState#gstate.grazer_process ! {grid_info, Grid},
			OldState;
		Message ->
			io:format("grazer_communicator ~p received unexpected ~p, ~nState=~p~n", 
				[self(), Message, OldState])
	end,
	grazer_communicator(NewState).
	

%% vim:tw=0
