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
	Grazer=gs:polygon(Canvas,[
		{coords,coords(State)},{fill,Color}]),
	GrazerCommunicator=spawn_link( fun() -> grazer_communicator(State) end),
	GrazerProcess=spawn_link( fun() -> grazer(Grazer, World, 
		State#gstate{comm_pid=GrazerCommunicator}, false) end ),
	GrazerCommunicator ! { set_grazer_process, GrazerProcess},
	GrazerCommunicator.


grazer(Grazer, World, State, _OldState  ) ->
	
	
	gs:config(Grazer, [{coords, coords(State)},
	{fill, State#gstate.color}]),

	NewState = 
	receive
		die ->
			exit(normal);
		{grid_info, Grid } ->
			handle_grid_info(World, Grid, State)
	after State#gstate.generation_interval ->
		State
	end,
	%% NewState is now set
	Cos=math:cos(State#gstate.direction),
	Sin=math:sin(State#gstate.direction),

	{X,Y}=State#gstate.pos,
	X1=X+State#gstate.speed * Cos,
	Y1=Y-State#gstate.speed * Sin,

	RandDeltaDirection=fun() -> ((-100+rand_uniform(0,201))*0.01) * 0.01 end,
	%% check if the size has reached the maximum value
	%% increment generation counter
	NewState2=NewState#gstate{
		pos={X1,Y1},
		direction = State#gstate.direction + RandDeltaDirection(),
		generation=State#gstate.generation+1}, 
	%% tell GrazerCommunicator our new gstate
	NewState2#gstate.comm_pid ! {update_state, NewState2},
	grazer(Grazer, World, NewState2, State).


handle_grid_info(_World, _Grid, State) ->
	State.


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
