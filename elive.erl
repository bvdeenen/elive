-module(elive).
-compile(export_all).

-import(crypto, [rand_uniform/2, start/0]).

-record(state, {
	color=red,
	dsize=0.1,
	quadrants=[],
	generation=1,
	pos,
	size=2,
	comm_pid,
	ball_process,
	generation_interval=50,
	generation_die=250,
	generation_split=20
	}).

init() ->
	crypto:start(),
	I= gs:start(),
	W= gs:window(I,[{title,"Ball"},{width,600},{height,600},{map,true}]),
	Canvas= gs:canvas(W,[{width,600},{height,600},{bg,yellow}]),
	gs:create(button, quit, W, [{label, {text,"Quit Elive"}},{x,5}, {y, 5}]),
	Pids = create_balls(Canvas, self(), _Nballs=4),
	loop(Canvas, Pids),
	gs:stop().

create_balls(_, _World, 0) ->
	[];

create_balls( Canvas, World, Nballs) ->
	Pid = create_ball(Canvas, World),
	[Pid | create_balls(Canvas, World, Nballs-1)].

loop(Canvas, Pids) ->
	receive
		{Pid, give_pids} ->
			Pid ! {self(), Pids},
			loop(Canvas, Pids);
		{gs,_,destroy,_,_} ->
			quit_balls(Pids);
		{gs,quit,click,_,_} ->
			quit_balls(Pids);
		{gs, A, B, C, D} ->
			io:format("GS message ~p ~p ~p ~p~n", [A,B,C,D]);
		{ _Pid, split, OldState } ->
			Pid1=clone_ball(Canvas, OldState) ,
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
	spawn_link( fun() -> ask_neighbour_info(BallCommunicator, World) end ),
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

	R=rand_uniform(0,100) ,
	if 
		NewState#state.generation >  NewState#state.generation_split ,
		NewState#state.quadrants =/= [],
		R > 95  ->
			World ! { self(), split, NewState1 };
		true ->
			true
	end,		
	NewState2=NewState1#state{
		size=Size,
		generation=OldState#state.generation+1}, 
	%% tell BallCommunicator our new state
	NewState2#state.comm_pid ! {update_state, NewState2},
	if 	
		NewState2#state.generation  < NewState2#state.generation_die ->
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
		Message ->
			io:format("ball_communicator ~p received unexpected ~p, ~nState=~p~n", 
				[self(), Message, OldState])
	end,
	ball_communicator(NewState).
	


ask_neighbour_info(BallCommunicator, World) ->

	BallCommunicator ! { self(), get_state },
	receive
		{BallCommunicator, info, State} ->
			true
	end,
	Center=State#state.pos,

	World ! { self(), give_pids },
	receive
		{World, Pids} ->
			true
	end,

	Others=lists:filter(fun(Pid) -> Pid =/= BallCommunicator end, Pids),
	lists:map(fun(Pid) -> Pid ! {self(), get_state} end, Others),

	%%Others=[fun() -> Pid ! {self(), get_state} end || Pid <- Pids, Pid =/= BallCommunicator],
	%% io:format("ask_neighbour_info BallCommunicator=~p, Center=~p, Others=~p~n", [BallCommunicator,Center, Others]),
	NeighbourInfo = collect_neighbour_info(Others),
	%% io:format("NeighbourInfo=~p~n", [NeighbourInfo]),
	QuadrantInfo=quadrant_info(NeighbourInfo, Center, 0, 0, 0, 0),
	BallCommunicator ! {neighbour_info, QuadrantInfo},
	receive
		after 1000 ->
			ask_neighbour_info(BallCommunicator, World)
	end.

quadrant_info([], _Center, A, B, C, D) ->
	[A, B, C, D];

quadrant_info([{{X1,Y1}, OtherSize}|T], Center={X0,Y0}, A, B, C, D) ->

	Limit=150,
	if
		X0 < 30;
		Y0 < 30;
		X1 < (X0-Limit);
		X1 > (X0+Limit);
		Y1 < (Y0-Limit);
		Y1 > (Y0+Limit) -> quadrant_info(T, Center, A,B,C,D);

		X1 < X0  , Y1 < Y0 -> quadrant_info(T  , Center , A+OtherSize , B           , C           , D);
		X1 < X0  , Y1 >= Y0 -> quadrant_info(T , Center , A           , B+OtherSize , C           , D);
		X1 >= X0 , Y1 <Y0 -> quadrant_info(T   , Center , A           , B           , C+OtherSize , D);
		X1 >= X0 , Y1 >= Y0 -> quadrant_info(T , Center , A           , B           , C           , D+OtherSize);
		true -> 999
	end.


clone_ball(Canvas, OldState) ->
	Z=lists:zip( lists:seq(0,3), OldState#state.quadrants ),
	VLimit=50,
	SortF=fun({_N0, Q0},{_N1, Q1}) -> Q0 < Q1 end,
	Z1=lists:sort(SortF, Z),

	{I, Q} =lists:nth(1,Z1),
	
	{X,Y} = OldState#state.pos,

	%%io:format("clone_ball quadrants = ~p~n", [ OldState#state.quadrants]),
	F=fun() -> rand_uniform(20, 50) end,

	{DX,DY}=
	if 
		Q < VLimit -> 
			case I of 
				0 -> {-F(), -F() } ;
				1 -> {-F(),  F() } ;
				2 -> { F(), -F() } ;
				3 -> { F(),  F() } ;
				true -> { 0, 0}
			end	;
		true ->
			{no_clone, no_clone}
		end,

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

collect_neighbour_info([]) -> [] ;

collect_neighbour_info([H|T]) ->
	receive
		{H, info, State} ->
			[{State#state.pos, State#state.size} | collect_neighbour_info(T)]
	end.






%% vim:tw=0
