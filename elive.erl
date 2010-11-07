-module(elive).
-compile(export_all).

-import(crypto, [rand_uniform/2, start/0]).

-record(state, {
	color=red,
	dsize=0.1,
	quadrants=[],
	generation=1,
	pos,
	size=2
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
	Pid = spawn(fun() -> create_ball(Canvas, World) end),
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
		Message ->
			io:format("Canvas got unexpected ~p~n", [Message]),
			loop(Canvas, Pids)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).

create_ball(Canvas, World) ->
	Pos = { rand_uniform(200,400), rand_uniform(200, 400) },
	create_ball(Canvas, World, Pos).

create_ball(Canvas, World, {X,Y}) ->
	State=#state{pos={X,Y}},
	Size = State#state.size, %% uses default value from recrd
	Self=self(),
	Color= State#state.color,
	Ball=gs:oval(Canvas,[{coords,[{X-Size, Y-Size}, {X+Size,Y+Size}]},{fill,Color}]),
	spawn_link( fun() -> ask_neighbour_info(Self, World) end ),
	ball(Ball, World, State ).



ball(Ball, World, OldState  ) ->
	{X,Y}=OldState#state.pos,
	Size = OldState#state.size + OldState#state.dsize,
	gs:config(Ball,[{coords,[{X-Size, Y-Size}, {X+Size, Y+Size}]}, 
		{fill, OldState#state.color}]),

	%% receive can lead to a state change
	NewState = 
	receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			exit(normal);
		{Pid, get_info} ->
			Pid ! {self(), info, OldState},
			OldState;
		{neighbour_info, QuadrantInfo} ->
			%io:format("~p Received neighbour_info= ~p ~n", [self(), QuadrantInfo]),
			Limit=50,
			QuadrantSum=lists:sum(QuadrantInfo),
			if
				QuadrantSum > 4 * Limit ->
					io:format("~p stopped growing~n", [self()]),
					OldState#state{dsize=0, color=green};
				true ->
					OldState#state{quadrants=QuadrantInfo}
			end;
		Message ->
			io:format("~p received unexpected ~p~n", [self(), Message]),
			OldState
	after 50 ->
		OldState
	end,
	NewState1=
	if
		Size > 20 -> NewState#state{dsize=0, color=blue};
		true -> NewState
	end,
	%% NewState is now set

	R=rand_uniform(0,100) ,
	if 
		NewState#state.generation rem  30 =:= 0, 
		NewState#state.color =:= red,
		NewState#state.quadrants =/= [],
		R > 80  ->
			World ! { self(), split, NewState1 };
		true ->
			true
	end,		
	ball(Ball, World, NewState1#state{
		size=Size,
		generation=OldState#state.generation+1}).


ask_neighbour_info(Owner, World) ->

	Owner ! { self(), get_info },
	receive
		{Owner, info, State} ->
			true
	end,
	Center=State#state.pos,

	World ! { self(), give_pids },
	receive
		{World, Pids} ->
			true
	end,

	Others=lists:filter(fun(Pid) -> Pid =/= Owner end, Pids),
	lists:map(fun(Pid) -> Pid ! {self(), get_info} end, Others),
	NeighbourInfo = collect_neighbour_info(Others),
	%% io:format("NeighbourInfo=~p~n", [NeighbourInfo]),
	QuadrantInfo=quadrant_info(NeighbourInfo, Center, 0, 0, 0, 0),
	Owner ! {neighbour_info, QuadrantInfo},
	receive
		after 1000 ->
			ask_neighbour_info(Owner, World)
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

	io:format("clone_ball quadrants = ~p~n", [ OldState#state.quadrants]),
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
			P=spawn(fun() -> create_ball(Canvas, World, {X+DX, Y+DY}) end),
			io:format("new ball at ~p,~p~n", [X+DX, Y+DY]),
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
