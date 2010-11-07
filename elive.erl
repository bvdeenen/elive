-module(elive).
-compile(export_all).

-import(crypto, [rand_uniform/2, start/0]).

-record(state, {color=red,
	dsize=0.1,
	quadrants=[],
	generation=1
	}).

init() ->
	crypto:start(),
	I= gs:start(),
	W= gs:window(I,[{title,"Ball"},{width,600},{height,600},{map,true}]),
	Canvas= gs:canvas(W,[{width,600},{height,600},{bg,yellow}]),
	gs:create(button, quit, W, [{label, {text,"Quit Demo"}},{x,100}]),
	Pids = create_balls(Canvas, self(), _Nballs=10),
	loop(Canvas, Pids),
	gs:stop().

create_balls(_, _World, 0) ->
	[];

create_balls( Canvas, World, Nballs) ->
	Pid = spawn(fun() -> ball(create, Nballs, Canvas, World) end),
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
		{ _Pid, split, Ball, OldState } when OldState#state.quadrants =/= [] ->
			Pid1=clone_ball(Ball, Canvas, OldState) ,
			case Pid1 of
				false -> loop(Canvas, Pids)	;
				Pid1 ->  loop(Canvas, [Pid1 | Pids] )
			end	;
		Message ->
			io:format("~p~n", [Message]),
			loop(Canvas, Pids)
	end.


quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).

ball(create, _Index, Canvas, World) ->
	Pos = { rand_uniform(200,400), rand_uniform(200, 400) },
	ball(create, _Index, Canvas, World, Pos).

ball(create, _Index, Canvas, World, Pos) ->
	{X,Y} = Pos ,
	%io:format("Pos=~p~n", [Pos]),
	Size = rand_uniform(1,10)+5,
	Self=self(),
	Color=red,
	Ball=gs:oval(Canvas,[{coords,[Pos ,{X+Size,Y+Size}]},{fill,Color}]),
	spawn_link( fun() -> ask_neighbour_info(Self, Ball, World) end ),
	ball(Ball, World, #state{} ).


ball(Ball, World, OldState  ) ->
	Coords=gs:read(Ball, coords),
	[{X,Y}, {X2,Y2}] = Coords,
	DSize = OldState#state.dsize,
	PosN=[{X-DSize, Y-DSize}, {X2+DSize, Y2+DSize}],

	gs:config(Ball,{coords,PosN}),

	%% receive can lead to a state change
	NewState = 
	receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			exit(normal);
		{Pid, get_info} ->
			Pid ! {self(), info, PosN},
			OldState;
		{neighbour_info, QuadrantInfo} ->
			{A, B, C, D} = QuadrantInfo,
			%io:format("~p Received neighbour_info= ~p ~n", [self(), QuadrantInfo]),
			Limit=50,
			if
				A > Limit, B>Limit, C>Limit, D>Limit ->
					io:format("~p stopped growing~n", [self()]),
					gs:config(Ball, {fill, green}),
					OldState#state{dsize=0, color=green};
				true ->
					OldState#state{quadrants=QuadrantInfo}
			end;

		Message ->
			io:format("~p received unexpected ~p~n", [self(), Message]),
			OldState
	after 100 ->
		if
			X2-X+2*DSize > 80 -> OldState#state{dsize=0};
			true -> OldState
		end
	end,
	%% NewState is now set

	if 
		OldState#state.generation rem  30 =:= 0 ->
			World ! { self(), split, Ball, NewState };
		true ->
			true
	end,		
	ball(Ball, World, NewState#state{generation=OldState#state.generation+1}).


ask_neighbour_info(Owner, Ball, World) ->

	World ! { self(), give_pids },
	receive
		{World, Pids} ->
			true
	end,

	Others=lists:filter(fun(Pid) -> Pid =/= Owner end, Pids),
	lists:map(fun(Pid) -> Pid ! {self(), get_info} end, Others),
	NeighbourInfo = collect_neighbour_info(Others),
	%io:format("NeighbourInfo=~p~n", [NeighbourInfo]),
	[{X,Y}, {X2,Y2}]=gs:read(Ball, coords),
	Center={ (X2+X)/2, (Y+Y2)/2 },

	QuadrantInfo=quadrant_info(NeighbourInfo, Center, 0, 0, 0, 0),
	Owner ! {neighbour_info, QuadrantInfo},
	receive
		after 1000 ->
			ask_neighbour_info(Owner, Ball, World)
	end.

quadrant_info([], _Center, A, B, C, D) ->
	{A, B, C, D};

quadrant_info([[{X3,Y3}, {X2,Y2}]|T], Center, A, B, C, D) ->
	{X0,Y0}=Center,
	OtherCenter={ (X2+X3)/2, (Y3+Y2)/2 },
	{X1,Y1}=OtherCenter,
	OtherSize = X2-X3,

	Limit=150,
	if
		X1 < (X0-Limit);
		X1 > (X0+Limit);
		Y1 < (Y0-Limit);
		Y1 > (Y0+Limit) -> quadrant_info(T, Center, A,B,C,D);

		X1 < X0, Y1 < Y0 -> quadrant_info(T, Center, A+OtherSize, B, C, D);
		X1 < X0, Y1 >= Y0 -> quadrant_info(T, Center, A, B+OtherSize, C, D);
		X1 >= X0, Y1 <Y0 -> quadrant_info(T, Center, A, B, C+OtherSize, D);
		X1 >= X0, Y1 >= Y0 -> quadrant_info(T, Center, A, B, C, D+OtherSize);
		true -> 999
	end.


clone_ball(Ball, Canvas, OldState) ->
	io:format("trying to clone ~p ~p ~n", [Ball, OldState]),
	{A,B,C,D} = OldState#state.quadrants,
	VLimit=50,
	{DLo, DLimit}={20, 150},
	[{X,Y}, {_X2,_Y2}]=gs:read(Ball, coords),

	{DX,DY}=
	if 
		A < VLimit ->
			{-rand_uniform(DLo, DLimit), -rand_uniform(DLo, DLimit) };
		B < VLimit ->
			{-rand_uniform(DLo, DLimit), rand_uniform(DLo, DLimit) };
		C < VLimit ->
			{rand_uniform(DLo, DLimit), -rand_uniform(DLo, DLimit) };
		D < VLimit ->
			{rand_uniform(DLo, DLimit), rand_uniform(DLo, DLimit) };
		true ->
			{0,0}
	end,

	World = self(),
	Pid=
	if 
		{DX,DY} =/= {0,0} ->
			
			P=spawn(fun() -> ball(create, DX, Canvas, World, {X+DX, Y+DY}) end),
			io:format("new ball at ~p,~p~n", [X+DX, Y+DY]),
			P;
		true ->
			false
	end,
	Pid.

collect_neighbour_info([]) -> [] ;

collect_neighbour_info([H|T]) ->
	receive
		{H, info, Info} ->
			[Info | collect_neighbour_info(T)]
	end.







%% vim:tw=0
