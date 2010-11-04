-module(elive).
-compile(export_all).

-import(crypto, [rand_uniform/2, start/0]).

init() ->
	crypto:start(),
    I= gs:start(),
    W= gs:window(I,[{title,"Ball"},{width,300},{height,300},{map,true}]),
    Canvas= gs:canvas(W,[{width,300},{height,300},{bg,yellow}]),
	gs:create(button, quit, W, [{label, {text,"Quit Demo"}},{x,100}]),
	Pids = create_balls(Canvas, self(), _Nballs=30),
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
		Message -> 	
			io:format("~p~n", [Message]),
			loop(Canvas, Pids)
    end.

quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).
	
ball(create, _Index, Canvas, World) ->
	{X,Y} = Pos = { rand_uniform(1,250), rand_uniform(1, 250) },
	%io:format("Pos=~p~n", [Pos]),
	Size = rand_uniform(1,10)+5,
	Self=self(),
	Color=red,
    Ball=gs:oval(Canvas,[{coords,[Pos ,{X+Size,Y+Size}]},{fill,Color}]),
	spawn_link( fun() -> ask_neighbour_info(Self, Ball, World) end ),
	ball(Ball, World).
	
	
ball(Ball, World  ) ->
	[{X,Y}, {X2,Y2}]=gs:read(Ball, coords),
	DSize=0.1,
	PosN=[{X-DSize, Y-DSize}, {X2+DSize, Y2+DSize}],
	
    gs:config(Ball,{coords,PosN}),

    receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			exit(normal);
		{Pid, get_info} ->
			Pid ! {self(), info, PosN};
		{neighbour_info, Info} ->
			{A, B, C, D} = Info,
			io:format("~p Received neighbour_info= ~p ~n", [self(), Info]);
		Message ->
			io:format("~p received unexpected ~p~n", [self(), Message])
    after 20 ->
	    true
    end,
	ball(Ball, World).


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
	
	Limit=50,
	if
		X1 < X0-Limit; 
		X1 > X0+Limit;
		Y1 < Y0-Limit;
		Y1 > Y0+Limit -> quadrant_info(T, Center, A,B,C,D);
		
		X1 < X0, Y1 < Y0 -> quadrant_info(T, Center, A+OtherSize, B, C, D);
		X1 < X0, Y1 >= Y0 -> quadrant_info(T, Center, A, B+OtherSize, C, D);
		X1 > X0, Y1 <Y0 -> quadrant_info(T, Center, A, B, C+OtherSize, D);
		X1 > X0, Y1 >= Y0 -> quadrant_info(T, Center, A, B, C, D+OtherSize);
		true -> 999
	end.

	

collect_neighbour_info([]) -> [] ;

collect_neighbour_info([H|T]) ->
	receive
		{H, info, Info} ->
			[Info | collect_neighbour_info(T)]
	end.		


	

	
	
	
%% vim:tw=0
