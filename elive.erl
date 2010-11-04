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
	spawn_link( fun() -> ask_neighbour_info(Self, World) end ),
	Color=red,
    Ball=gs:oval(Canvas,[{coords,[Pos ,{X+Size,Y+Size}]},{fill,Color}]),
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
			%[{X1,Y1}, {X2,Y2}] = Info,
			io:format("~p Received neighbour_info= ~p ~n", [self(),
			length(Info)]);
		Message ->
			io:format("~p received unexpected ~p~n", [self(), Message])
    after 20 ->
	    true
    end,
	ball(Ball, World).


ask_neighbour_info(Owner, World) ->
	
	World ! { self(), give_pids },
	receive
		{World, Pids} ->
			true
	end,

	Others=lists:filter(fun(Pid) -> Pid =/= Owner end, Pids),
	lists:map(fun(Pid) -> Pid ! {self(), get_info} end, Others),
	NeighbourInfo = collect_neighbour_info(Others),
	%io:format("NeighbourInfo=~p~n", [NeighbourInfo]),
	Owner ! {neighbour_info, NeighbourInfo}.



collect_neighbour_info([]) -> [] ;

collect_neighbour_info([H|T]) ->
	receive
		{H, info, Info} ->
			[Info | collect_neighbour_info(T)]
	end.		


	

	
	
	

