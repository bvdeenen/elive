-module(elive).
-compile(export_all).

-import(random, [uniform/1, seed/3]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Ball"},{width,300},{height,300},{map,true}]),
    Canvas= gs:canvas(W,[{width,300},{height,300},{bg,yellow}]),
    %gs:button(W,[{label, {text,"Quit Demo"}},{x,100}]),
	gs:create(button, quit, W, [{label, {text,"Quit Demo"}},{x,100}]),
	Nballs=10,
	Pids = create_balls(Canvas, Nballs),
	loop(Canvas, Pids).

create_balls(_, 0) ->
	[];

create_balls( Canvas, Nballs) ->
	Pid = spawn(fun() -> ball(create, Nballs, Canvas) end),
	[Pid | create_balls(Canvas, Nballs-1)].

loop(Canvas, Pids) ->
    receive
		{gs,_,destroy,_,_} -> 	
			quit_balls(Pids),
			exit(normal);
		{gs,quit,click,_,_} -> 
			quit_balls(Pids),
			exit(normal);
		Message -> io:format("~p~n", [Message])
    end,
	loop(Canvas, Pids).

quit_balls(Pids) ->
	lists:map( fun(Pid) -> Pid ! die end, Pids).
	

ball(create, Index, Canvas) ->
	{A1,A2,A3}=now(),
	random:seed(A1, A2, A3+Index),
	{X,Y} = Pos = { uniform(250), uniform(250) },
	Size = uniform(10)+5,
	Color=red,
    Ball=gs:oval(Canvas,[{coords,[Pos ,{X+Size,Y+Size}]},{fill,Color}]),
	ball(Ball).
	
ball(Ball) ->
    receive
		die ->
			%% io:format("~p being told to die~n", [self()]),
			exit(normal);
		Message -> io:format("~p~n", [Message])
    after 20 ->
	    true
    end,
	[{X,_Y}, {X2,_Y2}]=gs:read(Ball, coords),
	_Size=X2-X,
	DX=-5+uniform(9),
	DY=-5+uniform(9),
	
    gs:config(Ball,{move,{DX,DY}}),    
    ball(Ball).


	
	

