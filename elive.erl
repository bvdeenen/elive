-module(elive).
-compile(export_all).

-import(random, [uniform/1, seed/3]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Ball"},{width,300},{height,300},{map,true}]),
    C= gs:canvas(W,[{width,300},{height,300},{bg,yellow}]),
    gs:button(W,[{label, {text,"Quit Demo"}},{x,100}]),

	create_balls(C, lists:seq(1,10)).

create_balls(_, []) ->
	[];

create_balls( C, [H|T])  ->
	Pos = { uniform(250), uniform(250) },
	Size = uniform(10)+5,
	Ball=draw_ball(C, Pos, Size, red),
	spawn(fun() -> ball(Ball, H) end),
	create_balls(C, T).
	
draw_ball(C, Pos, Size, Color) ->
	{X,Y}=Pos,
    gs:oval(C,[{coords,[Pos ,{X+Size,Y+Size}]},{fill,Color}]).


ball(Ball, Index) ->
	{A1,A2,A3}=now(),
	random:seed(A1, A2, A3+Index),
	ball(Ball).
	
ball(Ball) ->
	[{X,Y}, {X2,Y2}]=gs:read(Ball, coords),
	Size=X2-X,
	DX=-5+uniform(9),
	DY=-5+uniform(9),

	
    gs:config(Ball,{move,{DX,DY}}),    
    receive
	{gs,_,click,_,_} -> exit(normal);
	{gs,_,destroy,_,_} -> exit(normal)
    after 20 ->
	    true
    end,
    ball(Ball).


	
	

