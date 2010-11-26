-record(state, {
	color=red,
	dsize=0.1,
	generation=1,
	pos,
	size=2,
	comm_pid,
	ball_process,
	generation_interval=50,
	generation_die=250,
	generation_split=20
	}).

-record(gstate, {
	color=yellow,
	generation=1,
	pos,
	lsize=20,
	wsize=7,
	speed=1,
	direction=0,
	comm_pid,
	grazer_process,
	generation_interval=200,
	generation_die=250,
	generation_split=20,
	food_state=20,
	grid_ball_pids=[] %% pids of balls in current grid cell
	}).

-define(WORLDSIZE, 600).
-define(GRIDSIZE, 50).

-define(gridindex(X,Y), 
	(round(X) div ?GRIDSIZE) + (round(Y) div ?GRIDSIZE) * (?WORLDSIZE div ?GRIDSIZE)).

