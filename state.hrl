-record(state, {
	color =red :: atom(),
	dsize=0.1 :: float(),
	generation=1 :: integer(),
	pos :: list(),
	size=2,
	comm_pid :: pid(),
	ball_process :: pid(),
	generation_interval=50,
	generation_die=250,
	generation_split=20
	}).

-record(gstate, {
	color=yellow :: atom(),
	generation=1 :: integer(),
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
	food_enough = 100,
	grid_ball_pids=[] %% pids of balls in current grid cell
	}).

-define(WORLDSIZE, 600).
-define(GRIDSIZE, 50).

-define(gridindex(X,Y), 
	(round(X) div ?GRIDSIZE) + (round(Y) div ?GRIDSIZE) * (?WORLDSIZE div ?GRIDSIZE)).

