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

-define(WORLDSIZE, 600).
-define(GRIDSIZE, 50).

gridindex(X,Y) ->
	(X div ?GRIDSIZE) + (Y div ?GRIDSIZE) * (?WORLDSIZE div ?GRIDSIZE).

