-module(worldrunner).
-compile(export_all).

main([A]) ->
	I = list_to_integer(atom_to_list(A)),
	world:start(I).
