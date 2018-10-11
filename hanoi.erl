-module(hanoi).
-export([hanoi/1]).

hanoi(Disks) when Disks > 0 ->
	Moves = hanoi(Disks, {origin, aux, target}),
	io:format("~w moves made~n", [Moves]).

hanoi(Disks, {Origin, Aux, Target}) when Disks > 1 ->
	A = hanoi(Disks - 1, {Origin, Target, Aux}),
	io:format("From ~w to ~w~n", [Origin, Target]),
	B = hanoi(Disks - 1, {Aux, Origin, Target}),
	A + B + 1;

hanoi(_, {Origin, _, Target}) ->
	io:format("From ~w to ~w~n", [Origin, Target]),
	1.
