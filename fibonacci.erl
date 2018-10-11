-module(fibonacci).
-export([f/1]).

f(N) when N >= 2 ->
	f(N - 1) + f (N - 2);

f(_) -> 1.
