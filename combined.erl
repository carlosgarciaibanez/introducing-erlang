-module(combined).
-import(drop, [falling_speed/1]).
-import(convert, [mps_to_mph/1]).
-export([height_to_mph/1]).

height_to_mph(Meters) -> mps_to_mph(falling_speed(Meters)).
