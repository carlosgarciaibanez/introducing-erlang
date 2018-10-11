-module(convert).
-export([mps_to_kph/1, mps_to_mph/1]).

mps_to_kph(Speed) -> 3.6 * Speed.
mps_to_mph(Speed) -> 2.23693629 * Speed.
