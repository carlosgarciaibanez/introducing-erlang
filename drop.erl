-module(drop).
-export([drop/0, falling_speed/1]).
-include("records.hrl").

drop() ->
	setup(),
	handle_drops().

handle_drops() ->
	receive
		{ From, Planemo, Distance } -> 
			From ! { Planemo, Distance, falling_speed(Planemo, Distance) }, 
			handle_drops();
		{ From, #tower{ planemo = Planemo, height = Distance} = Tower } ->
			From ! { Planemo, Distance, falling_speed(Tower)},
			handle_drops()
	end.

%falling_speed(#tower{} = T) ->
%	falling_speed(T#tower.planemo, T#tower.height).

falling_speed(#tower{ planemo = Planemo, height = Distance } = T ) ->
	io:format(
		"From ~s's elevation of ~p metrers on ~p, the object will reach ~p m/s before crashing in ~s~n", 
		[T#tower.name, Distance, Planemo, falling_speed(Planemo, Distance), T#tower.location]).

%falling_speed(#tower{planemo=Planemo, height=Distance}) ->
%	falling_speed(Planemo, Distance).

%falling_speed(earth, Distance) when Distance >= 0 ->
%	falling_speed_with_gravity(9.8, Distance);
%falling_speed(moon, Distance) when Distance >= 0 ->
%	falling_speed_with_gravity(1.6, Distance);
%falling_speed(mars, Distance) when Distance >= 0 ->
%	falling_speed_with_gravity(3.71, Distance).

falling_speed(Planemo, Distance) when Distance >= 0 ->
	P = hd(ets:lookup(planemos, Planemo)),
	falling_speed_with_gravity(P#planemo.gravity, Distance).
	
falling_speed_with_gravity(Gravity, Distance) -> 
	try math:sqrt(Gravity * 2 * Distance) of
		Result -> Result
	catch
		error:Error -> {error, Error}
	end.

setup() ->
    ets:new(planemos,[named_table, {keypos, #planemo.name}]),
    ets:insert(planemos, #planemo{ name = mercury,  gravity = 3.7,  diameter = 4878,    distance_from_sun = 57.9 }),
    ets:insert(planemos, #planemo{ name = venus,    gravity = 8.9,  diameter = 12104,   distance_from_sun = 108.2 }),
    ets:insert(planemos, #planemo{ name = earth,    gravity = 9.8,  diameter = 12756,   distance_from_sun = 149.6 }),
    ets:insert(planemos, #planemo{ name = moon,     gravity = 1.6,  diameter = 3475,    distance_from_sun = 149.6 }),
    ets:insert(planemos, #planemo{ name = mars,     gravity = 3.7,  diameter = 6787,    distance_from_sun = 227.9 }),
    ets:insert(planemos, #planemo{ name = ceres,    gravity = 0.27, diameter = 950,     distance_from_sun = 413.7 }),
    ets:insert(planemos, #planemo{ name = jupiter,  gravity = 23.1, diameter = 142796,  distance_from_sun = 778.3 }),
    ets:insert(planemos, #planemo{ name = saturn,   gravity = 9.0,  diameter = 120660,  distance_from_sun = 1427.0 }),
    ets:insert(planemos, #planemo{ name = uranus,   gravity = 8.7,  diameter = 51118,   distance_from_sun = 2871.0 }),
    ets:insert(planemos, #planemo{ name = neptune,  gravity = 11.0, diameter = 30200,   distance_from_sun = 4497.1 }),
    ets:insert(planemos, #planemo{ name = pluto,    gravity = 0.6,  diameter = 2300,    distance_from_sun = 5913.0 }),
    ets:insert(planemos, #planemo{ name = haumea,   gravity = 0.44, diameter = 1150,    distance_from_sun = 6484.0 }),
    ets:insert(planemos, #planemo{ name = makemake, gravity = 0.5,  diameter = 1500,    distance_from_sun = 6850.0 }),
    ets:insert(planemos, #planemo{ name = eris,     gravity = 0.8,  diameter = 2400,    distance_from_sun = 10210.0 }).