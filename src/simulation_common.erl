-module(simulation_common).

-include("../include/simulation_records.hrl").

-export([ randomize/0, direction/0,
          next_position_and_target/2,
          next_position/4,
          stop_children/1 ]).

randomize() ->
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({A,B,C}).

direction() ->
    PossibleDirections = [ n, s, e, w, nw, ne, sw, se ],
    NewDirectionIndex = random:uniform(length(PossibleDirections)),

    lists:nth(NewDirectionIndex, PossibleDirections).

next_position_and_target(Position, Target) ->
    {X, Y} = {Target#target.x - Position#position.x, Target#target.y - Position#position.y},

    PossibleDirections = if
        X >  0, Y >  0, abs(X) == abs(Y) -> [ne];
        X >  0, Y >  0                   -> [n, e];
        X >  0, Y <  0, abs(X) == abs(Y) -> [se];
        X >  0, Y <  0                   -> [s, e];
        X >  0, Y == 0                   -> [e];
        X <  0, Y == 0                   -> [w];
        X == 0, Y >  0                   -> [n];
        X == 0, Y <  0                   -> [s];
        X <  0, Y >  0, abs(X) == abs(Y) -> [nw];
        X <  0, Y >  0                   -> [n, w];
        X <  0, Y <  0, abs(X) == abs(Y) -> [sw];
        X <  0, Y <  0                   -> [s, w];
        X == 0, Y == 0                   -> [n, s, e, w, nw, ne, sw, se]
    end,

    randomize(),

    NewDirectionIndex = random:uniform(length(PossibleDirections)),
    NewDirection = lists:nth(NewDirectionIndex, PossibleDirections),
    NewPosition = next_position(Position, NewDirection),
    {NewPosition, NewDirection}.

next_position(World, Position, Direction, NewPositionValid) when not NewPositionValid ->
    NewPosition = next_position(Position, Direction),
    {WestLimit, SouthLimit, EastLimit, NorthLimit} = {0, 0, World#world_parameters.width, World#world_parameters.height},

    randomize(),

    case {NewPosition#position.x, NewPosition#position.y} of
        {WestLimit, _}  -> next_position(World, Position, new_direction(World, Position, Direction), false);
        {_, SouthLimit} -> next_position(World, Position, new_direction(World, Position, Direction), false);
        {EastLimit, _}  -> next_position(World, Position, new_direction(World, Position, Direction), false);
        {_, NorthLimit} -> next_position(World, Position, new_direction(World, Position, Direction), false);
        _               -> next_position(World, NewPosition, Direction, true)
    end;

next_position(_World, Position, Direction, _NewPositionValid) ->
    {Position, Direction}.

stop_children(SupervisorName) ->
    [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].

%% Internal functions.

new_direction(World, Position, Direction) ->
    {West, South, East, North} = {1, 1, World#world_parameters.width - 1, World#world_parameters.height - 1},

    PossibleDirections = case {Direction, Position#position.x, Position#position.y} of
        {n, West, _}      -> [s, e, se];
        {n, East, _}      -> [s, w, sw];
        {n, _, _}         -> [s, e, w, se, sw];
        {s, West, _}      -> [n, e, ne];
        {s, East, _}      -> [n, w, nw];
        {s, _, _}         -> [n, e, w, ne, nw];
        {e, _, South}     -> [n, w, nw];
        {e, _, North}     -> [s, w, sw];
        {e, _, _}         -> [n, s, w, nw, sw];
        {w, _, South}     -> [n, e, ne];
        {w, _, North}     -> [s, e, se];
        {w, _, _}         -> [n, s, e, ne, se];
        {nw, West, North} -> [s, e, se];
        {nw, West, _}     -> [n, s, e, ne, se];
        {nw, _, North}    -> [s, e, w, se, sw];
        {ne, East, North} -> [s, w, sw];
        {ne, East, _}     -> [n, s, w, nw, sw];
        {ne, _, North}    -> [s, e, w, se, sw];
        {sw, West, South} -> [n, e, ne];
        {sw, West, _}     -> [n, s, e, ne, se];
        {sw, _, South}    -> [n, e, w, ne, nw];
        {se, East, South} -> [n, w, nw];
        {se, East, _}     -> [n, s, w, nw, sw];
        {se, _, South}    -> [n, e, w, ne, nw]
    end,

    NewDirectionIndex = random:uniform(length(PossibleDirections)),
    lists:nth(NewDirectionIndex, PossibleDirections).

next_position(Position, Direction) ->
    {X, Y} = {Position#position.x, Position#position.y},

    case Direction of
        n  -> #position{x=X, y=Y+1};
        s  -> #position{x=X, y=Y-1};
        e  -> #position{x=X+1, y=Y};
        w  -> #position{x=X-1, y=Y};
        nw -> #position{x=X-1, y=Y+1};
        ne -> #position{x=X+1, y=Y+1};
        sw -> #position{x=X-1, y=Y-1};
        se -> #position{x=X+1, y=Y-1}
    end.
