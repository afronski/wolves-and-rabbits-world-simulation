-module(simulation_common).

-export([ randomize/0, direction/0, stop_children/1 ]).

randomize() ->
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({A,B,C}).

direction() ->
    PossibleDirections = [ n, s, e, w, nw, ne, sw, se ],
    NewDirectionIndex = random:uniform(length(PossibleDirections)),

    lists:nth(NewDirectionIndex, PossibleDirections).

stop_children(SupervisorName) ->
    [ Pid ! stop_entity || {_, Pid, _, _} <- supervisor:which_children(SupervisorName) ].
