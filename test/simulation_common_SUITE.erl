-module(simulation_common_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0 ]).
-export([ randomized_should_not_return_undefined/1,
          chasing_in_one_direction_only/1,
          calculating_valid_next_position/1,
          calculating_invalid_next_position/1 ]).

all() ->
    [ randomized_should_not_return_undefined,
      chasing_in_one_direction_only,
      calculating_valid_next_position,
      calculating_invalid_next_position ].

randomized_should_not_return_undefined(_Config) ->
    ?assertNot(simulation_common:randomize() =/= undefined).

chasing_in_one_direction_only(_Config) ->
    ?assertEqual({#position{x = 2, y = 2}, ne},
                 simulation_common:next_position_and_target(#position{x = 1, y = 1}, #target{x = 2, y = 2})),
    ?assertEqual({#position{x = 0, y = 0}, sw},
                 simulation_common:next_position_and_target(#position{x = 1, y = 1}, #target{x = 0, y = 0})),
    ?assertEqual({#position{x = 0, y = 2}, nw},
                 simulation_common:next_position_and_target(#position{x = 1, y = 1}, #target{x = 0, y = 2})),
    ?assertEqual({#position{x = 2, y = 0}, se},
                 simulation_common:next_position_and_target(#position{x = 1, y = 1}, #target{x = 2, y = 0})).

calculating_valid_next_position(_Config) ->
    ?assertEqual({#position{x = 1, y = 1}, n},
                 simulation_common:next_position(#world_parameters{}, #position{x = 1, y = 1}, n, true)).

calculating_invalid_next_position(_Config) ->
    ?assertEqual({#position{x = 1, y = 2}, n},
                 simulation_common:next_position(#world_parameters{}, #position{x = 1, y = 1}, n, false)).
