-module(simulation_supervisors_structure_SUITE).
-include_lib("common_test/include/ct.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ all_four_supervisors_should_be_available/1,
          all_carrots_should_be_spawned_when_board_was_populated/1,
          all_wolves_should_be_spawned_when_board_was_populated/1,
          all_rabbits_should_be_spawned_when_board_was_populated/1,
          all_entities_should_be_killed_when_simulation_was_stopped/1,
          start_stop_cycle_should_spawn_all_entities/1 ]).

all() ->
    [ all_four_supervisors_should_be_available,
      all_carrots_should_be_spawned_when_board_was_populated,
      all_wolves_should_be_spawned_when_board_was_populated,
      all_rabbits_should_be_spawned_when_board_was_populated,
      all_entities_should_be_killed_when_simulation_was_stopped,
      start_stop_cycle_should_spawn_all_entities ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 20, rabbits = 10, wolves = 5, width = 5, height = 5},

    simulation_event_stream:start_link(),
    simulation_event_stream:attach_handler(common_test_event_handler),

    {ok, Pid} = simulation_simulations_supervisor:start_link(WorldParameters),
    simulation_simulations_supervisor:populate(WorldParameters),

    [ {simulation_supervisors_supervisor_pid, Pid}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(simulation_supervisors_supervisor_pid, Config),
    exit(Pid, normal).

all_four_supervisors_should_be_available(_Config) ->
    true = is_pid(whereis(simulation_carrots_supervisor)),
    true = is_pid(whereis(simulation_wolves_supervisor)),
    true = is_pid(whereis(simulation_rabbits_supervisor)),

    true = is_pid(whereis(simulation_simulations_supervisor)).

all_carrots_should_be_spawned_when_board_was_populated(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    ExpectedCarrotsAmount = WorldParameters#world_parameters.carrots,

    [ _Specs, {active, ActualCarrotsAmount}, _Supervisors, _Workers ] = supervisor:count_children(simulation_carrots_supervisor),

    ExpectedCarrotsAmount = ActualCarrotsAmount.

all_wolves_should_be_spawned_when_board_was_populated(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    ExpectedWolvesAmount = WorldParameters#world_parameters.wolves,

    [ _Specs, {active, ActualWolvesAmount}, _Supervisors, _Workers ] = supervisor:count_children(simulation_wolves_supervisor),

    ExpectedWolvesAmount = ActualWolvesAmount.

all_rabbits_should_be_spawned_when_board_was_populated(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    ExpectedRabbitsAmount = WorldParameters#world_parameters.rabbits,

    [ _Specs, {active, ActualRabbitsAmount}, _Supervisors, _Workers ] = supervisor:count_children(simulation_rabbits_supervisor),

    ExpectedRabbitsAmount = ActualRabbitsAmount.

all_entities_should_be_killed_when_simulation_was_stopped(_Config) ->
    simulation_simulations_supervisor:restart(),
    timer:sleep(200),

    [ _Specs1, {active, ActualCarrotsAmount}, _Supervisors1, _Workers1 ] = supervisor:count_children(simulation_carrots_supervisor),
    [ _Specs2, {active, ActualWolvesAmount}, _Supervisors2, _Workers2 ] = supervisor:count_children(simulation_wolves_supervisor),
    [ _Specs3, {active, ActualRabbitsAmount}, _Supervisors3, _Workers3 ] = supervisor:count_children(simulation_rabbits_supervisor),

    0 = ActualCarrotsAmount,
    0 = ActualWolvesAmount,
    0 = ActualRabbitsAmount.

start_stop_cycle_should_spawn_all_entities(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),

    ExpectedCarrotsAmount = WorldParameters#world_parameters.carrots,
    ExpectedWolvesAmount = WorldParameters#world_parameters.wolves,
    ExpectedRabbitsAmount = WorldParameters#world_parameters.rabbits,

    simulation_simulations_supervisor:restart(),
    simulation_simulations_supervisor:populate(WorldParameters),
    timer:sleep(200),

    [ _Specs1, {active, ActualCarrotsAmount}, _Supervisors1, _Workers1 ] = supervisor:count_children(simulation_carrots_supervisor),
    [ _Specs2, {active, ActualWolvesAmount}, _Supervisors2, _Workers2 ] = supervisor:count_children(simulation_wolves_supervisor),
    [ _Specs3, {active, ActualRabbitsAmount}, _Supervisors3, _Workers3 ] = supervisor:count_children(simulation_rabbits_supervisor),

    ExpectedCarrotsAmount = ActualCarrotsAmount,
    ExpectedWolvesAmount = ActualWolvesAmount,
    ExpectedRabbitsAmount = ActualRabbitsAmount.
