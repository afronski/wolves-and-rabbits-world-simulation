-module(simulation_controller_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ simulation_parameters_can_be_obtained_via_call/1,
          simulation_can_be_started_via_call/1,
          simulation_can_be_stopped_via_call/1,
          simulation_cannot_be_started_twice/1,
          simulation_cannot_be_stopped_twice/1 ]).

all() ->
    [ simulation_parameters_can_be_obtained_via_call,
      simulation_can_be_started_via_call,
      simulation_can_be_stopped_via_call,
      simulation_cannot_be_started_twice,
      simulation_cannot_be_stopped_twice ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 10, rabbits = 5, wolves = 2, width = 5, height = 5},

    simulation_event_stream:start_link(),

    {ok, Supervisor} = simulation_simulations_supervisor:start_link(WorldParameters),
    {ok, Pid} = simulation_controller:start_link(WorldParameters),

    [ {controller, Pid}, {supervisor, Supervisor}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(controller, Config),
    exit(Pid, normal),

    Supervisor = proplists:get_value(supervisor, Config),
    exit(Supervisor, normal).

simulation_parameters_can_be_obtained_via_call(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),

    { {width, Width},
      {height, Height},
      {simulation_started, IsSimulationStarted} } = simulation_controller:get_simulation_parameters(),

    Width = WorldParameters#world_parameters.width,
    Height = WorldParameters#world_parameters.height,
    IsSimulationStarted = false.

simulation_can_be_started_via_call(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),

    simulation_controller:start_simulation(),
    timer:sleep(500),

    [ _Specs1, {active, ActualCarrotsAmount}, _Supervisors1, _Workers1 ] = supervisor:count_children(simulation_carrots_supervisor),
    [ _Specs2, {active, ActualWolvesAmount}, _Supervisors2, _Workers2 ] = supervisor:count_children(simulation_wolves_supervisor),
    [ _Specs3, {active, ActualRabbitsAmount}, _Supervisors3, _Workers3 ] = supervisor:count_children(simulation_rabbits_supervisor),

    ?assertEqual(WorldParameters#world_parameters.carrots, ActualCarrotsAmount),
    ?assertEqual(WorldParameters#world_parameters.wolves, ActualWolvesAmount),
    ?assertEqual(WorldParameters#world_parameters.rabbits, ActualRabbitsAmount).

simulation_can_be_stopped_via_call(_Config) ->
    simulation_controller:start_simulation(),
    timer:sleep(500),

    simulation_controller:stop_simulation(),
    timer:sleep(500),

    [ _Specs1, {active, ActualCarrotsAmount}, _Supervisors1, _Workers1 ] = supervisor:count_children(simulation_carrots_supervisor),
    [ _Specs2, {active, ActualWolvesAmount}, _Supervisors2, _Workers2 ] = supervisor:count_children(simulation_wolves_supervisor),
    [ _Specs3, {active, ActualRabbitsAmount}, _Supervisors3, _Workers3 ] = supervisor:count_children(simulation_rabbits_supervisor),

    ?assertEqual(0, ActualCarrotsAmount),
    ?assertEqual(0, ActualWolvesAmount),
    ?assertEqual(0, ActualRabbitsAmount).

simulation_cannot_be_started_twice(_Config) ->
    started = simulation_controller:start_simulation(),
    already_started = simulation_controller:start_simulation().

simulation_cannot_be_stopped_twice(_Config) ->
    started = simulation_controller:start_simulation(),

    stopped = simulation_controller:stop_simulation(),
    already_stopped = simulation_controller:stop_simulation().
