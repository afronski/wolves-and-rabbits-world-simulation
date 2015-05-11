-module(simulation_controller_SUITE).
-include_lib("common_test/include/ct.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ simulation_parameters_can_be_obtained_via_call/1 ]).

all() ->
    [ simulation_parameters_can_be_obtained_via_call ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 0, rabbits = 0, wolves = 0, width = 5, height = 5},

    simulation_event_stream:start_link(),
    {ok, Pid} = simulation_controller:start_link(WorldParameters),

    [ {controller, Pid}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(controller, Config),
    exit(Pid, normal).

simulation_parameters_can_be_obtained_via_call(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),

    { {width, Width},
      {height, Height},
      {simulation_started, IsSimulationStarted} } = simulation_controller:get_simulation_parameters(),

    Width = WorldParameters#world_parameters.width,
    Height = WorldParameters#world_parameters.height,
    IsSimulationStarted = false.
