-module(wolf_protocol_SUITE).
-include_lib("common_test/include/ct.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ wolf_entity_should_have_world_state_applied/1,
          wolf_entity_at_first_should_run_and_has_starting_position/1 ]).

all() ->
    [ wolf_entity_should_have_world_state_applied,
      wolf_entity_at_first_should_run_and_has_starting_position ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 0, rabbits = 0, wolves = 0, width = 5, height = 5},

    simulation_event_stream:start_link(),
    simulation_event_stream:attach_handler(common_test_event_handler),

    {ok, Pid} = simulation_entity_wolf:start_link({WorldParameters, {position, 1, 2}}),

    [ {wolf_entity_pid, Pid}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(wolf_entity_pid, Config),
    exit(Pid, normal).

wolf_entity_should_have_world_state_applied(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    Pid = proplists:get_value(wolf_entity_pid, Config),

    {wolf, born, EntityState} = common_test_event_handler:last_event_of(wolf, born),

    Pid = EntityState#wolf.pid,
    WorldParameters = EntityState#wolf.world.

wolf_entity_at_first_should_run_and_has_starting_position(Config) ->
    Pid = proplists:get_value(wolf_entity_pid, Config),

    {introspection, running, State} = gen_fsm:sync_send_all_state_event(Pid, introspection),

    #position{x = 1, y = 2} = State#wolf.position.
