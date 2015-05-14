-module(rabbit_protocol_SUITE).
-include_lib("common_test/include/ct.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ rabbit_entity_should_have_world_state_applied/1,
          rabbit_entity_at_first_should_run_and_has_starting_position/1,
          rabbit_entity_should_report_position_when_asked/1,
          rabbit_entity_should_answer_is_it_at_specified_position/1,
          rabbit_entity_should_answer_is_it_near_specified_position/1 ]).

all() ->
    [ rabbit_entity_should_have_world_state_applied,
      rabbit_entity_at_first_should_run_and_has_starting_position,
      rabbit_entity_should_report_position_when_asked,
      rabbit_entity_should_answer_is_it_at_specified_position,
      rabbit_entity_should_answer_is_it_near_specified_position ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 0, rabbits = 0, wolves = 0, width = 5, height = 5},

    simulation_event_stream:start_link(),
    simulation_event_stream:attach_handler(common_test_event_handler),

    {ok, Pid} = simulation_entity_rabbit:start_link({WorldParameters, {position, 1, 2}}),

    [ {rabbit_entity_pid, Pid}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(rabbit_entity_pid, Config),
    exit(Pid, normal).

rabbit_entity_should_have_world_state_applied(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    Pid = proplists:get_value(rabbit_entity_pid, Config),

    {rabbit, born, EntityState} = common_test_event_handler:last_event_of(rabbit, born),

    Pid = EntityState#rabbit.pid,
    WorldParameters = EntityState#rabbit.world.

rabbit_entity_at_first_should_run_and_has_starting_position(Config) ->
    Pid = proplists:get_value(rabbit_entity_pid, Config),

    {introspection, running, State} = gen_fsm:sync_send_all_state_event(Pid, introspection),

    #position{x = 1, y = 2} = State#rabbit.position.

rabbit_entity_should_report_position_when_asked(Config) ->
    Pid = proplists:get_value(rabbit_entity_pid, Config),

    #position{x = 1, y = 2} = gen_fsm:sync_send_all_state_event(Pid, where_are_you).

rabbit_entity_should_answer_is_it_at_specified_position(Config) ->
    Pid = proplists:get_value(rabbit_entity_pid, Config),

    true = gen_fsm:sync_send_all_state_event(Pid, {are_you_at, #position{x = 1, y = 2 }}),
    false = gen_fsm:sync_send_all_state_event(Pid, {are_you_at, #position{x = 0, y = 0}}).

rabbit_entity_should_answer_is_it_near_specified_position(Config) ->
    Pid = proplists:get_value(rabbit_entity_pid, Config),

    true = gen_fsm:sync_send_all_state_event(Pid, {are_you_near, #position{x = 2, y = 3}}),
    true = gen_fsm:sync_send_all_state_event(Pid, {are_you_near, #position{x = 0, y = 0}}),
    true = gen_fsm:sync_send_all_state_event(Pid, {are_you_near, #position{x = 4, y = 4}}),

    false = gen_fsm:sync_send_all_state_event(Pid, {are_you_near, #position{x = 5, y = 5}}).
