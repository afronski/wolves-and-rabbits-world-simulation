-module(carrot_protocol_SUITE).
-include_lib("common_test/include/ct.hrl").

-include_lib("../include/simulation_records.hrl").

-export([ all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([ carrot_entity_should_have_world_state_applied/1,
          carrot_entity_should_notify_about_state_changes/1,
          carrot_entity_can_be_asked_about_its_position/1,
          carrot_entity_can_be_bitten/1,
          carrot_entity_when_eaten_should_be_stopped/1 ]).

all() ->
    [ carrot_entity_should_have_world_state_applied,
      carrot_entity_should_notify_about_state_changes,
      carrot_entity_can_be_asked_about_its_position,
      carrot_entity_can_be_bitten,
      carrot_entity_when_eaten_should_be_stopped ].

init_per_testcase(_TestCase, Config) ->
    WorldParameters = #world_parameters{carrots = 1, rabbits = 1, wolves = 1, width = 5, height = 5},

    simulation_event_stream:start_link(),
    simulation_event_stream:attach_handler(common_test_event_handler),

    {ok, Pid} = simulation_entity_carrot:start_link(WorldParameters),

    [ {carrot_entity_pid, Pid}, {world_parameters, WorldParameters} | Config ].

end_per_testcase(_TestCase, Config) ->
    Pid = proplists:get_value(carrot_entity_pid, Config),
    exit(Pid, normal).

carrot_entity_should_have_world_state_applied(Config) ->
    WorldParameters = proplists:get_value(world_parameters, Config),
    Pid = proplists:get_value(carrot_entity_pid, Config),

    {carrot, planted, EntityState} = common_test_event_handler:last_event_of(carrot, planted),

    Pid = EntityState#carrot.pid,
    WorldParameters = EntityState#carrot.world.

carrot_entity_should_notify_about_state_changes(Config) ->
    Pid = proplists:get_value(carrot_entity_pid, Config),

    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),

    4 = length(common_test_event_handler:all_events()).

carrot_entity_can_be_asked_about_its_position(Config) ->
    Pid = proplists:get_value(carrot_entity_pid, Config),

    {carrot, planted, EntityState} = common_test_event_handler:last_event_of(carrot, planted),

    GoodPosition = EntityState#carrot.position,
    BadPosition = #position{x = 0, y = 0},

    true = gen_server:call(Pid, {are_you_at, GoodPosition}),
    false = gen_server:call(Pid, {are_you_at, BadPosition}).

carrot_entity_can_be_bitten(Config) ->
    Pid = proplists:get_value(carrot_entity_pid, Config),

    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),

    {carrot, bite, EntityState} = common_test_event_handler:last_event_of(carrot, bite),
    2 = EntityState#carrot.quantity.

carrot_entity_when_eaten_should_be_stopped(Config) ->
    Pid = proplists:get_value(carrot_entity_pid, Config),

    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),
    gen_server:call(Pid, eat),

    gen_server:call(Pid, eat),

    undefined = process_info(Pid).
