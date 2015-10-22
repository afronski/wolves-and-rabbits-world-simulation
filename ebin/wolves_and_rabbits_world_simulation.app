{ application, wolves_and_rabbits_world_simulation,
 [ { description, "World simulation in Erlang with Wolves, Rabbits and Carrots." },
   { vsn, "1.0" },
   { modules, [ simulation,	simulation_carrots_supervisor, simulation_cli_handler, simulation_common,
                simulation_controller, simulation_entity_carrot, simulation_entity_rabbit, simulation_entity_wolf,
                simulation_event_stream, simulation_main_supervisor, simulation_rabbits_supervisor,
                simulation_simulations_supervisor, simulation_wolves_supervisor ] },
   { registered, [] },
   { applications, [ kernel, stdlib, sasl ] },
   { env, [] },
   { mod, { simulation, [] } }
 ]
}.
