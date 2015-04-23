-module(simulation_event_stream).

-export([ init/0, component_ready/1 ]).

init() ->
    gen_event:start_link({local, world_event_stream}),   
    gen_event:add_handler(world_event_stream, simulation_cli_handler, []).

component_ready(Name) ->
    gen_event:notify(world_event_stream, {Name, ready}).
