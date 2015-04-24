-module(simulation_event_stream).

-export([ init/0, component_ready/1 ]).

init() ->
    gen_event:start_link({local, ?MODULE}),   
    gen_event:add_handler(?MODULE, simulation_cli_handler, []).

component_ready(Name) ->
    gen_event:notify(?MODULE, {Name, ready}).
