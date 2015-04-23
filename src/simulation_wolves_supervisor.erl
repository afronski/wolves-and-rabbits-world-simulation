-module(simulation_wolves_supervisor).
-behavior(supervisor).

-include("../include/simulation_world_parameters.hrl").

-export([ start_link/1, init/1 ]).
-export([ breed/1 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),
    
    {ok, {{one_for_one, State#world_parameters.wolves, 1}, []}}.

breed(_Parameters) ->
    done.
