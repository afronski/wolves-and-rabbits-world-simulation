-module(simulation_carrots_supervisor).
-behavior(supervisor).

-include("../include/simulation_world_parameters.hrl").

-export([ start_link/1, init/1 ]).
-export([ plant/1 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, State#world_parameters.carrots, 1}, []}}.

plant(_Parameters) ->
    done.
