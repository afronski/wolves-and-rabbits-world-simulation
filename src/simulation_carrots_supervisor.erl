-module(simulation_carrots_supervisor).
-behavior(supervisor).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1 ]).
-export([ plant/1 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, State#world_parameters.carrots, 1}, []}}.

plant(Parameters) ->
    Carrot = { {carrot, 1}, 
               {simulation_entity_carrot, start_link, [ Parameters ]}, 
               permanent, brutal_kill, worker, 
               [ simulation_carrot ]},

    supervisor:start_child(?MODULE, Carrot),
    done.
