-module(simulation_carrots_supervisor).
-behavior(supervisor).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1 ]).
-export([ plant/1, kill_children/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, State#world_parameters.carrots, 1}, []}}.

plant(Parameters) ->
    plantCarrots(0, Parameters#world_parameters.carrots, Parameters).

plantCarrots(Created, Total, WorldParameters) when Created < Total ->
    Carrot = { {carrot, Created + 1},
               {simulation_entity_carrot, start_link, [ WorldParameters ]},
               temporary, brutal_kill, worker,
               [ simulation_entity_carrot ]},

    supervisor:start_child(?MODULE, Carrot),
    plantCarrots(Created + 1, Total, WorldParameters);

plantCarrots(_Created, _Total, _WorldParameters) ->
    done.

kill_children() ->
    simulation_common:stop_children(?MODULE).
