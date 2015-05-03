-module(simulation_wolves_supervisor).
-behavior(supervisor).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1 ]).
-export([ breed/1, kill_children/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(State) ->
    simulation_event_stream:component_ready(?MODULE),

    {ok, {{one_for_one, State#world_parameters.wolves, 1}, []}}.

breed(Parameters) ->
    breedWolves(0, Parameters#world_parameters.wolves, Parameters).

breedWolves(Created, Total, WorldParameters) when Created < Total ->
    Wolf = { {wolf, Created + 1},
             {simulation_entity_wolf, start_link, [ {WorldParameters, random} ]},
             temporary, brutal_kill, worker,
             [ simulation_entity_wolf ]},

    supervisor:start_child(?MODULE, Wolf),
    breedWolves(Created + 1, Total, WorldParameters);

breedWolves(_Created, _Total, _WorldParameters) ->
    done.

kill_children() ->
    simulation_common:stop_children(?MODULE).
