-module(simulation_main_supervisor).
-behavior(supervisor).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1 ]).
-export([ populate/1, kill_population/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(WorldParameters) ->
    Args = [ WorldParameters ],

    EventStream = {simulation_event_stream,
                   {simulation_event_stream, start_link, []},
                   permanent, 1000, worker,
                   [ simulation_event_stream ]},

    SimulationController = {simulation_controller,
                            {simulation_controller, start_link, Args},
                            permanent, 1000, worker,
                            [ simulation_controller ]},

    SimulationsSupervisor = {simulations_supervisor,
                             {simulation_simulations_supervisor, start_link, Args},
                             permanent, brutal_kill, supervisor,
                             [ simulation_simulations_supervisor ]},

    {ok, {{one_for_one, 1, 60},
          [ EventStream, SimulationController, SimulationsSupervisor ]}}.

populate(Parameters) ->
    simulation_simulations_supervisor:populate(Parameters).

kill_population() ->
    simulation_simulations_supervisor:restart().
