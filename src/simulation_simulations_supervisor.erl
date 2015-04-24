-module(simulation_simulations_supervisor).
-behavior(supervisor).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1 ]).
-export([ populate/1, restart/0 ]).

start_link(WorldParameters) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, WorldParameters).

init(WorldParameters) ->
    Args = [ WorldParameters ],

    CarrotsSupervisor = {carrots_supervisor, 
                         {simulation_carrots_supervisor, start_link, Args}, 
                         permanent, brutal_kill, supervisor, 
                         [ simulation_carrots_supervisor ]},

    RabbitsSupervisor = {rabbbits_supervisor, 
                         {simulation_rabbits_supervisor, start_link, Args}, 
                         permanent, brutal_kill, supervisor, 
                         [ simulation_rabbits_supervisor ]},

    WolvesSupervisor = {wolves_supervisor, 
                        {simulation_wolves_supervisor, start_link, Args}, 
                        permanent, brutal_kill, supervisor, 
                        [ simulation_wolves_supervisor ]},

    {ok, {{one_for_all, 1, 60}, 
          [ CarrotsSupervisor, RabbitsSupervisor, WolvesSupervisor ]}}.

populate(Parameters) ->
    simulation_carrots_supervisor:plant(Parameters),

    simulation_rabbits_supervisor:breed(Parameters),
    simulation_wolves_supervisor:breed(Parameters),

    done.

restart() ->
    exit(whereis(?MODULE), kill),
    done.
