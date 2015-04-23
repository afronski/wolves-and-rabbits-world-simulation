-module(simulation_main_supervisor).
-behavior(supervisor).

-include("../include/simulation_world_parameters.hrl").

-export([ start_link/0, init/1 ]).
-export([ populate/1 ]).

start_link() ->
    supervisor:start_link(?MODULE, ?MODULE, []).

init(_State) ->
    %% TODO
    ok.

populate(Parameters) ->
    simulation_carrots_supervisor:plant(Parameters),

    simulation_rabbits_supervisor:breed(Parameters),
    simulation_wolves_supervisor:breed(Parameters),

    done.
