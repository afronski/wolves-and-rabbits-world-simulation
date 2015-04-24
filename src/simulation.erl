-module(simulation).
-behavior(application).

-include("../include/simulation_records.hrl").

-export([ start/2, stop/1 ]).

read_world_parameters_from_settings() ->
    Carrots = application:get_env(wolves_and_rabbits_world_simulation, carrots, 50),

    Rabbits = application:get_env(wolves_and_rabbits_world_simulation, rabbits, 20),
    Wolves = application:get_env(wolves_and_rabbits_world_simulation, wolves, 10),

    Width = application:get_env(wolves_and_rabbits_world_simulation, width, 20),
    Height = application:get_env(wolves_and_rabbits_world_simulation, height, 20),
    
    #world_parameters{carrots = Carrots, 
                      rabbits = Rabbits, 
                      wolves = Wolves, 
                      width = Width, 
                      height = Height}.

start(_Type, _Args) ->
    Parameters = read_world_parameters_from_settings(),
    simulation_main_supervisor:start_link(Parameters).

stop(_State) -> 
    ok.
