-module(simulation).
-behavior(application).

-include("../include/simulation_world_parameters.hrl").

-export([ start/2, stop/1 ]).

read_world_parameters_from_settings() ->
    carrots = application:get_env(wolves_and_rabbits_world_simulation, carrots, 50),

    rabbits = application:get_env(wolves_and_rabbits_world_simulation, rabbits, 20),
    wolves = application:get_env(wolves_and_rabbits_world_simulation, wolves, 10),

    width = application:get_env(wolves_and_rabbits_world_simulation, width, 20),
    height = application:get_env(wolves_and_rabbits_world_simulation, height, 20),
    
    #world_parameters{carrots = carrots, 
                      rabbits = rabbits, 
                      wolves = wolves, 
                      width = width, 
                      height = height}.

start(_Type, _Args) ->
    {ok, Pid} = simulation_main_supervisor:start_link(),

    Parameters = read_world_parameters_from_settings(),
    simulation_main_supervisor:populate(Parameters),

    {ok, Pid}.

stop(_State) -> 
    ok.
