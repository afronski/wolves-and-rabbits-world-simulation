-module(simulation_wolves_supervisor).
-behavior(supervisor).

-include("../include/simulation_world_parameters.hrl").

-export([ start_link/0, init/1 ]).
-export([ breed/1 ]).

start_link() ->
    supervisor:start_link(?MODULE, ?MODULE, []).

init(_State) ->
    %% TODO
    ok.

breed(_Parameters) ->
    done.
