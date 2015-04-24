-module(simulation_entity_carrot).
-behavior(gen_server).

-include("../include/simulation_records.hrl").

-export([ init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2 ]).
-export([ start_link/1 ]).

start_link(WorldParameters) ->
    gen_server:start_link(?MODULE, WorldParameters, []).

init(WorldParameters) ->
    simulation_common:randomize(),

    Width = WorldParameters#world_parameters.width,
    Height = WorldParameters#world_parameters.height,

    {X, Y} =  {random:uniform(Width - 1), random:uniform(Height - 1)},
    State = #carrot{pid = self(), 
                    world = WorldParameters, 
                    position = #position{x = X, y = Y}},

    simulation_event_stream:notify(carrot, planted, State),
    {ok, State}.

%% TODO: Carrot Protocol.
handle_call(_Message, _From, State) ->
    {reply, empty, State}.

terminate(_, State) ->
    simulation_event_stream:notify(carrot, eaten, State),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.
