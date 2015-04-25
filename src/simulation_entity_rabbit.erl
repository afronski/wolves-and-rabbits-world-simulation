-module(simulation_entity_rabbit).
-behavior(gen_fsm).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1, handle_sync_event/4, terminate/3, handle_event/3, handle_info/3, code_change/4 ]).
-export([ running/2 ]).

start_link(InitialState) ->
    gen_fsm:start_link(?MODULE, InitialState, []).

init({WorldParameters, Position}) ->
    case Position of
        random ->
            simulation_common:randomize(),

            Width = WorldParameters#world_parameters.width,
            Height = WorldParameters#world_parameters.height,

            {X, Y} = {random:uniform(Width - 1), random:uniform(Height - 1)};

        {position, X, Y} ->
            {X, Y}
    end,

    State = #rabbit{pid = self(), world = WorldParameters,
                    position = #position{x = X, y = Y},
                    direction = simulation_common:direction()},

    simulation_event_stream:notify(rabbit, born, State),

    {ok, running, State, ?TIMEOUT}.

terminate(_, _StateName, State) ->
    simulation_event_stream:notify(rabbit, die, State),
    ok.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info(stop_entity, _StateName, State) ->
    {stop, normal, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

running(_Message, State) ->
    {next_state, running, State, ?TIMEOUT}.
