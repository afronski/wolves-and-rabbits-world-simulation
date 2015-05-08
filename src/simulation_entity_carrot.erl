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

handle_call({are_you_at, Position}, _From, State) ->
    {X, Y} = {(State#carrot.position)#position.x, (State#carrot.position)#position.y},

    Result = case {Position#position.x, Position#position.y} of
                 {X, Y} -> true;
                 _      -> false
             end,

    {reply, Result, State};

handle_call(eat, _From, State) ->
    case State#carrot.quantity of
        0 ->
            {stop, normal, {error, carrot_patch_eaten}, State};
        _ ->
            NewQuantity = State#carrot.quantity - ?CARROT_EAT_AMOUNT,
            NewState = State#carrot{quantity = NewQuantity},

            simulation_event_stream:notify(carrot, bite, NewState),

            {reply, {ok, carrot_patch_partially_eaten}, NewState}
    end.

terminate(_, State) ->
    simulation_event_stream:notify(carrot, eaten, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(stop_entity, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
