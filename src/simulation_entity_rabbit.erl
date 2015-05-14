-module(simulation_entity_rabbit).
-behavior(gen_fsm).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1, handle_sync_event/4, terminate/3, handle_event/3, handle_info/3, code_change/4 ]).
-export([ running/2, eating/2, splitting/2 ]).

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
    simulation_event_stream:notify(rabbit, died, State),
    ok.

handle_sync_event(introspection, _From, StateName, State) ->
    {reply, {introspection, StateName, State}, StateName, State};

handle_sync_event({are_you_near, Position}, _From, StateName, State) ->
    if
        abs((State#rabbit.position)#position.x - Position#position.x) =< ?NOTIFY_RATIO,
        abs((State#rabbit.position)#position.y - Position#position.y) =< ?NOTIFY_RATIO  ->
            Reply = true;

        true ->
            Reply = false
    end,
    {reply, Reply, StateName, State, ?FAST_TIMEOUT};

handle_sync_event({are_you_at, Position}, _From, StateName, State) ->
    Reply = case State#rabbit.position of
        Position -> true;
        _        -> false
    end,
    {reply, Reply, StateName, State, ?FAST_TIMEOUT};

handle_sync_event(where_are_you, _From, StateName, State) ->
    Reply = State#rabbit.position,
    {reply, Reply, StateName, State, ?FAST_TIMEOUT};

handle_sync_event(eaten, _From, _StateName, State) ->
    {stop, normal, {ok, dead}, State}.

handle_event({wolf_around, WolfPosition}, _StateName, State) ->
    if
        abs((State#rabbit.position)#position.x - WolfPosition#position.x) =< ?NOTIFY_RATIO,
        abs((State#rabbit.position)#position.y - WolfPosition#position.y) =< ?NOTIFY_RATIO  ->
            NewState = State#rabbit{wolf_around = true};

        true ->
            NewState = State
    end,
    {next_state, running, NewState, ?FAST_TIMEOUT};

handle_event({chasing_you, WolfPosition}, _StateName, State) ->
    NewState = State#rabbit{wolf_around = true},
    broadcast_discovered_wolf(WolfPosition),
    {next_state, running, NewState, ?FAST_TIMEOUT}.

handle_info(stop_entity, _StateName, State) ->
    {stop, normal, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

running({carrot_around, CarrotPosition}, State) when State#rabbit.wolf_around == false ->
    case State#rabbit.target of
        {target, undefined, undefined} ->
            if
                abs((State#rabbit.position)#position.x - CarrotPosition#position.x) =< ?NOTIFY_RATIO,
                abs((State#rabbit.position)#position.y - CarrotPosition#position.y) =< ?NOTIFY_RATIO  ->
                    {X, Y} = {CarrotPosition#position.x, CarrotPosition#position.y},
                    NewState = State#rabbit{target = #target{x = X, y = Y}},

                    simulation_event_stream:notify(rabbit, new_carrot_found, NewState);

                true ->
                    NewState = State
            end;

        {target, _Tx, _Ty} ->
            NewState = State
    end,
    {next_state, running, NewState, ?TIMEOUT};

running({carrot_around, _CarrotPosition}, State) ->
    {next_state, running, State, ?TIMEOUT};

running(timeout, State) when State#rabbit.wolf_around == true ->
    {NewPosition, NewDirection} = simulation_common:next_position(State#rabbit.world, State#rabbit.position, State#rabbit.direction, false),
    State2 = State#rabbit{position = NewPosition, direction = NewDirection},

    simulation_event_stream:notify(rabbit, move, State2),

    if
        State2#rabbit.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT,
        State2#rabbit.time_escaping =< ?MAX_TIME_ESCAPING ->
            NewState = State2#rabbit{time_without_food = State2#rabbit.time_without_food + ?TIMEOUT,
                                     time_escaping = State2#rabbit.time_escaping + ?TIMEOUT},
            {next_state, running, NewState, ?TIMEOUT};

        State2#rabbit.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT ->
            NewState = State2#rabbit{wolf_around = false,
                                     time_without_food = State#rabbit.time_without_food + ?TIMEOUT,
                                     time_escaping = 0},

            {next_state, running, NewState, ?TIMEOUT};

        true ->
            NewState = State2,
            {stop, normal, NewState}
    end;

running(timeout, State) ->
    {NewPosition, NewDirection} = case State#rabbit.target of
        {target, undefined, undefined} ->
            TargetReached = false,
            simulation_common:next_position(State#rabbit.world, State#rabbit.position, State#rabbit.direction, false);

        {target, X, Y} when X == (State#rabbit.position)#position.x,
                            Y == (State#rabbit.position)#position.y ->
            TargetReached = true,
            simulation_common:next_position(State#rabbit.world, State#rabbit.position, State#rabbit.direction, false);

        {target, _X, _Y} ->
            TargetReached = false,
            simulation_common:next_position_and_target(State#rabbit.position, State#rabbit.target)
    end,

    NewState = case TargetReached of
        true  ->
            State#rabbit{position = NewPosition,
                         direction = NewDirection,
                         target = #target{x = undefined, y = undefined},
                         time_without_food = State#rabbit.time_without_food + ?TIMEOUT};

        false ->
            State#rabbit{position = NewPosition,
                         direction = NewDirection,
                         time_without_food = State#rabbit.time_without_food + ?TIMEOUT}
    end,

    simulation_event_stream:notify(rabbit, move, NewState),
    Carrot = get_first_carrot_at(NewPosition),

    case length(Carrot) of
        0 when NewState#rabbit.time_without_food < ?MAX_TIME_WITHOUT_FOOD_RABBIT ->
            {next_state, running, NewState, ?TIMEOUT};

        0 ->
            {stop, normal, NewState};

        1 when NewState#rabbit.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_RABBIT ->
            broadcast_found_carrot(NewPosition),
            NewStateEating = NewState#rabbit{target = #target{x = undefined, y = undefined},
                                             carrot_being_eaten = lists:nth(1, Carrot)},

            simulation_event_stream:notify(rabbit, eating, NewStateEating),
            {next_state, eating, NewStateEating, ?TIMEOUT};

        1 ->
            {stop, normal, NewState}
    end.

eating({carrot_around, _CarrotPosition}, State) ->
    {next_state, eating, State, ?TIMEOUT};

eating(timeout, State) ->
    try gen_server:call(State#rabbit.carrot_being_eaten, eat) of
        {ok, _} ->
            NewState = State#rabbit{carrots_eaten = State#rabbit.carrots_eaten + ?CARROT_EAT_AMOUNT,
                                    time_without_food = 0},

            case NewState#rabbit.carrots_eaten of
                ?CARROTS_TO_SPLIT ->
                    NewStateSplitting = NewState#rabbit{carrots_eaten = 0,
                                                        carrot_being_eaten = undefined},
                    {next_state, splitting, NewStateSplitting, ?FAST_TIMEOUT};

                _ ->
                    simulation_event_stream:notify(rabbit, eating, NewState),
                    {next_state, eating, NewState, ?TIMEOUT}
            end;

        {error, _} ->
            NewState = State#rabbit{carrot_being_eaten = undefined},
            {next_state, running, NewState, ?TIMEOUT}
    catch
        exit:_Reason ->
            NewState = State#rabbit{carrot_being_eaten=undefined},
            {next_state, running, NewState, ?TIMEOUT}
    end.

splitting({carrot_around, _CarrotPosition}, State) ->
    {next_state, splitting, State, ?FAST_TIMEOUT};

splitting(timeout, State) ->
    NewRabbit = { {rabbit, erlang:now()},
                  {simulation_entity_rabbit, start_link, [ {State#rabbit.world, State#rabbit.position} ]},
                  temporary, brutal_kill, worker,
                  [ simulation_entity_rabbit ]},

    supervisor:start_child(simulation_rabbits_supervisor, NewRabbit),
    {next_state, running, State, ?FAST_TIMEOUT}.

%% Internal functions.

broadcast_found_carrot(CarrotPosition) ->
    simulation_event_stream:notify(rabbit, self(), there_is_a_carrot_at, CarrotPosition),
    AllRabbits = supervisor:which_children(simulation_rabbits_supervisor),

    broadcast_found_carrot(CarrotPosition, AllRabbits).

broadcast_found_carrot(_CarrotPosition, []) ->
    done;

broadcast_found_carrot(CarrotPosition, [ {_Id, Rabbit, _Type, _Modules} ]) ->
    if
        Rabbit /= self() ->
            gen_fsm:send_event(Rabbit, {carrot_around, CarrotPosition});

        true ->
            no_notification
    end;

broadcast_found_carrot(CarrotPosition, [ {_Id, Rabbit, _Type, _Modules} | Rest ]) ->
    if
        Rabbit /= self() ->
            gen_fsm:send_event(Rabbit, {carrot_around, CarrotPosition});

        true ->
            no_notification
    end,
    broadcast_found_carrot(CarrotPosition, Rest).

broadcast_discovered_wolf(WolfPosition) ->
    simulation_event_stream:notify(rabbit, self(), there_is_a_wolf_around, WolfPosition),
    AllRabbits = supervisor:which_children(simulation_rabbits_supervisor),

    broadcast_discovered_wolf(WolfPosition, AllRabbits).

broadcast_discovered_wolf(_WolfPosition, []) ->
    done;

broadcast_discovered_wolf(WolfPosition, [ {_Id, Rabbit, _Type, _Modules} ]) ->
    if
        Rabbit /= self() ->
            gen_fsm:send_all_state_event(Rabbit, {wolf_around, WolfPosition});

        true ->
            no_notification
    end;

broadcast_discovered_wolf(WolfPosition, [ {_Id, Rabbit, _Type, _Modules} | Rest ]) ->
    if
        Rabbit /= self() ->
            gen_fsm:send_all_state_event(Rabbit, {wolf_around, WolfPosition});

        true ->
            no_notification
    end,
    broadcast_discovered_wolf(WolfPosition, Rest).

get_first_carrot_at(Position) ->
    AllCarrots = supervisor:which_children(simulation_carrots_supervisor),
    get_first_carrot_at(Position, AllCarrots).

get_first_carrot_at(_Position, []) ->
    [];

get_first_carrot_at(Position, [ {_Id, Carrot, _Type, _Modules} | Rest ]) ->
    try gen_server:call(Carrot, {are_you_at, Position}) of
        true ->
            get_first_carrot_at(Position, Carrot);
        false ->
            get_first_carrot_at(Position, Rest)
    catch
        exit:_Reason -> get_first_carrot_at(Position, Rest)
    end;

get_first_carrot_at(_Position, Carrot) ->
    [ Carrot ].
