-module(simulation_entity_wolf).
-behavior(gen_fsm).

-include("../include/simulation_records.hrl").

-export([ start_link/1, init/1, handle_sync_event/4, terminate/3, handle_event/3, handle_info/3, code_change/4 ]).
-export([ running/2, chasing/2, splitting/2 ]).

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

    State = #wolf{pid = self(), world = WorldParameters,
                  position = #position{x = X, y = Y},
                  direction = simulation_common:direction()},

    simulation_event_stream:notify(wolf, born, State),

    {ok, running, State, ?TIMEOUT}.

terminate(_, _StateName, State) ->
    simulation_event_stream:notify(wolf, died, State),
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

running({chase_rabbit, RabbitPid, NotifierPosition}, State) ->
    if
        abs((State#wolf.position)#position.x - NotifierPosition#position.x) =< ?NOTIFY_RATIO,
        abs((State#wolf.position)#position.y - NotifierPosition#position.y) =< ?NOTIFY_RATIO  ->
            NewState = State#wolf{rabbit_being_chased = RabbitPid},
            simulation_event_stream:notify(wolf, new_target, NewState),
            {next_state, chasing, NewState, ?FAST_TIMEOUT};

        true ->
            {next_state, running, State, ?FAST_TIMEOUT}
    end;

running(timeout, State) ->
    {NewPosition, NewDirection} = simulation_common:next_position(State#wolf.world, State#wolf.position, State#wolf.direction, false),
    NewState = State#wolf{position = NewPosition, direction = NewDirection, time_without_food = State#wolf.time_without_food + ?TIMEOUT},

    simulation_event_stream:notify(wolf, move, NewState),

    RabbitsInPosition = get_rabbits_at(NewPosition),

    case length(RabbitsInPosition) of
        0 when NewState#wolf.time_without_food < ?MAX_TIME_WITHOUT_FOOD_WOLF ->
            RabbitsAround = how_many_rabbits_around(NewPosition),

            case length(RabbitsAround) of
                0 ->
                    {next_state, running, NewState, ?TIMEOUT};

                _ ->
                    simulation_common:randomize(),

                    RabbitChasedIndex = random:uniform(length(RabbitsAround)),
                    RabbitChased = lists:nth(RabbitChasedIndex, RabbitsAround),

                    try gen_fsm:send_all_state_event(RabbitChased, {chasing_you, NewPosition}) of
                        ok ->
                            NewStateChasing = NewState#wolf{rabbit_being_chased = RabbitChased},

                            simulation_event_stream:notify(wolf, self(), chasing_rabbit, RabbitChased),
                            broadcast_chased_rabbit({RabbitChased, NewPosition}),

                            {next_state, chasing, NewStateChasing, ?TIMEOUT}

                    catch
                        exit:_Reason ->
                            {next_state, running, NewState, ?TIMEOUT}
                    end
            end;

        0 ->
            {stop, normal, NewState};

        _ when NewState#wolf.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF ->
            RabbitsEaten = eat_rabbits(RabbitsInPosition),

            NewStateEating = NewState#wolf{target = #target{x = undefined, y = undefined},
                                           rabbits_eaten = State#wolf.rabbits_eaten + RabbitsEaten,
                                           time_without_food = 0},

            case NewStateEating#wolf.rabbits_eaten of
                ?RABBITS_TO_SPLIT ->
                    NewStateSplitting = NewStateEating#wolf{rabbits_eaten = 0},
                    {next_state, splitting, NewStateSplitting, ?FAST_TIMEOUT};

                _ ->
                    {next_state, running, NewStateEating, ?TIMEOUT}
            end;
        _ ->
            {stop, normal, NewState}
    end.

chasing({chase_rabbit, _Rabbit, _Notifier}, State) ->
    {next_state, chasing, State, ?FAST_TIMEOUT};

chasing(timeout, State) ->
    try gen_fsm:sync_send_all_state_event(State#wolf.rabbit_being_chased, where_are_you) of
        {position, X, Y} ->
            State2 = State#wolf{target = #target{x = X, y = Y}},
            {NewPosition, NewDirection} = simulation_common:next_position_and_target(State2#wolf.position, State2#wolf.target),

            simulation_event_stream:notify(wolf, move, State2),

            State3 = State2#wolf{position = NewPosition, direction = NewDirection},

            try gen_fsm:sync_send_all_state_event(State3#wolf.rabbit_being_chased, where_are_you) of
                {position, X, Y} ->
                    if
                        NewPosition == #position{x = X, y = Y},
                        State3#wolf.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF,
                        State3#wolf.time_chasing =< ?MAX_TIME_CHASING ->
                            RabbitsEaten = eat_rabbits([ State3#wolf.rabbit_being_chased ]),

                            State4 = State3#wolf{rabbits_eaten = State3#wolf.rabbits_eaten + RabbitsEaten,
                                                 time_without_food = 0,
                                                 rabbit_being_chased = undefined,
                                                 target = #target{x = undefined, y = undefined}},

                            case State4#wolf.rabbits_eaten of
                                ?RABBITS_TO_SPLIT ->
                                    NewState = State4#wolf{rabbits_eaten = 0, time_without_food = 0},
                                    {next_state, splitting, NewState, ?FAST_TIMEOUT};

                                _ ->
                                    NewState = State4#wolf{time_chasing = 0, time_without_food = 0},
                                    {next_state, running, NewState, ?TIMEOUT}
                            end;

                        State3#wolf.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF,
                        State3#wolf.time_chasing =< ?MAX_TIME_CHASING ->
                            NewState = State3#wolf{time_chasing = State3#wolf.time_chasing + ?TIMEOUT},
                            {next_state, chasing, NewState, ?TIMEOUT};

                        State3#wolf.time_without_food =< ?MAX_TIME_WITHOUT_FOOD_WOLF ->
                            NewState = State3#wolf{time_chasing = 0,
                                                   rabbit_being_chased = undefined,
                                                   target = #target{x = undefined, y = undefined}},

                            {next_state, running, NewState, ?TIMEOUT};

                        true ->
                            NewState = State3,
                            {stop, normal, NewState}
                    end
            catch
                exit:_Reason ->
                    NewState = State3#wolf{time_chasing = 0,
                                           rabbit_being_chased = undefined,
                                           target = #target{x = undefined, y = undefined}},
                    {next_state, running, NewState, ?TIMEOUT}
            end
    catch
        exit:_Reason ->
            NewState = State#wolf{time_chasing = 0,
                                  rabbit_being_chased = undefined,
                                  target = #target{x = undefined, y = undefined}},
            {next_state, running, NewState, ?TIMEOUT}
    end.

splitting({chase_rabbit, _RabbitPid, _NotifierPosition}, State) ->
    {next_state, splitting, State, ?FAST_TIMEOUT};

splitting(timeout, State) ->
    NewWolf = { {wolf, erlang:now()},
                {simulation_entity_wolf, start_link, [ {State#wolf.world, State#wolf.position} ]},
                temporary, brutal_kill, worker,
                [ simulation_entity_wolf ]},

    supervisor:start_child(simulation_wolves_supervisor, NewWolf),
    {next_state, running, State, ?FAST_TIMEOUT}.

%% Internal functions.

broadcast_chased_rabbit({Rabbit, NotifierPosition}) ->
    simulation_event_stream:notify(wolf, self(), rabbit_found, Rabbit),

    AllWolves = supervisor:which_children(simulation_wolves_supervisor),
    broadcast_chased_rabbit({Rabbit, NotifierPosition}, AllWolves).

broadcast_chased_rabbit({_Rabbit, _NotifierPosition}, []) ->
    done;

broadcast_chased_rabbit({Rabbit, NotifierPosition}, [{_Id, Wolf, _Type, _Modules}]) ->
    if
        Wolf /= self() ->
            gen_fsm:send_event(Wolf, {chase_rabbit, Rabbit, NotifierPosition});

        true ->
            no_notification
    end;

broadcast_chased_rabbit({Rabbit, NotifierPosition}, [{_Id, Wolf, _Type, _Modules} | Rest]) ->
    if
        Wolf /= self() ->
            gen_fsm:send_event(Wolf, {chase_rabbit, Rabbit, NotifierPosition});

        true ->
            no_notification
    end,
    broadcast_chased_rabbit({Rabbit, NotifierPosition}, Rest).

get_rabbits_at(Position) ->
    AllRabbits = supervisor:which_children(simulation_rabbits_supervisor),
    get_rabbits_at(Position, AllRabbits, []).

get_rabbits_at(_Position, [], []) ->
    [];

get_rabbits_at(_Position, [], RabbitsInPosition) ->
    RabbitsInPosition;

get_rabbits_at(Position, [{_Id, Rabbit, _Type, _Modules} | Rest], RabbitsInPosition) ->
    try gen_fsm:sync_send_all_state_event(Rabbit, {are_you_at, Position}) of
        true ->
            get_rabbits_at(Position, Rest, [Rabbit | RabbitsInPosition]);

        false ->
            get_rabbits_at(Position, Rest, RabbitsInPosition)
    catch
        exit:_Reason ->
            get_rabbits_at(Position, Rest, RabbitsInPosition)
    end.

how_many_rabbits_around(Position) ->
    AllRabbits = supervisor:which_children(simulation_rabbits_supervisor),
    how_many_rabbits_around(Position, AllRabbits, []).

how_many_rabbits_around(_Position, [], []) ->
    [];

how_many_rabbits_around(_Position, [], RabbitsAround) ->
    RabbitsAround;

how_many_rabbits_around(Position, [{_Id, Rabbit, _Type, _Modules} | Rest], RabbitsAround) ->
    try gen_fsm:sync_send_all_state_event(Rabbit, {are_you_near, Position}) of
        true ->
            how_many_rabbits_around(Position, Rest, [Rabbit | RabbitsAround]);

        false ->
            how_many_rabbits_around(Position, Rest, RabbitsAround)
    catch
        exit:_Reason ->
            how_many_rabbits_around(Position, Rest, RabbitsAround)
    end.

eat_rabbits([]) -> 0;
eat_rabbits(Rabbits) -> eat_rabbits(Rabbits, 0).

eat_rabbits([], RabbitsEaten) ->
    RabbitsEaten;

eat_rabbits([Rabbit | Rest], RabbitsEaten) ->
    try gen_fsm:sync_send_all_state_event(Rabbit, eaten) of
        {ok, _} ->
            RabbitsEatenNow = RabbitsEaten + 1,
            simulation_event_stream:notify(wolf, self(), rabbit_eaten, Rabbit),
            eat_rabbits(Rest, RabbitsEatenNow)
    catch
        exit:_Reason ->
            eat_rabbits(Rest, RabbitsEaten)
    end.
