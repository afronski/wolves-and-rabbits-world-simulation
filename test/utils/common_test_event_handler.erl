-module(common_test_event_handler).
-behavior(gen_event).

-export([ all_events/0, last_event_of/2 ]).
-export([ init/1, handle_event/2,
          terminate/2, handle_call/2, handle_info/2, code_change/3 ]).

all_events() ->
    gen_event:call(simulation_event_stream, common_test_event_handler, all_events).

last_event_of(From, Type) ->
    gen_event:call(simulation_event_stream, common_test_event_handler, {last_event_of, From, Type}).

init(_Args) ->
    {ok, []}.

handle_event(Message, State) ->
    {ok, [ Message | State ]}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(all_events, State) ->
    {ok, lists:reverse(State), State};

handle_call({last_event_of, From, Type}, State) ->
    Event = lists:last([ X || X <- lists:reverse(State),
                              element(1, X) =:= From,
                              element(2, X) =:= Type ]),
    {ok, Event, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

