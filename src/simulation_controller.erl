-module(simulation_controller).
-behavor(gen_server).

-include("../include/simulation_records.hrl").

-export([ init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2 ]).
-export([ start_link/1, start_simulation/0, stop_simulation/0, get_board_parameters/0 ]).

start_link(WorldParameters) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, WorldParameters, []).

start_simulation() ->    
    gen_server:call(?MODULE, start_simulation).

stop_simulation() ->
    gen_server:call(?MODULE, stop_simulation).

get_board_parameters() ->
    gen_server:call(?MODULE, get_board_parameters).

init(WorldParameters) ->
    simulation_event_stream:component_ready(?MODULE),
    {ok, {stopped, WorldParameters}}.

%% Starting simulation.

handle_call(start_simulation, _From, {stopped, WorldParameters}) ->
    simulation_main_supervisor:populate(WorldParameters),
    {reply, started, {started, WorldParameters}};

handle_call(start_simulation, _From, {started, _WorldParameters} = State) ->
    {reply, already_started, State};

%% Stopping simulation.

handle_call(stop_simulation, _From, {started, State}) ->
    simulation_main_supervisor:kill_population(),
    {reply, stopped, {stopped, State}};

handle_call(stop_simulation, _From, {stopped, _WorldParameters} = State) ->
    {reply, already_stopped, State};

%% Getting board parameters and default `handle_call/3` handler.

handle_call(get_board_parameters, _From, {_StateName, WorldParameters} = State) ->
    Width = WorldParameters#world_parameters.width,
    Height = WorldParameters#world_parameters.height,

    {reply, { {width, Width}, {height, Height} }, State}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

handle_cast(_Request, State) -> 
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.
