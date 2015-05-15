# wolves-and-rabbits-world-simulation

## Description

This is an implementation of an assignment described by Francessco Cessarini [here](http://www.youtube.com/watch?v=d5G3P2iosmA).

A textual description:

> We had to implement a simulated world inhabited by carrots, rabbits, and wolves. Rabbits would roam this world eating carrots that grew in random patches. When they had eaten enough carrots, the rabbits would get fat and split in two. Wolves ran around eating up the rabbits; if they managed to catch and eat enough rabbits, they would also get fat and split. Rabbits and wolves within a certain distance of each other would broadcast information on food and predators. If a rabbit found a carrot patch, other rabbits would quickly join him. If a wolf found a rabbit, the pack would start chasing it.

## How to build and run it?

1. Grab `rebar3` binaries from [here](https://github.com/rebar/rebar3).
2. `rebar3 compile`
3. `erl -pa _build/default/lib/wolves_and_rabbits_world_simulation/ebin`
4. Invoke following commands in the *Erlang* shell:
  - `application:start(sasl).`
  - `application:start(wolves_and_rabbits_world_simulation).`
  - `simulation_controller:start_simulation().`
5. If you would like to run tests, invoke command: `rebar3 ct`.

## Architecture

Each *carrot*, *rabbit* and *wolf* on the board is represented as a separate process inside *Erlang VM*. All of them will be spawned underneath one of the *entities supervisor*. Process of starting and stopping the simulation can be observed below:

![Starting and stopping the simulation](/docs/simulation.gif)

Massive amount of processes has been spawned during the simulation start. Amount can be defined in the application `env` configuration, but also sensible defaults are setup inside the code. Besides the entities, we can distinguish following modules in the application's architecture:

- `simulation_main_supervisor` - Root of supervision hierarchy.
  - `simulation_controller` - Server which is responsible for handling simulation state and sending commands to the simulation subsystems from the outside.
  - `simulation_event_stream` - Events notifier, which is responsible for notifying about all state changes during the simulation.
  - `simulation_simulations_supervisor` - Supervisor for rest of supervisors responsible for simulation entities.
    - `simulation_carrots_supervisor` - Supervisor responsible for all carrots on the board.
    - `simulation_rabbits_supervisor` - Supervisor responsible for all rabbits on the board.
    - `simulation_wolves_supervisor` - Supervisor responsible for all wolves on the board.

Here you can see how the structure of main *supervision tree* for this application looks like:

![Main supervision tree for wolves_and_rabbits_world_simulation application](/docs/supervision-tree.png)

First two anonymous processes are related with the *Erlang* `application` behavior.

Regarding state changes which are occurring during the simulation - each movement, death, birth, bite etc. is reported as a separate event. By default *Erlang* application connects only the *CLI* handler, which prints all events to the console. But nothing stops you from adding your custom event handler, in order to react for the changes in your own way - `simulation_event_stream:attach_handler/1` is designed to handle that requirement. Prepared *event handler* passed to aforementioned function must implement the `gen_event` behavior. On each `handle_event/2` it will receive as a first argument *tuple* with three elements - first is an information which entity send an event (`carrot`, `rabbit` or `wolf`), second is an actual event name (e.g. `eaten`) and third will be an internal state of the entity (represented as a *record*).

Example event:

```erlang
{carrot,eaten,{carrot,<0.297.0>,{world_parameters,50,20,10,20,20},{position,11,17},4}}
```
