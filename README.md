# wolves-and-rabbits-world-simulation

## Description

This is an implementation of assignment described by Francessco Cessarini [here](http://www.youtube.com/watch?v=d5G3P2iosmA):

The most detailed description:

> We had to implement a simulated world inhabited by carrots, rabbits, and wolves. Rabbits would roam this world eating carrots that grew in random patches. When they had eaten enough carrots, the rabbits would get fat and split in two. Wolves ran around eating up the rabbits; if they managed to catch and eat enough rabbits, they would also get fat and split. Rabbits and wolves within a certain distance of each other would broadcast information on food and predators. If a rabbit found a carrot patch, other rabbits would quickly join him. If a wolf found a rabbit, the pack would start chasing it.

## How to build and run it?

1. `make clean`
2. `make`

## Architecture

*TODO*

## TODO

### Documentation

- *TODO*: Screen from *Observer* tool (*supervision tree*).
- *TODO*: Small description of the notification mechanism and FSMs.
- *TODO*: `GIF` from *Erlubi* visualization connected to it.

### Internals

- *TODO*: Add new level of supervisors - *communication_supervisor* and *simulation_supervisor*.
- *TODO*: All carrots, rabbits and wolves will go underneath simulation supervisor, event streams and simulation_controller (`gen_server`) will go underneath communication supervisor.
- *TODO*: Add a timeout, not a brutal kill for supervisors.
- *TODO*: Add simulation controller with start and stop publicly visible API.
- *TODO*: Add a public API for connecting gen_event handler to the *simulation_event_stream*.
- *TODO*: Carrot FSM and logic.
- *TODO*: Rabbit FSM and logic.
- *TODO*: Wolf FSM and logic.
- *TODO*: Add `rebar` to it, in order to integrate with `mix` afterwards.
