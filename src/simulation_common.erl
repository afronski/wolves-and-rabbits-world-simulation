-module(simulation_common).

-export([ randomize/0 ]).

randomize() ->
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({A,B,C}).
