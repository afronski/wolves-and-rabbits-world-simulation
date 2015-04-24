-define(CARROT_PATCH_AMOUNT, 4).
-define(CARROT_EAT_AMOUNT, 1).

-record(position, {x, y}).

-record(carrot, {pid, world, position, quantity = ?CARROT_PATCH_AMOUNT}).

-record(world_parameters, {carrots, rabbits, wolves, width, height}).
