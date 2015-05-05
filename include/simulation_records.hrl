-define(TIMEOUT, 1000).
-define(FAST_TIMEOUT, 100).

-define(NOTIFY_RATIO, 3).

-define(RABBITS_TO_SPLIT, 3).
-define(CARROTS_TO_SPLIT, 6).

-define(MAX_TIME_ESCAPING, 5000).
-define(MAX_TIME_CHASING, 5000).

-define(MAX_TIME_WITHOUT_FOOD_RABBIT, 20000).
-define(MAX_TIME_WITHOUT_FOOD_WOLF, 30000).

-define(CARROT_PATCH_AMOUNT, 4).
-define(CARROT_EAT_AMOUNT, 1).

-record(target, {x, y}).
-record(position, {x, y}).

-record(carrot, {pid, world, position, quantity = ?CARROT_PATCH_AMOUNT}).

-record(rabbit, {pid, position, direction, world, target = #target{},
                 carrots_eaten = 0, time_without_food = 0, time_escaping = 0,
                 carrot_being_eaten = undefined, wolf_around = false}).

-record(wolf, {pid, position, direction, world, target = #target{},
               rabbits_eaten = 0, time_without_food = 0, time_chasing = 0,
               rabbit_being_chased = undefined}).

-record(world_parameters, {carrots, rabbits, wolves, width, height}).
