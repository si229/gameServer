-define(GAME_TYPE_LUCKY, 1). %% 幸运百家乐
-define(GAME_TYPE_CLASSIC, 2). %% 经典百家乐

-record(room_role, {
    account
    , pid
    , bonus_credits = 0
    , real_money = 0
    , bet_info
}).

