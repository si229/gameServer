-define(GUEST,1).
-define(NORMAL,2).

-record(room_role,{
    account,
    pid,
    type,
    chips,
    bet_info
}).

-define(SHOE_SIZE,8). %% 1 shoe 8副牌
-define(SHOE_MIN_NUM,52). %% 小于52要重新换牌