-define(GUEST,1).
-define(NORMAL,2).

-record(room_role,{
    account,
    pid,
    type,
    chips,
    bet_info
}).

