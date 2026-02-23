
-record(user_state, {account, ws_pid, user, timer_ref,play_type,game_type}).

-record(email, {
    address,
    account
}).
-record(phone, {
    num,
    account
}).

-record(t_email, {
    address,
    account,
    code
}).


-record(t_phone, {
    num,
    account
}).


-record(phone_password, {
    num,
    password,
    account
}).

-record(email_password, {
    address,
    password,
    account
}).

-record(user_base_info, {
    account,
    email,
    phone,
    password
}).
-record(user, {
    account,
    type,
    real_money = 0,  %% 真实金额
    bonus_credits %% 体验金
}).