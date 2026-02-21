%%------ 错误码
-define(ok, 0). %%
-define(insufficient_balance, 1). %% 余额不足
-define(bet_exceeds_limit, 2). %% 限红
-define(account_logged_another, 3). %% 异地登录


%%------ 阶段 ----
-define(preparation, 0). %% 准备 此阶段可以选择是否上庄
-define(dealing, 1). %% 发牌
-define(betting, 2). %% 下注
-define(settlement, 3). %% 结算


%%------ 区域 ----
-define(lucky_7, 0).
-define(super_lucky_7, 1).
-define(lucky_6, 2).
-define(super_lucky_6, 3).
-define(banker_pair, 4).
-define(player_pair, 5).
-define(banker, 6).
-define(player, 7).


-record(heartbeat_req,{
    id
}).

-record(heartbeat_resp,{
    id,
    time
}).

-record(login_req, {
    account,
    password
}).

-record(login_resp, {
    account,
    chips,
    reconnect_info
}).


-record(reconnect_info,{
    table_id,
    phase_info,
    bet_info
}).


%% 阶段变更信息
-record(phase_change_push,
{
    phase,
    countdown,
    can_banker = false, %% 准备阶段
    hash_value,%% 发牌阶段推送
    result     %% 结算阶段推送
}).


%% 下注请求
-record(betting_req,
{
    zone,
    amount
}).
%% 下注信息推送
-record(bet_push,
{
    is_self,
    amount,
    code
}).


