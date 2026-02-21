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
-define(lucky_6_2, 3).
-define(lucky_6_3, 4).
-define(banker_pair, 5).
-define(player_pair, 6).
-define(banker, 7).
-define(player, 8).
-define(tie, 9).


-record(heartbeat_req, {
    id
}).

-record(heartbeat_resp, {
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


-record(reconnect_info, {
    table_id,
    phase_info,
    bet_info
}).


%% 阶段变更信息
-record(phase_change_push,
{
    phase,
    cut_off_time,
    reset_the_road,
    deal_info,
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


