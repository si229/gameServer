-record(game_net_state,{
    account,
    pid,
    last_heartbeat,
    timeout_times = 0,
    ip
}).