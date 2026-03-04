-define(GAME_TYPE_BACCARAT_LUCKY, 1). %% 幸运百家乐
-define(GAME_TYPE_BACCARAT_CLASSIC, 2). %% 经典百家乐
-define(GAME_TYPE_AMERICAN_ROULETTE,3).
-define(GAME_TYPE_FRENCH_ROULETTE,4).


-record(room_state, {id, play_type, guest_role_list = [], game_type
    , normal_role_list = [], loop_timer_ref, phase_state
    , deck = [], player_cards, banker_cards, hash_value
    , deal_info, bet_info = [], room_mod
}).


-record(room_role, {
    account
    , pid
    , bonus_credits = 0
    , real_money = 0
    , bet_info = []
}).

