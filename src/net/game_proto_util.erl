%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 2月 2026 上午 11:03
%%%-------------------------------------------------------------------
-module(game_proto_util).
-author("si").

%% API
-export([login_resp/1,login_resp/3, phase_change_push/3, phase_change_push/4, phase_change_push/5]).

-export([
    bind_email/1,
    bind_phone/1,
    bind_password/1,
    enter_room/1,
    leave_room/1
]).

login_resp(Account, Chips, ReconnectInfo) ->
    jsx:encode(#{msg_id => login_resp, account => Account, chips => Chips, reconnect_info => ReconnectInfo}).

login_resp(Code) ->
    jsx:encode(#{msg_id => login_resp, code=>Code}).

bind_email(Code) ->
    jsx:encode(#{msg_id => bind_email_resp, code => Code}).


bind_phone(Code) ->
    jsx:encode(#{msg_id => bind_phone_resp, code => Code}).


bind_password(Code) ->
    jsx:encode(#{msg_id => bind_password_resp, code => Code}).

enter_room(Code) ->
    jsx:encode(#{msg_id => enter_room_resp, code => Code}).

leave_room(Code) ->
    jsx:encode(#{msg_id => leave_room_resp, code => Code}).

%% 阶段变更信息
phase_change_push(Phase, CutOffTime, ResetTheRoad) ->
    jsx:encode(#{msg_id => phase_change_push, phase => Phase
        , cut_off_time => CutOffTime
        , reset_the_road => ResetTheRoad
        , result => none
        , deal_info => none
    }).
phase_change_push(Phase, CutOffTime, ResetTheRoad, Result) ->
    jsx:encode(#{msg_id => phase_change_push, phase => Phase
        , cut_off_time => CutOffTime
        , reset_the_road => ResetTheRoad
        , result => Result
    }).

phase_change_push(Phase, CutOffTime, ResetTheRoad, DealInfo, Result) ->
    jsx:encode(#{msg_id => phase_change_push, phase => Phase
        , cut_off_time => CutOffTime
        , deal_info => DealInfo
        , reset_the_road => ResetTheRoad
        , result => Result
    }).