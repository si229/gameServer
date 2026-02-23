%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 11月 2025 下午 9:34
%%%-------------------------------------------------------------------
-module(mod_user).
-include("game_net.hrl").
-include("msg.hrl").
-include("user.hrl").
-include("common.hrl").
-include("login.hrl").
%% API
-export([login/2]).


login(#{<<"msg_id">> := <<"login_req">>, <<"option">> := Option} = Msg, #game_net_state{} = State) ->
    case check_login(Option, Msg) of
        {ok, Account} ->
            login_do(Account, Option, State);
        ok ->
            {ok, State};
        _ ->
            Msg = game_proto_util:login_resp(?fail),
            {ok, Msg, State}
    end;

login(Msg, State) ->
    ?INFO("--- ~p~n", [{Msg, State}]),
    {ok, State}.


check_login(?login_with_guest, _Msg) -> {ok, game_server_id:next_id()};
check_login(?login_with_email_password, #{<<"email">> := Email, <<"password">> := Password}) ->
    case mnesia:dirty_read(email_password, Email) of
        [#email_password{account = Account, password = Password}] ->
            {ok, Account};
        _ ->
            Msg = game_proto_util:login_resp(?invalid_password),
            {error, Msg}
    end;
check_login(?login_with_phone_password, #{<<"phone">> := Phone, <<"password">> := Password}) ->
    case mnesia:dirty_read(phone_password, Phone) of
        [#phone_password{account = Account, password = Password}] ->
            {ok, Account};
        _ ->
            Msg = game_proto_util:login_resp(?invalid_password),
            {error, Msg}
    end;

check_login(?login_with_email_code, #{<<"email">> := Email, <<"code">> := Code}) ->
    case mnesia:dirty_read(login_email, Email) of
        [#login_email{code = Code, account = Account}] ->
            mnesia:dirty_delete(login_email, Email),
            {ok, Account};
        _ ->
            Msg = game_proto_util:login_resp(?invalid_verification_code),
            {error, Msg}
    end;

check_login(?login_with_email_code, #{<<"email">> := Email}) ->
    case mnesia:dirty_read(email, Email) of
        [#email{account = Account}] ->
            Code = game_mail:gen_code(),
            case game_mail:send_code(Email, Code) of
                {ok, _} ->
                    mnesia:dirty_write(#login_email{account = Account, code = Code, email = Email});
                _ ->
                    skip
            end,
            ok;
        _ ->
            Msg = game_proto_util:login_resp(?invalid_email),
            {error, Msg}
    end.


login_do(Account, ?login_with_guest, State) ->
%%    -record(login_resp, {
%%        account,
%%        bonus_credits,  %% 体验金额
%%        real_money,     %% 真实金额
%%        reconnect_info
%%    }).
    case supervisor:start_child(user_sup, [#user{account = Account, type = ?GUEST}, self()]) of
        {ok, Pid} ->
            Msg = game_proto_util:login_resp(Account, 1000, none),
            {ok, Msg, State#game_net_state{account = Account, pid = Pid}};
        R ->
            ?WARNING("# ~p", [R]),
            {ok, State}
    end;

login_do(Account, _, State) ->
    case mnesia:dirty_read(user, Account) of
        [#user{} = User] ->
            case supervisor:start_child(user_sup, [User, self()]) of
                {ok, Pid} ->
                    Msg = game_proto_util:login_resp(Account, 1000, none),
                    {ok, Msg, State#game_net_state{account = Account, pid = Pid}};
                R ->
                    ?WARNING("# ~p", [R]),
                    {ok, State}
            end;
        _ ->
            ?WARNING("# ~p", [Account]),
            {ok, State}
    end.


