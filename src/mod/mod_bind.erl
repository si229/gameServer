%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2026 下午 8:26
%%%-------------------------------------------------------------------
-module(mod_bind).
-author("si").
-include("user.hrl").
-include("msg.hrl").
%% API
-export([bind_email/2, bind_password/2]).


bind_email(#{<<"msg_id">> := <<"bind_email_req">>
    , <<"email">> := Email, <<"code">> := Code},
    #user_state{account = Account} = User) ->
    case mnesia:dirty_read(t_email, Email) of
        [#t_email{account = Account, code = Code}] ->
            mnesia:dirty_delete(t_email, Email),
            mnesia:dirty_write(#email{address = Email, account = Account}),
            game_user:update_email(Account, Email),
            Msg = game_proto_util:bind_email(?ok),
            {ok, Msg, User};
        _ ->
            Msg = game_proto_util:bind_email(?invalid_verification_code),
            {ok, Msg, User}
    end;
bind_email(#{<<"msg_id">> := <<"bind_email_req">>
    , <<"email">> := Email},
    #user_state{account = Account} = User) ->
    case mnesia:dirty_read(email, Email) of
        [#email{account = Account}] ->
            Msg = game_proto_util:bind_email(?is_already_linked),
            {ok, Msg, User};
        [#email{}] ->
            Msg = game_proto_util:bind_email(?other_already_linked),
            {ok, Msg, User};
        _ ->
            Code = game_mail:gen_code(),
            game_mail:send_code(Email, Code),
            mnesia:dirty_write(#t_email{address = Email, account = Account, code = Code})
    end.

bind_password(#{<<"msg_id">> := <<"bind_password_req">>
    , <<"phone">> := Phone, <<"email">> := Email, <<"password">> := Password},
    #user_state{account = Account} = User) ->
    mnesia:dirty_write(#phone_password{num = Phone, password = Password, account = Account}),
    mnesia:dirty_write(#email_password{address = Email, password = Password, account = Account}),

    game_user:update_email(Account, Email),
    game_user:update_email(Account, Phone),
    Msg = game_proto_util:bind_password(?ok),
    {ok, Msg, User};

bind_password(#{<<"msg_id">> := <<"bind_password_req">>
    , <<"phone">> := Phone, <<"password">> := Password},
    #user_state{account = Account} = User) ->
    mnesia:dirty_write(#phone_password{num = Phone, password = Password, account = Account}),
    game_user:update_email(Account, Phone),
    Msg = game_proto_util:bind_password(?ok),
    {ok, Msg, User};

bind_password(#{<<"msg_id">> := <<"bind_password_req">>
    , <<"email">> := Email, <<"password">> := Password},
    #user_state{account = Account} = User) ->
    mnesia:dirty_write(#email_password{address = Email, password = Password, account = Account}),
    Msg = game_proto_util:bind_password(?ok),
    {ok, Msg, User};

bind_password(#{<<"msg_id">> := <<"bind_password_req">>},
    #user_state{} = User) ->
    Msg = game_proto_util:bind_password(?fail),
    {ok, Msg, User}.