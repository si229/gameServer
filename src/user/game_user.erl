%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2026 下午 9:20
%%%-------------------------------------------------------------------
-module(game_user).
-author("si").
-include("common.hrl").
-include("user.hrl").

-export([update_password/2, update_email/2, update_phone/2, load/1, save/1]).

update_password(Account, Password) ->
    case mnesia:dirty_read(user_base_info, Account) of
        [#user_base_info{} = BaseInfo] ->
            mnesia:dirty_write(BaseInfo#user_base_info{password = Password});
        _ ->
            mnesia:dirty_write(#user_base_info{password = Password, account = Account})
    end.

update_email(Account, Email) ->
    case mnesia:dirty_read(user_base_info, Account) of
        [#user_base_info{} = BaseInfo] ->
            mnesia:dirty_write(BaseInfo#user_base_info{email = Email});
        _ ->
            mnesia:dirty_write(#user_base_info{email = Email, account = Account})
    end.

update_phone(Account, Phone) ->
    case mnesia:dirty_read(user_base_info, Account) of
        [#user_base_info{} = BaseInfo] ->
            mnesia:dirty_write(BaseInfo#user_base_info{phone = Phone});
        _ ->
            mnesia:dirty_write(#user_base_info{phone = Phone, account = Account})
    end.

load(Account) ->
    case mnesia:dirty_read(user, Account) of
        [#user{} = User] -> User;
        _ -> #user{type = ?GUEST, account = Account, bonus_credits = 1000}
    end.

save(#user{} = User) ->
    mnesia:dirty_write(User).