%%%-------------------------------------------------------------------
%%% @author si
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 2月 2026 下午 5:45
%%%-------------------------------------------------------------------
-module(card).
-author("si").

%% API
-export([shuffle/1]).

shuffle(Deck) ->
    NewDeck = lists:keysort(2, [{Card, rand:uniform()} || Card <- Deck]),
    [Card || {Card, _} <- NewDeck].
