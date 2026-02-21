-module(game_baccarat).

%% API
-export([dealing/1]).

dealing([PlayerCard1, PlayerCard2, BankerCard1, BankerCard2 | Res]) ->
    PlayerCard = [PlayerCard1, PlayerCard2],
    BankerCard = [BankerCard1, BankerCard2],
    PlayerPoint = get_point(PlayerCard),
    BankerPoint = get_point(BankerCard),
    if PlayerPoint >= 8 orelse BankerPoint >= 8 ->
        {PlayerCard, BankerCard, Res};
        true ->
            {IsPlayerPick, NewPlayerCard, PlayerPickPoint, NewRes} =
                if PlayerPoint >= 6 -> {false, PlayerCard, get_point([PlayerCard2]), Res};
                    true ->
                        [PPickCard | Res1] = Res,
                        {true, PlayerCard ++ [PPickCard], get_point([PPickCard]), Res1}
                end,
            if BankerPoint == 7 ->
                {NewPlayerCard, BankerCard, NewRes};
                BankerPoint == 6 ->
                    if IsPlayerPick == true andalso (PlayerPickPoint == 6 orelse PlayerPickPoint == 7) ->
                        [BPickCard | Res2] = NewRes,
                        {NewPlayerCard, BankerCard ++ [BPickCard], Res2};
                        true ->
                            {NewPlayerCard, BankerCard, NewRes}
                    end;
                BankerPoint == 5 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [0, 1, 2, 3, 8, 9]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;

                BankerPoint == 4 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [0, 1, 8, 9]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;

                BankerPoint == 3 ->
                    case IsPlayerPick andalso lists:member(PlayerPickPoint, [8]) of
                        true ->
                            {NewPlayerCard, BankerCard, NewRes};
                        false ->
                            [BPickCard | Res2] = NewRes,
                            {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
                    end;
                true ->
                    [BPickCard | Res2] = NewRes,
                    {NewPlayerCard, BankerCard ++ [BPickCard], Res2}
            end
    end.


get_point(CardList) ->
    lists:foldl(fun({Point, _}, Acc) ->
        if Point =< 9 -> Acc + Point;
            true -> Point
        end
                end, 0, CardList) rem 10.