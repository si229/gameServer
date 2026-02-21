
%% 点数 1,13
-define(COLOR_SPADE,1).
-define(COLOR_HEART,2).
-define(COLOR_DIAMOND,3).
-define(COLOR_CLUD,4).

-define(DECK,[{Point,Color}||Point<-lists:seq(1,13),Color<-lists:seq(1,13)]).