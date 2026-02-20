

-define(INFO(Msg),lager:info(Msg)).
-define(INFO(Format,Msg),lager:info(Format,Msg)).

-define(WARNING(Msg),lager:warning(Msg)).
-define(WARNING(Format,Msg),lager:warning(Format,Msg)).

-define(ERROR(Msg),lager:error(Msg)).
-define(ERROR(Format,Msg),lager:error(Format,Msg)).

-define(MILLI_TIMESTAMP,erlang:system_time(1000)).