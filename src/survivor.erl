-module(survivor).
-export([start/0, entry/1, init/0]). 

-spec start() -> 'true'.
start() ->
	(whereis(survivor) =:= undefined) orelse unregister(survivor), 
	register(survivor, spawn(?MODULE, init, [])).

-spec entry(_) -> 'true'.
entry(Data)-> 
	ets:insert(logboek, {{now(), self()}, Data}). 

-spec init() -> 'ok'.
init() -> 
	(ets:info(logboek) =:= undefined) orelse ets:delete(logboek),
	ets:new(logboek, [named_table, ordered_set, public]),		
	loop().
	
-spec loop() -> 'ok'.
loop() -> 
	receive
		stop -> ok
	end. 