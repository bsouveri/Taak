-module(location).
-export([create/2, get_ResInst/1, get_Visitor/1, get_Type/1, arrival/2, departure/1, dispose/1]).
-export([init/2]). 

-spec create(pid(), pid()) -> pid().
create(ResInst_Pid, LocationTyp_Pid) ->
	spawn(?MODULE, init, [ResInst_Pid, LocationTyp_Pid]).
	
-spec init(pid(), pid()) -> no_return().
init(ResInst_Pid, LocationTyp_Pid) ->
	loop(ResInst_Pid, LocationTyp_Pid, vacant).
	
-spec get_ResInst(pid()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get_ResInst(Location_Pid) -> 
	msg:get(Location_Pid, get_ResInst).

-spec get_Visitor(pid()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get_Visitor(Location_Pid) ->
	msg:get(Location_Pid, get_Visitor).
	
-spec get_Type(pid()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get_Type(Location_Pid) ->
	msg:get(Location_Pid, get_Type).
	
-spec arrival(pid(), pid()) -> {'arrived', pid()}.
arrival(Location_Pid, Visitor_Pid) ->
	Location_Pid ! {arrived, Visitor_Pid}. 

-spec departure(pid()) -> 'departed'.
departure(Location_Pid) ->
	Location_Pid ! departed. 
	
-spec dispose(pid()) -> 'remove'.
dispose(Location_Pid) ->
	Location_Pid ! remove. 

-spec loop(pid(), pid(), pid()|'vacant') -> 'stopped'.
loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid) -> 
	receive
		{get_ResInst, ReplyFn} -> 
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Visitor, ReplyFn} -> 
			ReplyFn(Visitor_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Type, ReplyFn} -> 
			ReplyFn(LocationTyp_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{arrived, V_Pid} ->
			loop(ResInst_Pid, LocationTyp_Pid, V_Pid);
		departed -> 
			loop(ResInst_Pid, LocationTyp_Pid, vacant);
		remove -> 
			stopped
	end. 