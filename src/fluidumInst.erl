-module(fluidumInst).

-export([create/2, init/2, get_resource_circuit/1, occupyCircuit/2]).

-spec create(pid(), pid()) -> {ok, pid()}.
create(Root_ConnectorPid, ResTyp_Pid) -> 
	{ok, spawn(?MODULE, init, [Root_ConnectorPid, ResTyp_Pid])}.

-spec init(pid(), pid()) -> no_return().
init(Root_ConnectorPid, ResTyp_Pid) -> 
	{ok, State} = msg:get(ResTyp_Pid, initial_state, {self(), {Root_ConnectorPid, plain_water}}),
	survivor:entry({ fluidInst_created, State }),
	loop(Root_ConnectorPid, State, ResTyp_Pid).

-spec get_resource_circuit(pid()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get_resource_circuit(ResInstPid) ->
	msg:get(ResInstPid, get_resource_circuit). 

-spec loop(pid(), map(), pid()) -> no_return().
loop(Root_ConnectorPid, State, ResTyp_Pid) -> 
	receive
		{get_locations, ReplyFn} ->
			{ok, L_List} = resource_type:get_locations_list(ResTyp_Pid, State), 
			ReplyFn(L_List),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(ResTyp_Pid),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_resource_circuit, ReplyFn} -> 
			{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
			ReplyFn(C), 
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		load_into_circuit ->
			{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
			% C = #{<0.109.0> => processed,<0.111.0> => processed, ... }		
			occupyCircuit(C, self()), 
			loop(Root_ConnectorPid, State, ResTyp_Pid)
	end.


occupyCircuit(C, Occupant) -> 
	survivor:entry({C, Occupant}), 
	occupyC(maps:next(maps:iterator(C)), Occupant).

occupyC({P, _ , Iter }, Occupant) ->
	{ok, [Loc | _ ]} = resource_instance:list_locations(P),
	survivor:entry({P, Occupant, Loc, Iter}),
	survivor:entry({Loc, location:get_Visitor(Loc)}), 
	location:arrival(Loc, Occupant),
	survivor:entry({Loc, location:get_Visitor(Loc)}), 
	occupyC(maps:next(Iter), Occupant);

occupyC( none , _ ) -> ok. 
