-module(pipeInst).
-export([create/2, init/2, get_flow_influence/1]).

-spec create(pid(), pid()) -> {ok, pid()}.
create(Host, ResTyp_Pid) -> {ok, spawn(?MODULE, init, [Host, ResTyp_Pid])}.

-spec init(pid(), pid()) -> no_return().
init(Host, ResTyp_Pid) -> 
%	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), []]),	
	{ok, State} = resource_type:get_initial_state(ResTyp_Pid, self(), []),
	survivor:entry({ pipeInst_created, State }),
	loop(Host, State, ResTyp_Pid).

-spec get_flow_influence(pid()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get_flow_influence(PipeInst_Pid) -> 
	msg:get(PipeInst_Pid, get_flow_influence).

-spec loop(pid(), map(), pid()) -> no_return().
loop(Host, State, ResTyp_Pid) -> 
	receive
		{get_connectors, ReplyFn} ->
			{ok,C_List} = resource_type:get_connections_list(ResTyp_Pid, State), 
			ReplyFn(C_List),
			loop(Host, State, ResTyp_Pid);
		{get_locations, ReplyFn} ->
			{ok, List} = resource_type:get_locations_list(ResTyp_Pid, State),
			ReplyFn(List),
			loop(Host, State, ResTyp_Pid);
		{get_type, ReplyFn} -> 
			ReplyFn(ResTyp_Pid),
			loop(Host, State, ResTyp_Pid);
		{get_ops, ReplyFn} ->
			ReplyFn([]),
			loop(Host, State, ResTyp_Pid);
		{get_state, ReplyFn} ->
			ReplyFn(State),
			loop(Host, State, ResTyp_Pid);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = msg:get(ResTyp_Pid, flow_influence, State),
			ReplyFn(InfluenceFn),
			loop(Host, State, ResTyp_Pid)
	end.
	