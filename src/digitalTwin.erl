-module(digitalTwin).
-export([start/0]).
	
-spec start() -> {'ok',{pid(),{pid(),pid(),pid(),pid()},{pid(),pid(),pid(),pid(),pid(),pid(),pid(),pid()},{pid(),pid(),pid(),pid()},pid(),pid(),pid(),pid(),pid(),pid(),pid(),{pid(),pid()}}}.
start() ->
	observer:start(),
	survivor:start(),

	%%Create pipes
	%Pipe type
	{ok, Pipe_Type} = resource_type:create(pipeTyp, []),
	% 4 Pipe instances
	{ok, Pipe_1} = resource_instance:create(pipeInst, [self(), Pipe_Type]),
	{ok, Pipe_2} = resource_instance:create(pipeInst, [self(), Pipe_Type]),
	{ok, Pipe_3} = resource_instance:create(pipeInst, [self(), Pipe_Type]),
	{ok, Pipe_4} = resource_instance:create(pipeInst, [self(), Pipe_Type]),
	
	%%Create Pump
	%Pump Type
	{ok, Pump_Type} = resource_type:create(pumpTyp, []),
	%Pump Instance, attached to Pipe_1
	{ok, Pump} = resource_instance:create(pumpInst, [self(), Pump_Type, Pipe_1, fun(on) -> {ok, on}; (off) -> {ok, off} end]),
	
	%%Create FlowMeter
	%FlowMeter Type
	{ok, FM_Type} = resource_type:create(flowMeterTyp, []),
	%FlowMeter Instance
	{ok, FlowMeter} = resource_instance:create(flowMeterInst, [self(), FM_Type, Pipe_2, fun() -> {ok, real_flow} end]),
	
	%%Create HeatExchangers
	%HeatExchanger Type
	{ok, HE_Type} = resource_type:create(heatExchangerTyp, []),
	% 2 HeatExchanger Instances
	{ok, HE_1} = resource_instance:create(heatExchangerInst, [self(), HE_Type, Pipe_3, #{delta => 1}]),
	{ok, HE_2} = resource_instance:create(heatExchangerInst, [self(), HE_Type, Pipe_4, #{delta => 5}]),
	
	%%Connections
	%Get connectors from pipes
	{ok, [C1_In, C1_Out]} = resource_instance:list_connectors(Pipe_1),
	{ok, [C2_In, C2_Out]} = resource_instance:list_connectors(Pipe_2),
	{ok, [C3_In, C3_Out]} = resource_instance:list_connectors(Pipe_3),
	{ok, [C4_In, C4_Out]} = resource_instance:list_connectors(Pipe_4),
	
	%Make connections
	connector:connect(C1_In, C4_Out),
	connector:connect(C2_In, C1_Out),
	connector:connect(C3_In, C2_Out),
	connector:connect(C4_In, C3_Out),
	
	%%Locations
	{ok, [Loc1]} = resource_instance:list_locations(Pipe_1),
	{ok, [Loc2]} = resource_instance:list_locations(Pipe_2),
	{ok, [Loc3]} = resource_instance:list_locations(Pipe_3),
	{ok, [Loc4]} = resource_instance:list_locations(Pipe_4),
	
	%%Create Fluidum
	%Fluidum Type
	{ok, Fluidum_Type} = resource_type:create(fluidumTyp, []),
	%Fluidum Instance
	{ok, Fluidum} = resource_instance:create(fluidumInst, [C1_In, Fluidum_Type]),
	
	%%Fill Pipes
	location:arrival(Loc1, Fluidum),
	location:arrival(Loc2, Fluidum),
	location:arrival(Loc3, Fluidum),
	location:arrival(Loc4, Fluidum),
	
	{ok,{Pipe_Type, {Pipe_1, Pipe_2, Pipe_3, Pipe_4}, {C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}, {Loc1, Loc2, Loc3, Loc4}, Fluidum_Type,
			Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, {HE_1, HE_2}}}.