-module(proper_digitalTwin).
-include_lib("proper/include/proper.hrl").

%flow
prop_flow() ->
	survivor:start(),
	Result = ?FORALL(F, integer(1,100), test_flow(F)),
	timer:send_after(1000, survivor, stop),
	Result.

test_flow(F) ->
	{ok,{Pipe_Type, {Pipe_1, Pipe_2, Pipe_3, Pipe_4}, {C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}, {Loc1, Loc2, Loc3, Loc4}, Fluidum_Type,
			Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, {HE_1, HE_2}}} = digitalTwin:start(),
		%Make Lists
		Pipes = tuple_to_list({Pipe_1, Pipe_2, Pipe_3, Pipe_4}),
		Connectors = tuple_to_list({C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}),
		Locations = tuple_to_list({Loc1, Loc2, Loc3, Loc4}),
		HeatExchangers = tuple_to_list({HE_1, HE_2}),
		
	[P|_RestPipes] = Pipes,

	{ok, FlowInf} = pipeInst:get_flow_influence(P),
	CalcPipe = -0.01 * F,
	TestPipe = (FlowInf(F) == CalcPipe),
	

	pumpInst:switch_on(Pump),
	CalcPump = (250 - 5 * F - 2 * F * F),
	{ok, FlowOn} = pumpInst:flow_influence(Pump),
	TestPump = (FlowOn(F) == CalcPump),

	Result = [TestPipe, TestPump],
	case lists:member(false, Result) of
	    true ->
		false
		;
	    false ->
		true
	end.


%HeatExchangers
prop_HE_Flow() ->
	survivor:start(),
	Result = ?FORALL(F, integer(1,100), test_HE_Flow(F)),
	timer:send_after(1000, survivor, stop),
	Result.


test_HE_Flow(F) ->
	{ok,{Pipe_Type, {Pipe_1, Pipe_2, Pipe_3, Pipe_4}, {C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}, {Loc1, Loc2, Loc3, Loc4}, Fluidum_Type,
			Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, {HE_1, HE_2}}} = digitalTwin:start(),
		%Make Lists
		Pipes = tuple_to_list({Pipe_1, Pipe_2, Pipe_3, Pipe_4}),
		Connectors = tuple_to_list({C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}),
		Locations = tuple_to_list({Loc1, Loc2, Loc3, Loc4}),
		HeatExchangers = tuple_to_list({HE_1, HE_2}),
	
	Diff = 1,
	Temp = 25,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(F, Temp),
	Calc = Temp + (Diff/F),	
	Influence == Calc.



prop_HE_Temp() ->
	survivor:start(),
	Result = ?FORALL(T, integer(1,100), test_HE_Temp(T)),
	timer:send_after(1000, survivor, stop),
	Result.


test_HE_Temp(T) ->
	{ok,{Pipe_Type, {Pipe_1, Pipe_2, Pipe_3, Pipe_4}, {C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}, {Loc1, Loc2, Loc3, Loc4}, Fluidum_Type,
			Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, {HE_1, HE_2}}} = digitalTwin:start(),
		%Make Lists
		Pipes = tuple_to_list({Pipe_1, Pipe_2, Pipe_3, Pipe_4}),
		Connectors = tuple_to_list({C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}),
		Locations = tuple_to_list({Loc1, Loc2, Loc3, Loc4}),
		HeatExchangers = tuple_to_list({HE_1, HE_2}),
	
	Flow = 3,
	Diff = 1,

	{ok, {ok, TempInf}} = heatExchangerInst:temp_influence(HeatExInst),
	{ok, Influence} = TempInf(Flow, T),
	Calc = T + (Diff/Flow),	
	Influence == Calc.