-module(eunit_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).


digitalTwin_test() ->
	{foreach, fun setup/0, fun cleanup/1, [
		fun testDigitalTwin/1,
		fun testPipes/1,
		fun testFlowMeter/1,
		fun testPump/1,
		fun testHeatExchangers/1
		]}.
		
setup() ->
	NoPipes = 4,
    NoHeatExchangers = 2,
	{ok,{Pipe_Type, {Pipe_1, Pipe_2, Pipe_3, Pipe_4}, {C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}, {Loc1, Loc2, Loc3, Loc4}, Fluidum_Type,
			Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, {HE_1, HE_2}}} = digitalTwin:start(),
		Pipes = tuple_to_list({Pipe_1, Pipe_2, Pipe_3, Pipe_4}),
		Connectors = tuple_to_list({C1_In, C1_Out, C2_In, C2_Out, C3_In, C3_Out, C4_In, C4_Out}),
		Locations = tuple_to_list({Loc1, Loc2, Loc3, Loc4}),
		HeatExchangers = tuple_to_list({HE_1, HE_2}),
		
	{Pipe_Type, Pipes, Connectors, Locations, Fluidum_Type, Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, HeatExchangers, NoPipes, NoHeatExchangers}.
			
cleanup(_) ->
	digitalTwin:stop().
	
%% Checks whether single types & instances are alive
testDigitalTwin({Pipe_Type, Pipes, Connectors, Locations, Fluidum_Type, Fluidum, Pump_Type, Pump, FM_Type, FlowMeter, HE_Type, HeatExchangers, NoPipes, NoHeatExchangers}) ->
	
	[
        %% Types
        ?_assert(alive(Pipe_Type)),
        ?_assert(alive(Fluidum_Type)),
        ?_assert(alive(Pump_Type)),
        ?_assert(alive(FM_Type)),
        ?_assert(alive(HE_Type)),
        %% Test Instances
        ?_assert(alive(Fluidum)),
        ?_assert(alive(FlowMeter))
    ].
	
testPipes({_, Pipes, Connectors, Locations, _, FluidumInst, _, _, _, _, _, _, NoPipes, _}) ->
    testPipes(Pipes, Connectors, Locations, NoPipes, [], FluidumInst)
.

testPipes(Pipes, Connectors, Locations, NoPipes, Tests, FluidumInst) when NoPipes > 0 ->
    [Pipe | OtherPipes] = Pipes,
    [ConIn, ConOut | OtherConnectors] = Connectors,
    [Location | OtherLocations] = Locations,

    Flow = 5,
    {ok, FlowInfluenceFn} = pipeInst:get_flow_influence(Pipe),
    
    {ok, ConInInst} = connector:get_ResInst(ConIn),
    {ok, ConOutInst} = connector:get_ResInst(ConOut),
    {ok, LocationInst} = location:get_ResInst(Location),
    {ok, ConInTyp} = connector:get_type(ConIn),
    {ok, ConOutTyp} = connector:get_type(ConOut),
    {ok, Visitor} = location:get_Visitor(Location),

    Test = [
        %% Test if Pipe, Connectors and Location are alive
        ?_assert(alive(Pipe)),
        ?_assert(alive(ConIn)),
        ?_assert(alive(ConOut)),
        ?_assert(alive(Location)),
        %% Test if Flow influence is correctly calculated
        ?_assertEqual(-0.01 * Flow, FlowInfluenceFn(Flow)),
        %% Test if instances and types are correct
        ?_assertEqual(ConInInst, Pipe),
        ?_assertEqual(ConOutInst, Pipe),
        ?_assertEqual(LocationInst, Pipe),
        ?_assertEqual(ConInTyp, simplePipe),
        ?_assertEqual(ConOutTyp, simplePipe),
        %% Test if Visitor is correct
        ?_assertEqual(Visitor, FluidumInst)
    ],

testPipes(OtherPipes, OtherConnectors, OtherLocations, NoPipes - 1, Tests ++ [Test], FluidumInst);

testPipes([], [], [], 0, Tests, _) ->
	Tests.

testFlowMeter({_, _, _, _, _, _, _, _, _, FlowMeter, _, _, _, _, _}) ->
    Test = [
        %% Test instance pid's
        ?_assert(alive(FlowMeter))
    ],

    {ok, Measurement} = flowMeterInst:measure_flow(FlowMeter),
    Tests = Test ++ [
        ?_assertEqual(Measurement, {ok, real_flow})
    ],

    Tests
.

testPump({_, _, _, _, _, _, _, Pump, _, _, _, _, _, _, _}) ->
    Tests = [
        ?_assert(alive(Pump)),
        ?_assertEqual({ok, off}, pumpInst:is_on(Pump))
    ],
    Tests.
		
testHeatExchangers({_, _, _, _, _, _, _, _, _, _, _, HeatExchangers, _, _, NoHeatExchangers}) ->
    testHeatExchangers(HeatExchangers, NoHeatExchangers, [])
.

testHeatExchangers([HeatExchanger | RemHeatExchangers], NoHeatExchangers, Tests) when NoHeatExchangers > 0 ->
    {ok, {ok, TempInfl}} = heatExchangerInst:temp_influence(HeatExchanger),
    {ok, Influence} = TempInfl(5, 25), %% (Flow, Temperature)
    ExpectedInfluence = 25 + (1 / 5), %% Temperature + (Difference / Flow)

    Test = [
        ?_assert(alive(HeatExchanger)),
        ?_assertEqual(ExpectedInfluence, Influence)
    ],

    testHeatExchangers(RemHeatExchangers, NoHeatExchangers - 1, Tests ++ Test);

testHeatExchangers([], 0, Tests) ->
    Tests.

alive(Pid) ->
    erlang:is_process_alive(Pid)
.
	

