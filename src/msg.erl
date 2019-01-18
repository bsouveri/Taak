-module(msg).
-define(TimeOut, 5000).
-export([get/2, get/3]).
-export([test/0]). 

-spec get(pid(),_) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get(Pid, Key) ->
	Pid ! {Key, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

-spec get(pid(), _, any()) -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
get(Pid, Key, P_list) -> 
	Pid ! {Key, P_list, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

-spec replier(reference()) -> fun((_) -> {reference(),_}).
replier(Ref) ->  
    Sender = self(),
	fun(Msg) -> Sender ! {Ref, Msg} end.

-spec test() -> {'ok',_}|{'error', 'timed_out', pid(), _, reference()}.
test() -> 
	Pid = spawn(fun() -> receive {_Dummy, [P | _ ], F } -> F(2 * P) end, ok end),
	msg:get(Pid, dummy, [10]). 

