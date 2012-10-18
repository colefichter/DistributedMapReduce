-module(bootstrap).

-compile([export_all]).

start() ->
    mrs:start(),
    worker:new(),
    worker:new(),
    worker:new(),
    seed().

seed(N) ->
    Integers = lists:seq(0, N),
    lists:foreach(fun (I) ->
			  mrs:store(I)
		  end, Integers).    
seed() ->
    Integers = lists:seq(0, 99),
    lists:foreach(fun (I) ->
			  mrs:store(I)
		  end, Integers).
