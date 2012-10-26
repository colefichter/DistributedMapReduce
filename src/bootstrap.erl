-module(bootstrap).

-compile([export_all]).

start() ->
    mrs:start(),
    worker:new(),
    worker:new(),
    worker:new(),
    %seed().
    mrs:store("Cole", 1),
    mrs:store("Cole", 2),
    mrs:store("Cole", 3),
    mrs:store("Cameron", 49),
    mrs:store("Cameron", 50),
    mrs:store("Mike", 99),
    mrs:store("Mike", 100),
    mrs:store("Fred", 1),
    mrs:store("12345", asdf),
    mrs:store("MyKey", "MyValue"),
    mrs:store(tuple, {with_an, atom_for_a_key}).

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
