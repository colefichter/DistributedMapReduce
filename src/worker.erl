-module(worker).

-compile([export_all]).

new() ->
    Pid = spawn(?MODULE, server_loop, [[]]),
    mrs:register(Pid),
    Pid.

server_loop(Numbers) ->    
    receive
	{map, From, Fun} ->
	    io:format("Mapping (~p)~n", [self()]),
	    %entries in this list have the format: {Key, Value}
	    ResultList = lists:map(Fun, Numbers),
	    From ! {map_result, self(), ResultList},
	    server_loop(Numbers);
	{store, Int} ->
	    io:format("Storing ~p~n", [Int]),
	    server_loop([Int|Numbers]);
	{reset} ->
	    server_loop([]);
	{print} ->
	    io:format(" ~p: ~p~n", [self(), Numbers]),
	    server_loop(Numbers)
    end.
