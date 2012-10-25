-module(worker).

-compile([export_all]).

new() ->
    Pid = spawn(?MODULE, server_loop, [[]]),
    mrs:register(Pid),
    Pid.

server_loop(KeyValuePairs) ->    
    receive
	{map, From, Fun} ->
	    io:format("Mapping (~p)~n", [self()]),
	    %entries in this list have the format: {Key, Value}
	    ResultList = lists:map(fun({K,V}) -> Fun(K, V) end, KeyValuePairs),
%io:format("ResultList: ~p~n", [ResultList]),
	    From ! {map_result, self(), ResultList},
	    server_loop(KeyValuePairs);
	{store, {Key, Value}} ->
	    io:format("Storing ~p = ~p~n", [Key, Value]),
	    server_loop([{Key, Value}|KeyValuePairs]);
	{reset} ->
	    server_loop([]);
	{print} ->
	    io:format(" ~p: ~p~n", [self(), KeyValuePairs]),
	    server_loop(KeyValuePairs)
    end.
