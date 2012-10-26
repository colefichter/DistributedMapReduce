-module(mrs).
%MRS - a MapReduce Server.

-compile([export_all]).

-define(SERVER, mrs).

%client API ----------------------------------------------------
register(Pid) ->
    global:send(?SERVER, {register, Pid}).

store(Key, Value) ->
    global:send(?SERVER, {store, {Key, Value}}).

print() ->
    global:send(?SERVER, {print}).


mapreduce(Map, Reduce) ->
    SortPredicate = fun(X,Y) -> X =< Y  end,
    ?MODULE:mapreduce(Map, Reduce, SortPredicate).

mapreduce(Map, Reduce, SortPredicate) ->
    global:send(?SERVER, {mapreduce, Map, Reduce, SortPredicate}).      

%server implementation ----------------------------------------
start() ->
    %register the server process globally, across the entire cluster.
    global:trans({?SERVER, ?SERVER},
		 fun() ->
			 case global:whereis_name(?SERVER) of
			     undefined ->
				 FirstWorker = spawn(worker, server_loop, [[]]),
				 Workers = [FirstWorker],
				 Pid = spawn(?MODULE, server_loop, [Workers]),
				 global:register_name(?SERVER, Pid);
			     _ ->
				 ok
			 end
		 end).

server_loop(Workers) -> % The main processing loop for the server.
    receive
	{mapreduce, MapFun, ReduceFun, SortPredicate} ->
	    Self = self(),
	    lists:foreach(fun (Pid) ->
				  Pid ! {map, Self, MapFun}
			  end, Workers),
	    N = length(Workers),	    
	    %% Wait for N Map processes to terminate
	    MapResults1 = collect_replies(N, []),
	    %Aggregate values into lists by distinct key
	    MapResults2 = lists:foldl(fun({K,V}, Acc0) -> 
					      case dict:find(K, Acc0) of
						  error ->
						      dict:store(K, [V], Acc0);
						  {ok, ValuesList} ->
						      dict:store(K, [V|ValuesList], Acc0)
					      end
				      end, dict:new(), MapResults1),
	    %Run reduce logic on each {Key, ValueList} tuple to produce {Key, ReduceResult} tuples in dict.
	    ReduceResult = dict:map(ReduceFun, MapResults2), 
	    SortedResults = lists:sort(SortPredicate, dict:to_list(ReduceResult)),
	    %print final results as a list of {Key, ReduceOutput} tuples.
	    io:format("MapReduce Result:~n    ~p~n", [SortedResults]), 
	    server_loop(Workers);	    
	{store, {Key, Value}} ->
	    Index = get_machine_index(Key, Workers),
	    Worker = lists:nth(Index, Workers),
	    Worker ! {store, {Key, Value}},
	    server_loop(Workers);	    
	{print} ->
	    io:format("Workers: ~p~n", [Workers]),
	    lists:foreach(fun (Pid) ->  Pid ! {print} end, Workers),
	    server_loop(Workers);
	{register, Pid} ->
	    Id = length(Workers) + 1,
	    io:format("Registering worker ~p (~p).~n", [Id, Pid]),
	    server_loop([Pid|Workers])
    end.

get_machine_index(Key, Workers) ->  
    %For the hash, we'll use a simple Consistent Hash Ring strategy: http://www.martinbroadhurst.com/Consistent-Hash-Ring.html
    H = erlang:phash2(Key),
    H rem length(Workers) + 1. %lists use 1-based indexing
    
collect_replies(0, List) ->
    List1 = lists:flatten(List),
    lists:filter(fun(X) -> X =/= nil  end, List1); %remove the nil entries...
collect_replies(N, List) ->
    receive
	{map_result, _Worker, []} ->
	    collect_replies(N-1, List);
	{map_result, _Worker, ResultList} ->	   
	    collect_replies(N-1, [ResultList|List])
    end.
	    

    
