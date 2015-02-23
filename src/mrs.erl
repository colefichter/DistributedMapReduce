-module(mrs).
%MRS - a MapReduce Server.

-compile([export_all]).

-define(SERVER, mrs).

%client API ----------------------------------------------------
register(Pid) ->
    ?SERVER ! {register, Pid}.

store(Int) ->
    ?SERVER ! {store, Int}.

print() ->
    ?SERVER ! {print}.

reset() ->
    ?SERVER ! {reset}.

mapreduce(Map, Reduce) ->
    ?SERVER ! {mapreduce, self(), Map, Reduce},
    receive
    	{result, _From, ReduceResult} ->
    		ReduceResult
	end.	

rebalance() ->
	?SERVER ! {rebalance}.

%server implementation ----------------------------------------
start() ->
    Workers = [],
    Pid = spawn(?MODULE, server_loop, [Workers]),
    register(?SERVER, Pid).

server_loop(Workers) -> % The main processing loop for the server.
    receive
	{mapreduce, From, MapFun, ReduceFun} ->
	    Self = self(),
	    lists:foreach(fun (Pid) ->
				  Pid ! {map, Self, MapFun}
			  end, Workers),
	    N = length(Workers),	    
	    %% Wait for N Map processes to terminate
	    MapResults1 = collect_replies(N, dict:new()),
	    %Group results into a single list
	    MapResults2 = dict:fold(fun (_Key, Value, Acc0) -> [Value|Acc0] end, [], MapResults1),
	    ReduceResult = ReduceFun(lists:flatten(MapResults2)),
	    io:format("Sending MapReduce Result: ~p~n", [ReduceResult]),
	    From ! {result, self(), ReduceResult},
	    server_loop(Workers);	    
	{store, Int} ->
	    %For the hash, we will just find n MOD num_workers and store on that machine.
	    Index = (Int rem length(Workers)) + 1, %lists use 1-based indexing
	    Worker = lists:nth(Index, Workers),
	    Worker ! {store, Int},
	    server_loop(Workers);	  
	{rebalance} ->
	    io:format("Rebalancing Data...~n"),
	    NumWorkers = length(Workers),
	    From = self(),
	    _unused = lists:foldl(fun(Worker, Index) ->
				Worker ! {rebalance, From, NumWorkers, Index},
				receive
					{purged_data, Items} ->
						lists:foreach(fun mrs:store/1, Items)
				end,
				Index + 1
			end, 0, Workers),
	    server_loop(Workers);
	{print} ->
	    io:format("Workers: ~p~n", [Workers]),
	    lists:foreach(fun (Pid) ->  Pid ! {print} end, Workers),
	    server_loop(Workers);
	{register, Pid} ->
	    Id = length(Workers) + 1,
	    io:format("Registering worker ~p (~p).~n", [Id, Pid]),
	    mrs:rebalance(),	    
	    server_loop([Pid|Workers]);
	{reset} ->
	    lists:foreach(fun(Pid) -> Pid ! {reset} end, Workers),
	    server_loop(Workers)
    end.

collect_replies(0, Dict) ->
    Dict;
collect_replies(N, Dict) ->
    receive
	{map_result, Worker, ResultList} ->	   
	    case dict:is_key(Worker, Dict) of
		true ->
		    %THIS SHOULDN'T HAPPEN
		    io:format("THIS SHOULDN'T HAPPEN!", []);		    
		false ->
		    Dict1 = dict:store(Worker, ResultList, Dict),
		    collect_replies(N-1, Dict1)
	    end
    end.
