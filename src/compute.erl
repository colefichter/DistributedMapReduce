-module(compute).

-compile([export_all]).

% Sample map-reduce algorithms --------------------------------
count() ->
    Map = fun(_X) -> 1  end,
    Reduce = fun(List) -> length(List) end,
    mrs:mapreduce(Map, Reduce).

sum() -> %Find the sum of all the integers stored in the system
    Map = fun(X) -> X end,
    Reduce = fun lists:sum/1,
    mrs:mapreduce(Map, Reduce).

min() -> %Find the smallest integer stored in the system
    Map = fun(X) -> X end,
    Reduce = fun lists:min/1,
    mrs:mapreduce(Map, Reduce).

max() -> %Find the largest integer stored in the system 
    Map = fun(X) -> X end,
    Reduce = fun lists:max/1,
    mrs:mapreduce(Map, Reduce).

mean() ->
    Map = fun(X) -> X end,
    Reduce = fun(List) ->
		     {Sum, Count} = lists:foldl(fun(X, {Sum0, Count0}) ->
							{Sum0 + X, Count0 + 1}	
						end, {0,0}, List),
		     Sum / Count
	     end,
    mrs:mapreduce(Map, Reduce).

median() ->		  
    Map = fun(X) -> X end,
    Reduce = fun(List) ->
		     List1 = lists:sort(List),
		     N = length(List1),
		     I = round(N / 2),
		     Median = case (N rem 2) of 
				  1 ->
				      lists:nth(I, List1);
				  0 ->
				      Sum = lists:nth(I, List1) + lists:nth(I+1, List1),
				      Sum / 2
			      end,
		     Median
	     end,
     mrs:mapreduce(Map, Reduce).

mode() -> %Find the most common integer stored in the system, and the number of times it occurs.
    Map = fun(X) -> X end,
    Reduce = fun(List) -> 
		     KeyValuePairs = lists:foldl(fun(X, Dict) -> dict:update_counter(X, 1, Dict) end, dict:new(), List),
		     {MostCommon, MaxCount} = dict:fold(fun(K,V,{MostCommon, MaxCount}) ->
					     case V > MaxCount of
						 true ->
						     {K, V};
						 false ->
						     {MostCommon, MaxCount}
					     end
				     end,
				     {-1, -99999999},
				     KeyValuePairs),
		     [{most_common, MostCommon}, {count, MaxCount}]
	     end,
    mrs:mapreduce(Map, Reduce).
