-module(compute).

-compile([export_all]).

%%Old functions for the integer mapreduce version (not working in this branch)

% Sample map-reduce algorithms --------------------------------
%count() -> %Count the number of integers stored in the system
%    Map = fun(_X) -> 1  end,
%    Reduce = fun(List) -> length(List) end,
%    mrs:mapreduce(Map, Reduce).

%sum() -> %Find the sum of all the integers stored in the system
%    Map = fun(X) -> X end,
%    Reduce = fun lists:sum/1,
%    mrs:mapreduce(Map, Reduce).

max() -> %Find the largest integer stored in the system 
    Map = fun(X) -> X end,
    Reduce = fun lists:max/1,
    mrs:mapreduce(Map, Reduce).

min() -> %Find the smallest integer stored in the system
    Map = fun(X) -> X end,
    Reduce = fun lists:min/1,
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

most_common() -> %Find the most common integer stored in the system, and the number of times it occurs.
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



%% New functions for the KeyValue mapreduce version...

count() ->
    Map = fun(_K, _V) -> {count, 1} end,
    Reduce = fun(_K, ValueList) -> length(ValueList) end,
    mrs:mapreduce(Map, Reduce).	     

%Assumes that all values in the store are numbers!
sum() ->		   
    Map = fun(_K,V) -> {sum ,V} end,
    Reduce = fun(_K, ValueList) -> lists:sum(ValueList) end,
    mrs:mapreduce(Map, Reduce).

groupcount() ->
    Map = fun(K, _V) -> {K, 1} end,
    Reduce = fun(_K, ValueList) -> length(ValueList) end,
    mrs:mapreduce(Map, Reduce).
				    
groupsum() ->		     
    Map = fun(K,V) -> {K,V} end,
    Reduce = fun(_K, ValueList) -> lists:sum(ValueList) end,
    mrs:mapreduce(Map, Reduce).

find(QueryKey) ->
    Map = fun(K,V) ->
		  case K =:= QueryKey of
		      true ->
			  {K,V};
		      false ->
			  nil
		  end
	  end,
    Reduce = fun(_K,Values) -> Values end,
    mrs:mapreduce(Map, Reduce).

findbyvalue(Query) ->
    Map = fun(K,V) ->
		  case V =:= Query of
		      true ->
			  {K,V};
		      false ->
			  %nil is a special atom that we will filter out of the map results.
			  %We rely on this because erlang functions always return a value.
			  nil
		  end
	  end,
    Reduce = fun(_K, Values) -> Values end,
    mrs:mapreduce(Map,Reduce).
