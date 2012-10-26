-module(compute).

-compile([export_all]).

%% New functions for the KeyValue mapreduce version...
count() ->
    Map = fun(_K, _V) -> {count, 1} end,
    Reduce = fun(_K, ValueList) -> length(ValueList) end,
    mrs:mapreduce(Map, Reduce).	     


sum() -> %Assumes that all values in the store are numbers!
    Map = fun(_K,V) -> {sum ,V} end,
    Reduce = fun(_K, ValueList) -> lists:sum(ValueList) end,
    mrs:mapreduce(Map, Reduce).

group_count() ->
    Map = fun(K, _V) -> {K, 1} end,
    Reduce = fun(_K, ValueList) -> length(ValueList) end,
    mrs:mapreduce(Map, Reduce).
				    
group_sum() -> %Assumes that all values in the store are numbers!	     
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

find_by_value(Query) ->
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

find_by_value(Query, reverse) ->
    SortPredicate = fun(X,Y) -> Y =< X  end, %reverses the usual sort...
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
    mrs:mapreduce(Map,Reduce, SortPredicate).
