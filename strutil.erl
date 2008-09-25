-module (strutil).
-author ("bkerley@brycekerley.net").
-export ([string_join/2, each_line/2, each_line/3]).

string_join(Items, Sep) ->
	lists:flatten(lists:reverse(string_join1(Items, Sep, []))).

string_join1([Head | []], _Sep, Acc) ->
	[Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
	string_join1(Tail, Sep, [Sep, Head | Acc]).

each_line(Fun, String, Limit) ->
	each_line1(Fun, string:tokens(String, "\n"), 1, Limit).

each_line1(_, [], _, _) -> true;
each_line1(Fun, _, Max, Max) ->
	Fun("Maximum lines reached");
each_line1(Fun, [Head|Tail], N, Max) ->
	Fun(Head),
	each_line1(Fun, Tail, N+1, Max).

each_line(Fun, String) ->
	each_line1(Fun, string:tokens(String, "\n")).

each_line1(_, []) -> true;
each_line1(Fun, [Head|Tail]) ->
	Fun(Head),
	each_line1(Fun, Tail).