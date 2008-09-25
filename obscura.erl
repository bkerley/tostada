-module (obscura).
-author ("bkerley@brycekerley.net").
-export ([eval/1, init/0]).
-include ("obscura.hrl").
-import (io_lib, [format/2]).

eval(Line) ->
	Nodes = integerize(Line, []),
	eval(Nodes, []).

eval(Rest, {obserror, Desc}) ->
	describe_error(Desc, Rest, [trashed]);

eval(Rest, Stack) when length(Stack) > 5000 ->
	describe_error("Stack too big (more than 5000 entries)", Rest, [toobig]);

eval([], Stack) -> % no more ops, dump stack
	Stack;

eval([First|Rest], Stack) when is_integer(First); is_record(First, obssym); true == First -> % push
	eval(Rest, [First|Stack]);

eval(["x"|Rest], Stack) -> % swap
	case pop_two(Stack) of
		{ok, Alpha, Bravo, Remain} ->
			eval(Rest, [Bravo|[Alpha|Remain]]);
		stack_underflow ->
			describe_error("Stack underflow", Rest, Stack)
	end;

eval(["."|Rest], Stack) -> % dup
	case Stack of
		[] ->
			describe_error("Stack underflow (dup'd an empty stack like a tard)", Rest, Stack);
		[Top|Remain] ->
			eval(Rest, [Top|[Top|Remain]])
	end;

eval(["+"|Rest], Stack) ->
	binary_op(fun (A,B) -> A+B end, Stack, Rest);

eval(["*"|Rest], Stack) ->
	binary_op(fun (A,B) -> A*B end, Stack, Rest);

eval(["-"|Rest], Stack) ->
	binary_op(fun (A,B) -> A-B end, Stack, Rest);

eval(["/"|Rest], Stack) -> % numeric divide
	Div = fun 
		(0,_) -> {op_fail, "Divide by zero"};
		(A,B) -> B/A
	end,
	binary_op(Div, Stack, Rest);

eval(["/i"|Rest], Stack) -> % integer divide
	Div = fun
		(0,_) -> {op_fail, "Divide by zero"};
		(A,B) -> B div A
	end,
	integer_op(Div, Stack, Rest);

eval(["%i"|Rest], Stack) -> % integer modulus
	Div = fun
		(0,_) -> {op_fail, "Modulus by zero"};
		(A,B) -> B rem A
	end,
	integer_op(Div, Stack, Rest);

eval(["round"|Rest], Stack) -> % truncate
	unary_op(fun (A) -> trunc(A) end, Stack, Rest);

eval(["not"|Rest], [Head|Tail]) when is_integer(Head) -> % bitwise not
	unary_op(fun (A) -> bnot A end, [Head|Tail], Rest);
eval(["not"|Rest], Stack) -> describe_error("Non-integer arg to bitwise not", Rest, Stack);

eval(["and"|Rest], Stack) -> integer_op(fun(A,B) -> A band B end, Stack, Rest);
eval(["or"|Rest], Stack) -> integer_op(fun(A,B) -> A bor B end, Stack, Rest);
eval(["xor"|Rest], Stack) -> integer_op(fun(A,B) -> A bxor B end, Stack, Rest);
eval(["asl"|Rest], Stack) -> integer_op(fun(A,B) -> B bsl A end, Stack, Rest);
eval(["asr"|Rest], Stack) -> integer_op(fun(A,B) -> B bsr A end, Stack, Rest);

eval(["ifelse"|Rest], Stack) when length(Stack) < 3 ->
	describe_error("ifelse wants args: value, then, else", Rest, Stack);
eval(["ifelse"|Rest], Stack) ->
	[Else|[Then|[Value|Tail]]] = Stack,
	case Value of
		0 ->
			eval(Rest, eval(Then, Tail));
		true ->
			eval(Rest, eval(Then, Tail));
		_ ->
			eval(Rest, eval(Else, Tail))
	end;

eval(["eval"|Rest], [Head|Tail]) when is_list(Head) ->
	eval(Rest, eval(Head, Tail));

eval(["proc"|Rest], Stack) ->
	binary_op(fun
		(_, []) ->
			{op_fail, "Cowardly refusing to store an empty proc."};
		(A,B) when is_record(A, obssym), is_list(B) ->
			storeproc(A, B),
			true;
		(_,_) -> 
			{op_fail, "Proc wants a symbol and marked block as arguments."}
	end, Stack, Rest);

eval(["do"|Rest], [Head|Tail]) when is_record(Head, obssym) ->
	case fetchproc(Head) of
		noproc ->
			describe_error(format("Couldn't find a proc named ~p",[Head#obssym.name]), Rest, [Head|Tail]);
		Proc ->
			eval(Rest, eval(Proc, Tail))
	end;
eval(["do"|Rest], Stack) ->
	describe_error("Do wants a symbol as an argument.", Rest, Stack);

eval(["desc"|Rest], [Head|Tail]) when is_record(Head, obssym) ->
	case fetchproc(Head) of
		noproc ->
			describe_error(format("Couldn't find a proc named ~p",[Head#obssym.name]), Rest, [Head|Tail]);
		Proc ->
			eval(Rest, [Proc|Tail])
	end;
eval(["desc"|Rest], Stack) ->
	describe_error("Desc wants a symbol as an argument.", Rest, Stack);

eval(["{"|Rest], Stack) -> marked_eval(Rest, Stack, [], 1); % begin marked_eval

eval([Invalid|Rest], Stack) ->
	case fetchproc(#obssym{name=Invalid}) of
		noproc ->
			describe_error(format("Invalid opcode \"~s\" with no proc",[Invalid]), Rest, Stack);
		Proc ->
			eval(Rest, eval(Proc, Stack))
	end.

marked_eval(["}"|Rest], Stack, Accum, 1) ->
	eval(Rest, [lists:reverse(Accum)|Stack]);
marked_eval(["}"|Rest], Stack, Accum, N) ->
	marked_eval(Rest, Stack, ["}"|Accum], N-1);
marked_eval(["{"|Rest], Stack, Accum, N) ->
	marked_eval(Rest, Stack, ["{"|Accum], N+1);
marked_eval([First|Rest], Stack, Accum, Depth) ->
	marked_eval(Rest, Stack, [First|Accum], Depth);

marked_eval([], Stack, Accum, Depth) ->
	E = format("Unclosed mark with depth: ~w accum: ~w",[Depth, Accum]),
	describe_error(E, [], Stack).

describe_error(Desc, [], [trashed]) ->
	{obserror, Desc};
describe_error(Desc, [], Stack) ->
	{obserror, format("~s. Stack ~w", [Desc, Stack])};
describe_error(Desc, Rest, Stack) ->
	{obserror, format("~s.  Rest: ~w Stack ~w", [Desc, Rest, Stack])}.

integer_op(Fun, Stack, Rest) ->
	Op = fun
		(A,B) when is_integer(A), is_integer(B) ->
			Fun(A,B);
		(_,_) -> {op_fail, "Non-integer arg(s) to integer operator"}
	end,
	binary_op(Op, Stack, Rest).

binary_op(Fun, Stack, Rest) ->
	case pop_two(Stack) of
		{ok, Alpha, Bravo, Remain} ->
			case Fun(Alpha, Bravo) of
				{op_fail, Desc} ->
					describe_error(Desc, Rest, Stack);
				Result ->
					eval(Rest, [Result|Remain])
			end;
		stack_underflow ->
			describe_error("Stack underflow", Rest, Stack)
	end.

unary_op(_, [], Rest) -> describe_error("Stack underflow", Rest, []);
unary_op(Fun, [Head|Tail], Rest) ->
	eval(Rest, [Fun(Head)|Tail]).

pop_two(Stack) when length(Stack) < 2 -> stack_underflow;
pop_two(Stack) ->
	[Alpha|[Bravo|Remain]] = Stack,
	{ok, Alpha, Bravo, Remain}.


init() ->
	mnesia:create_table(obsproc, [
		{disc_copies, [node()]},
		{attributes, record_info(fields, obsproc)}
	]).

storeproc(Name, Proc) when is_record(Name, obssym), is_list(Proc) ->
	Writer = fun () ->
		mnesia:write(#obsproc{name=Name#obssym.name, body=Proc})
	end,
	mnesia:transaction(Writer).

fetchproc(Name) when is_record(Name, obssym) ->
	Reader = fun() ->
		mnesia:read({obsproc, Name#obssym.name})
	end,
	case mnesia:transaction(Reader) of
		{atomic,[Row]} ->
			Row#obsproc.body;
		{atomic,[]} ->
			noproc
	end.

integerize([],Accum) ->
	lists:reverse(Accum);
integerize([First|Rest], Accum) ->
	integerize(Rest, [try_integerize(First)|Accum]).

try_integerize(Candidate) ->
	try
		list_to_integer(Candidate)
	catch
		error:_ ->
			based_integerize(Candidate)
	end.

based_integerize(Candidate) ->
	case string:substr(Candidate,1,2) of
		"0x" -> erlang:list_to_integer(string:substr(Candidate, 3), 16);
		"0b" -> erlang:list_to_integer(string:substr(Candidate, 3), 2);
		"::" -> #obssym{name=string:substr(Candidate, 3)};
		":t" -> true;
		_ -> Candidate
	end.
