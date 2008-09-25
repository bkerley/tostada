-module (memoryhole).
-author ("bkerley@brycekerley.net").
-export ([init/0, store/2, fetch/1]).
-include ("memoryhole.hrl").

init() ->
	mnesia:create_table(mhitem, [{ram_copies, [node()]}, {attributes, record_info(fields, mhitem)}]).

store(Key, Value) ->
	Writer = fun () ->
		mnesia:write(#mhitem{key=Key, value=Value})
	end,
	mnesia:transaction(Writer).

fetch(Key) ->
	Reader = fun() ->
		mnesia:read({mhitem, Key})
	end,
	{atomic,[Row]}=mnesia:transaction(Reader),
	Row#mhitem.value.