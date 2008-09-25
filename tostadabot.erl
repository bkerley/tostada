-module (tostadabot).
-author ("bkerley@brycekerley.net").
-export ([start/2, connect/2, loop/1]).
-include ("irc.hrl").
-include ("command.hrl").
-import (irc, [build/1]).
-import (handler, [handle_message/2]).
-define (nickname, "tostada").

start(Host, Port) ->
	mnesia:create_schema(node()),
	mnesia:start(),
	memoryhole:init(),
	obscura:init(),
	spawn(tostadabot, connect, [Host, Port]).

connect(Host, Port) ->
	try
		{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}]),
		gen_tcp:send(Sock, build(#ircmesg{command="NICK", params=[?nickname]})),
		gen_tcp:send(Sock, build(#ircmesg{command="USER", params=[?nickname, "alpha", "bravo", "charlie", "delta"]})),
		loop(Sock)
	catch
		error:X ->
			io:format("Just died :(~n"),
			io:format("~p~n",[{X, erlang:get_stacktrace()}]),
			exit(stopped)
	end.

loop(Sock) ->
	receive
		{tcp, Sock, Data} ->
			_ = spawn(handler, handle_message, [self(), Data]),
			loop(Sock);
		{reply, Message} ->
			gen_tcp:send(Sock, build(Message)),
			loop(Sock);
		{code_switch} ->
			tostadabot:loop(Sock);
		{quit, Message} ->
			gen_tcp:send(Sock, build(#ircmesg{command="QUIT", params=[Message]})),
			io:format("[~w] Quitting~n", [Sock]),
			gen_tcp:close(Sock),
			exit(stopped)
	end.

