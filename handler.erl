-module (handler).
-author ("bkerley@brycekerley.net").
-include ("irc.hrl").
-include ("command.hrl").
-export ([handle_message/2]).
-import (irc, [parse/1]).
-import (command, [command_parse/1, command_evaluate/1]).
-define (channel, "#wcsc").

handle_message(Parent, Data) when not is_record(Data, ircmesg) ->
	Mesg = parse(Data),
	handle_message(Parent, Mesg#ircmesg.command, Mesg).

% message with my name
handle_message(Parent, "PRIVMSG", Mesg) ->
	[_|Content] = Mesg#ircmesg.params,
	Reply = select_reply(Mesg),
	Command = command_parse(Content),
	PackedCommand = Command#toscmd{sender=Mesg#ircmesg.prefix, channel=Reply},
	case command_evaluate(PackedCommand) of
		{bail, Message} ->
			Parent!{quit, Message};
		Output when is_list(Output) ->
			strutil:each_line(fun (Line) ->
					Outbound = #ircmesg{command="PRIVMSG", params=[Reply, Line]},
					Parent!{reply,Outbound}
			end, lists:flatten(Output), 4);
		Output when is_record(Output, ircmesg) ->
			Parent!{reply, Output};
		_ ->
			false
	end;

% end of motd
handle_message(Parent, "001", _) ->
	io:format("Joining channel~n",[]),
	Parent!{reply,#ircmesg{command="JOIN", params=[?channel]}};

handle_message(Parent, "PING", Mesg) ->
	Parent!{reply,#ircmesg{command="PONG", params=Mesg#ircmesg.params}};

handle_message(_, _, Mesg) ->
	io:format("~p ~n",[Mesg]).

select_reply(Mesg) ->
	[Channel|_] = Mesg#ircmesg.params,
	Sender = lists:nth(1, string:tokens(Mesg#ircmesg.prefix, "!@")),
	case Channel of
		"tostada" ->
			Sender;
		_ ->
			Channel
	end.
