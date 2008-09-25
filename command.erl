-module (command).
-author ("bkerley@brycekerley.net").
-include ("irc.hrl").
-include ("command.hrl").
-export ([command_parse/1, command_evaluate/1]).
-import (io_lib, [format/2]).

command_parse([Content]) when is_list(Content) ->
	try
		Tokens = string:tokens(Content, " "),
		[Keyword|[Command|Args]] = Tokens,
		#toscmd{keyword=Keyword, command=Command, args=Args}
	catch
		error:{badmatch,_} ->
			#toscmd{}
	end.

command_evaluate(Cmd) when not is_record(Cmd, toscmd) ->
	erlang:error(badarg, "command_evaluate expects a toscmd");

command_evaluate(Cmd) when Cmd#toscmd.keyword == "erl" ->
	command(Cmd#toscmd.command, Cmd);

command_evaluate(Cmd) when Cmd#toscmd.keyword == "obs"; Cmd#toscmd.keyword == "obp" ->
	obscura(Cmd);

command_evaluate(_) -> false.

obscura(Cmd) ->
	Self = self(),
	Child = spawn(fun()-> obscura_wrap(Self,[Cmd#toscmd.command|Cmd#toscmd.args]) end),
	receive
		{obs_done, Output} ->
			Format = case Cmd#toscmd.keyword of
				"obs" ->
					"~w";
				"obp" ->
					"~p"
			end,
			case Output of
				{obserror, Desc} ->
					Desc;
				_ ->
					format(Format, [Output])
			end
	after 5000 ->
		exit(Child, timeout),
		"Timed out after 5000 ms."
	end.

obscura_wrap(Parent, Line) ->
	Result = obscura:eval(Line),
	Parent!{obs_done, Result}.

command("channel",Cmd) ->
	format("This is ~p",[Cmd#toscmd.channel]);

command("version",_) ->
	"I don't have a version.";

command("slavepid",_) ->
	format("I'm ~p", [self()]);

command("whoami",Cmd) ->
	format("You're ~p", [Cmd#toscmd.sender]);

command("sleep",Cmd) ->
	Sleeptime = list_to_integer(lists:nth(1, Cmd#toscmd.args)),
	timer:sleep(Sleeptime),
	format("I am awake after sleeping ~p", [Sleeptime]);

command("remember",Cmd) ->
	[Key| Value] = Cmd#toscmd.args,
	CollapsedValue = strutil:string_join(Value, " "),
	{_, ok} = memoryhole:store(Key, CollapsedValue),
	format("Stored ~p into ~p", [CollapsedValue, Key]);

command("whatis", Cmd) ->
	[Key | _] = Cmd#toscmd.args,
	try
		Value = memoryhole:fetch(Key),
		format("~p is ~p.",[Key, Value])
	catch
		error:{badmatch, _} ->
			format("I don't know anything about ~p",[Key])
	end;

command("twothings", _) ->
	lists:flatten(format("First, I rule.~nSecond, you rule!",[]));

command("join", Cmd) ->
	Chan = lists:nth(1, Cmd#toscmd.args),
	#ircmesg{command="JOIN", params=[Chan]};

command("part", Cmd) ->
	[Chan|Mesg] = Cmd#toscmd.args,
	#ircmesg{command="PART", params=[Chan|Mesg]};

command("dump", _) ->
	Filename = "manualdump.erl",
	mnesia:dump_to_textfile(Filename),
	format("Tables dumped to ~p.",[Filename]);

command("bail", _) ->
	{bail, "told to leave"};

command(Cmd,_) ->
	format("~p isn't a command you fag", [Cmd]).
