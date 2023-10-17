-module(entry).
-export([new/1]).

%% An entry is an Erlang process that waits for _reading_ or
%% _writing_ messages, and returns its internal state or
%% updates it accordingly.

%% Initialization of an entry.
new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
    	%% If it is a "read" command, send back the internal state
        {read, Ref, From} ->
            From ! {Ref, self(), Value, Time}, % TODO - not tested
            entry(Value, Time);

	%% If it is a "write" command, "New" becomes the new "Value"
        {write, New} ->
            entry(New, make_ref()); % TODO - not tested

	%% For validation purposes, a "check" command returns
	%% whether the entry has changed from Readtime to Time
        {check, Ref, Readtime, From} ->
            if
                 Readtime == Time ->
                    From ! {Ref, ok}; %% TODO: not tested
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);

	%% If a "stop" command is issued, then **return** ok (instead of
        %% sending a message), and don't do the recursive call,
        %% causing termination
        stop ->
            ok
    end.
