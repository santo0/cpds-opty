-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            From ! {Ref, self(), Value, Time}, % TODO - not tested
            entry(Value, Time);
        {write, New} ->
            entry(New, make_ref()); % TODO - not tested
        {check, Ref, Readtime, From} ->
            if 
                 Readtime == Time ->
                    From ! {Ref, ok}; %% TODO: not tested
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
