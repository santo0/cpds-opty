-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
            %% TODO: not tested
            send_read_checks(Reads, Tag),
            %% TODO: not tested
            case check_reads(length(Reads), Tag) of
                ok ->
                    %% TODO: not tested
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    %% TODO: not tested - I have the feeling more lines are necessary, but not sure.
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

update(Writes) ->
    lists:foreach(
        fun({_, Entry, Value}) ->
            %% TODO: not tested
            Entry ! Value
        end,
        Writes
    ).

send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(
        fun({Entry, Time}) ->
            %% TODO: not tested
            Entry ! {check, Tag, Time, Self}
        end,
        Reads
    ).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N - 1, Tag);
        {Tag, abort} ->
            abort
    end.
