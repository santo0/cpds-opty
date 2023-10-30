-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    validator().

validator() ->
    receive
        %% Removed Reads, no necesary
        {validate, Ref, Reads, Writes, Client, TransactionId} ->
            Tag = make_ref(),
            send_stop_reading(Reads, Tag, TransactionId),
            wait_stop_reading(length(Reads), Tag),
            send_check_conflict(Writes, Tag, TransactionId),
            case check_writes(length(Writes), Tag) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    abort_entries_writes(Writes),
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
            Entry ! {write, Value}
        end,
        Writes
    ).

abort_entries_writes(Writes) ->
    lists:foreach(
        fun({_, Entry, _}) ->
            Entry ! unblock
        end,
        Writes
    ).

wait_stop_reading(0, _) ->
    ok;
wait_stop_reading(N, Tag) ->
    receive
        {Tag, stopped} ->
            wait_stop_reading(N - 1, Tag)
    end.

send_stop_reading(Reads, Tag, TransactionId) ->
    Self = self(),
    lists:foreach(
        fun(Entry) ->
            Entry ! {stop_reading, Tag, Self, TransactionId}
        end,
        Reads
    ).
send_check_conflict(Writes, Tag, TransactionId) ->
    Self = self(),
    lists:foreach(
        fun({_, Entry, _}) ->
            Entry ! {check, Tag, Self, TransactionId}
        end,
        Writes
    ).

check_writes(0, _) ->
    ok;
check_writes(N, Tag) ->
    receive
        {Tag, ok} ->
            check_writes(N - 1, Tag);
        {Tag, abort} ->
            abort
    end.
