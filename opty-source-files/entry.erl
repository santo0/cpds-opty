-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, [], 0).

entry(Value, ActiveReads, WriteCount) ->
    receive
        {read, Ref, From, TransactionId} when WriteCount == 0 ->
            From ! {from_entry, Ref, self(), Value},
            % Avoid duped TransactionId in ActiveReads
            case lists:member(TransactionId, ActiveReads) of
                true ->
                    entry(Value, ActiveReads, WriteCount);
                false ->
                    entry(Value, [TransactionId | ActiveReads], WriteCount)
            end;
        {stop_reading, Ref, From, TransactionId} ->
            From ! {Ref, stopped},
            entry(Value, lists:delete(TransactionId, ActiveReads), WriteCount);
        {check, Ref, From, TransactionId} ->
            case check_conflict(ActiveReads) of
                true ->
                    From ! {Ref, abort};
                false ->
                    From ! {Ref, ok}
            end,
            entry(Value, lists:delete(TransactionId, ActiveReads), WriteCount + 1);
        {write, New} ->
            entry(New, ActiveReads, WriteCount - 1);
        unblock ->
            entry(Value, ActiveReads, WriteCount - 1);
        stop ->
            ok
    end.
%%Return True if conflict, False otherwise
check_conflict(ActiveReads) ->
    case ActiveReads of
        % No Active Reads, no conflict
        [] -> false;
        % Ther are Active Reads that are from other Transactions, There is conflict!
        _ -> true
    end.
