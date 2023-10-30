-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

% New param: TransactionId
% No need to keep reads, Entry has to keep reads
init(Client, Validator, Store) ->
    TransactionId = make_ref(),
    handler(Client, Validator, Store, [], TransactionId).

handler(Client, Validator, Store, Writes, TransactionId) ->
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Writes, TransactionId);
                false ->
                    Entry = store:lookup(N, Store),
                    Entry ! {read, Ref, self(), TransactionId},
                    handler(Client, Validator, Store, Writes, TransactionId)
            end;
        {from_entry, Ref, Value} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, Writes, TransactionId);
        {write, N, Value} ->
            Entry = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Added, TransactionId);
        {commit, Ref} ->
            Validator ! {validate, Ref, Writes, Client, TransactionId};
        abort ->
            ok
    end.
