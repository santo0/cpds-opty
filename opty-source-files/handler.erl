-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
    receive
        {read, Ref, N} ->
            %% (key, N, tuplelist) Writers[{N, Entry, Value}], Reads[{Entry, Time}]
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                    Entry = store:lookup(N, Store),
                    %% TODO: ADD SOME CODE
                    Entry ! {read, Ref, self()},
                    %% TODO: ADD SOME CODE
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value, Time} ->
            %% TODO: ADD SOME CODE HERE AND COMPLETE NEXT LINE
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time} | Reads], Writes);
        {write, N, Value} ->
            %% TODO: ADD SOME CODE HERE AND COMPLETE NEXT LINE
            io:format("~w ~w ~n", [N, Store]),
            Entry = store:lookup(N, Store),
            %% keystore(Key, N, TupleList1, NewTuple) -> TupleList2
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Reads, Added);
        {commit, Ref} ->
            %% TODO: ADD SOME CODE
            Validator ! {validate, Ref, Reads, Writes, Client};
        abort ->
            ok
    end.
