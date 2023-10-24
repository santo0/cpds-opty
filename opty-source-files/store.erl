-module(store).
-export([new/1, stop/1, lookup/2]).

%% Store is just a tuple of entry processes, and is managed with these
%% functions. WARNING: Store is not a process, is just a value.

%% Create the store and initialize to 0 the internal entry processes.
new(N) ->
    list_to_tuple(entries(N, [])).

%% Send the stop signal to all the entry processes.
stop(Store) ->
    lists:foreach(
        fun(E) ->
            E ! stop
        end,
        tuple_to_list(Store)
    ).

%% Retrieve the Ith entry process (element is a BIF, and it is 1-indexed).
lookup(I, Store) ->
    element(I, Store).

%% Auxiliary function used by new/1 to build an initial list of entry
%% processes. From N down to 0, it takes a list and prepends an
%% entry:new(0) to it. This way, N entry processes are created in the list.
entries(0, ListSoFar) ->
    ListSoFar;
entries(N, ListSoFar) ->
    Entry = entry:new(0),
    entries(N - 1, [Entry | ListSoFar]).
