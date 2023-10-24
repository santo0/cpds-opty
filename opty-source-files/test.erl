-module(test).
-export([test/0]).

-include_lib("stdlib/include/assert.hrl").

recv() ->
    receive
        X -> X
    end.

entry_test() ->
    EntryA = entry:new(100),
    EntryB = entry:new(200),
    entry_test(EntryA, 100, EntryB, 200).

entry_test(EntryA, InitialA, EntryB, InitialB) ->
    ?assert(is_pid(EntryA)),
    ?assert(is_pid(EntryB)),

    %% Reading is OK in both entries
    EntryA ! {read, 1, self()},
    T1 = recv(),
    ?assertMatch({1, _, InitialA, _}, T1),
    {1, _, InitialA, Time1} = T1,

    EntryB ! {read, 2, self()},
    T2 = recv(),
    %% This "Time" should be equal to the previous one
    ?assertMatch({2, _, InitialB, _}, T2),
    {2, _, InitialB, Time2} = T2,
    %% Entries A and B should not have the same timestamp
    ?assert(Time2 /= Time1),

    %% EntryA write is OK
    EntryA ! {write, 105},
    EntryA ! {read, 3, self()},
    T3 = recv(),
    ?assertMatch({3, _, 105, _}, T3),
    {3, _, 105, Time3} = T3,
    ?assert(Time3 /= Time1),

    %% EntryB is not modified
    EntryB ! {read, 4, self()},
    %% This "Time" should be equal to the previous one.
    ?assertMatch({4, _, InitialB, Time2}, recv()),

    %% Time1 should not check, since EntryA has been written
    EntryA ! {check, 5, Time1, self()},
    ?assertMatch({5, abort}, recv()),
    %% But Time3 should check
    EntryA ! {check, 6, Time3, self()},
    ?assertMatch({6, ok}, recv()),

    %% EntryB should go independently from all this
    EntryB ! {read, 7, self()},
    %% This "Time" should be equal to the previous one.
    ?assertMatch({7, _, InitialB, Time2}, recv()),

    EntryA ! stop,
    EntryB ! stop,
    ok.

store_test() ->
    Store = store:new(4),

    ?assert(tuple_size(Store) == 4),
    entry_test(store:lookup(1, Store), 0, store:lookup(2, Store), 0),
    entry_test(store:lookup(3, Store), 0, store:lookup(4, Store), 0),

    store:stop(Store).

handler_test() ->
    Store = store:new(4),
    Handler = handler:start(self(), self(), Store),

    %% These writes get inserted in the write set. The 200 value
    %% overwrites the 100 value, both in the entry 2, so there'll
    %% be only 1 annotation in the write set for entry 2.
    Handler ! {write, 2, 100},
    Handler ! {write, 3, 300},
    Handler ! {write, 2, 200},

    %% These reads are in the write set, so doesn't count for the
    %% read set.
    Handler ! {read, 1, 2},
    ?assertMatch({value, 1, 200}, recv()),

    Handler ! {read, 2, 3},
    ?assertMatch({value, 2, 300}, recv()),

    %% This read, however, is from an entry that has not been
    %% write, so it is not in the write set, so it gets inserted
    %% in the read set.
    Handler ! {read, 3, 1},
    ?assertMatch({value, 3, 0}, recv()),

    %% A commit, which we check it returns the valid data.
    Handler ! {commit, 4},
    Entry1 = store:lookup(1, Store),
    Entry2 = store:lookup(2, Store),
    Entry3 = store:lookup(3, Store),
    Self = self(),
    ?assertMatch({validate, 4, [{Entry1, _}], [{2, Entry2, 200}, {3, Entry3, 300}], Self}, recv()),

    Handler ! abort.

test() ->
    entry_test(),
    store_test(),
    handler_test(),
    ok.
