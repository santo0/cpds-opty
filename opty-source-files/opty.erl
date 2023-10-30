-module(opty).
-export([start/6, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% EntriesPerClient: Number of Entries per client
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, EntriesPerClient, Reads, Writes, Time) ->
    CSVFile = io_lib:format(
        "clients~w.entries~w.reads~w.writes~w.time~w.csv",
        [Clients, Entries, Reads, Writes, Time]),

    %file:write_file(CSVFile,
    %        "Total, OK, Percentage\n"
    %),

    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, EntriesPerClient, Reads, Writes, CSVFile),
    io:format(
        "Starting: ~w CLIENTS, ~w ENTRIES, ~w EntriesPerClient, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
        [Clients, Entries, EntriesPerClient, Reads, Writes, Time]
    ),
    timer:sleep(Time * 1000),
    stop(L, CSVFile).

stop(L, CSVFile) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L, 0, 0, 0, length(L), CSVFile),
    s ! stop,
    io:format("Stopped~n"),
    erlang:halt().

startClients(0, L, _, _, _, _, _) ->
    L;
startClients(Clients, L, Entries, EntriesPerClient, Reads, Writes, CSVFile) ->
    RandomClientEntries = lists:sublist([X || {_ , X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])], EntriesPerClient),
    Pid = client:start(Clients, RandomClientEntries, Reads, Writes, s, CSVFile),
    startClients(Clients - 1, [Pid | L], Entries, EntriesPerClient, Reads, Writes, CSVFile).

stopClients([]) ->
    ok;
stopClients([Pid | L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([], Total, OK, Percentage, NumClients, CSVFile) ->
    file:write_file(CSVFile,
        io_lib:fwrite(
            "\n\n~w,~w,~w\n",
            [Total/NumClients, OK/NumClients, Percentage/NumClients]),
            [append]
    ),
    io:format(
        "######################## COMPOUNT RESULTS ##########################\n"
    ),
    io:format(
        "NumClients, Total, OK, Percentage\n"
    ),
    io:format(
        "~w,~w,~w,~w\n",
        [NumClients, Total/NumClients, OK/NumClients, Percentage/NumClients]
    ),
    ok;
waitClients(L, Total, OK, Percentage, NumClients, CSVFile) ->
    receive
        {done, Pid, ClientTotal, ClientOK, ClientPercentage} ->
            waitClients(lists:delete(Pid, L), Total+ClientTotal, OK+ClientOK, Percentage+ClientPercentage, NumClients, CSVFile)
    end.
