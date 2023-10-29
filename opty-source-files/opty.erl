-module(opty).
-export([start/5, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time, ExecNumber) ->
    CSVFile = io_lib:format(
        "clients~w.entries~w.reads~w.writes~w.time~w.exec~w.csv",
        [Clients, Entries, Reads, Writes, Time, ExecNumber]),

    file:write_file(CSVFile,
            "ClientID, Entries, Reads, Writes, Time, ExecNumber\n"
    ),

    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format(
        "Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
        [Clients, Entries, Reads, Writes, Time]
    ),
    timer:sleep(Time * 1000),
    stop(L, CSVFile).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L, CSVFile),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _) ->
    L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes).

stopClients([], _) ->
    ok;
stopClients([Pid | L], CSVFile) ->
    Pid ! {stop, self(), CSVFile},
    stopClients(L, CSVFile).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
