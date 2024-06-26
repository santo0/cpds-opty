-module(opty).
-export([start/5, stop/1, startDistributed/6, stopDistributed/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

startDistributed(Clients, Entries, Reads, Writes, Time, ServerRef) ->
    Main = self(),
    spawn(ServerRef, fun() -> register(s, server:start(Entries)), Main ! registerFinished end),
    receive registerFinished -> io:format("Server started~n", []) end,
    L = startClientsDistributed(Clients, [], Entries, Reads, Writes, ServerRef),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", 
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stopDistributed(L, ServerRef).

stopDistributed(L, ServerRef) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    {s, ServerRef} ! stop,
    io:format("Stopped~n").

startClientsDistributed(0, L, _, _, _, _) -> L;
startClientsDistributed(Clients, L, Entries, Reads, Writes, ServerRef) ->
    Pid = client:start(Clients, Entries, Reads, Writes, {s, ServerRef}),
    startClientsDistributed(Clients-1, [Pid|L], Entries, Reads, Writes, ServerRef).



start(Clients, Entries, Reads, Writes, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format(
        "Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
        [Clients, Entries, Reads, Writes, Time]
    ),
    timer:sleep(Time * 1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _) ->
    L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes).

stopClients([]) ->
    ok;
stopClients([Pid | L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
