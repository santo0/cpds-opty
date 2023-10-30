-module(client).
-export([start/6]).

start(ClientID, Entries, Reads, Writes, Server, CSVFile) ->
    file:write_file(CSVFile, "\n"),
    spawn(fun() -> open(ClientID, Entries, Reads, Writes, Server, 0, 0, CSVFile) end).

open(ClientID, Entries, Reads, Writes, Server, Total, Ok, CSVFile) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
            Percentage = 100 * Ok / Total,
            io:format(
                "~w: Transactions PERCENTAGE:  ~w   TOTAL:~w, OK:~w, -> ~w   % ~n",
                [ClientID, Percentage, Total, Ok, Percentage]
            ),
            file:write_file(CSVFile,
                io_lib:fwrite(
                    "~w,",
                    [Percentage]),
                    [append]
            ),
    
            From ! {done, self(), Total, Ok, 100 * Ok / Total},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            case do_transaction(ClientID, Entries, Reads, Writes, Handler) of
                ok ->
                    open(ClientID, Entries, Reads, Writes, Server, Total + 1, Ok + 1, CSVFile);
                abort ->
                    open(ClientID, Entries, Reads, Writes, Server, Total + 1, Ok, CSVFile)
            end
    end.

do_transaction(_, _, 0, 0, Handler) ->
    do_commit(Handler);
do_transaction(ClientID, Entries, 0, Writes, Handler) ->
    do_write(Entries, Handler, ClientID),
    do_transaction(ClientID, Entries, 0, Writes - 1, Handler);
do_transaction(ClientID, Entries, Reads, 0, Handler) ->
    do_read(Entries, Handler),
    do_transaction(ClientID, Entries, Reads - 1, 0, Handler);
do_transaction(ClientID, Entries, Reads, Writes, Handler) ->
    Op = rand:uniform(),
    if
        Op >= 0.5 ->
            do_read(Entries, Handler),
            do_transaction(ClientID, Entries, Reads - 1, Writes, Handler);
        true ->
            do_write(Entries, Handler, ClientID),
            do_transaction(ClientID, Entries, Reads, Writes - 1, Handler)
    end.

do_read(Entries, Handler) ->
    Ref = make_ref(),
    Num = lists:nth(rand:uniform(length(Entries)), Entries),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, Value} -> Value
    end.

do_write(Entries, Handler, Value) ->
    Num = lists:nth(rand:uniform(length(Entries)), Entries),
    Handler ! {write, Num, Value}.

do_commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, Value} -> Value
    end.
