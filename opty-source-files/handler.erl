-module(handler).
-export([start/3]).

%% Initialize the Handler, which is a middleware process between
%% Clients and the data Store
start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

%% The handler process listens for messages from the Client, and
%% performs the relevant actions on the Validator and Store, and
%% answers Client messages.
handler(Client, Validator, Store, Reads, Writes) ->         
    receive
	%% Client request to read a value.
        {read, Ref, N} ->
	    %% The value may be in the write set, if it has been write previously.
            case lists:keyfind(N, 1, Writes) of  %% TODO: not tested
                %% If it is, then answer the client with it.
                {N, _, Value} ->
                    Client ! {value, Ref, Value}, %% TODO: not tested
                    handler(Client, Validator, Store, Reads, Writes);
                %% If it is not, then ask the Store for it.
                false ->
                    Entry = store:lookup(N, Store), %% TODO: not tested
                    Entry ! {read, Ref, self()},    %% TODO: not tested
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        %% NOT A CLIENT REQUEST, but an entry (of the store) answer
        %% with the value read: send it back to the client, and update
        %% the read set.
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value}, %% TODO: not tested
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes);
        %% Client request to write a value. No answer done in this
	%% case. Update the store and the write set.
        {write, N, Value} -> 
            %% Begin: Added by Vincent %%
            Entry = store:lookup(N, Store), %% TODO: not tested
            Added = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Reads, Added);
	%% Client request to commit the transaction. Triggers the
	%% validation of the transaction.
        {commit, Ref} ->
            Validator !  {validate, Ref, Reads, Writes, Client}; %% TODO: not tested
        abort ->
            ok
    end.
