-module(afile_server).
-export([start/1, loop/1]).

% Starts the process.
start(Dir) -> spawn(afile_server, loop, [Dir]).

% Listens for a message (uses pattern matching to determine
% what to do) and responds to the client.  Finally reinvokes
% itself.
loop(Dir) ->
    % Awaits a message.
    receive
        % The message is "list_dir".
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        % The message is "get_file" and the name of a file.
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)};
        {Client, {put_file, File, Bytes}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:write_file(Full, Bytes)}
    end,
    loop(Dir).  % Erlang performs TCO.

% A function calling itself in Erlang is the standard way to
% create a loop.

% In Erlang, a server is just a program which services requests
% in an infinite loop.
