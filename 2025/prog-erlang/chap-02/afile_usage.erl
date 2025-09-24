FileSrv = afile_server:start("."):

Files = FileSrv ! {self(), list_dir}.
receive X -> X end.  % We need to await and process the resulting message.

File = FileSrv ! {self(), {get_file, "afile_server.erl"}}.

afile_client:ls(FileSrv).
afile_client:get_file(FileSrv, "afile_server.erl").

afile_client:put_file(FileSrv, "my_file.txt", "Hello, World!").
