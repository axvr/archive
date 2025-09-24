-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

% File module docs: https://www.erlang.org/doc/apps/kernel/file.html

ls(Server) ->
    Server ! {self(), list_dir},
    receive {Server, FileList} -> FileList end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive {Server, Content} -> Content end.

put_file(Server, File, Bytes) ->
    Server ! {self(), {put_file, File, Bytes}},
    receive {Server, Response} -> {Response, File} end.
