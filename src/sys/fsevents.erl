-module(fsevents).
-include("api.hrl").
-include_lib("kernel/include/file.hrl").
-export(?API).

mad_file(StringPath) ->
    case filelib:exists(StringPath) of
    true  -> StringPath;
    false ->
        case mad_repl:load_file(StringPath) of
        {error,_} ->
            %% This path has been already checked in find_executable/0
            false;
        {ok,ETSFile} ->
            filelib:ensure_dir(StringPath),
            file:write_file(StringPath, ETSFile),
            file:write_file_info(StringPath, #file_info{mode=8#00555}) end end.

find_executable() ->
    Priv = filename:join(code:priv_dir(fs), "mac_listener"),
    case filelib:is_regular(Priv) of
    true -> Priv;
    false -> mad_file("deps/fs/priv/mac_listener") end.

known_events() ->
    [mustscansubdirs,userdropped,kerneldropped,eventidswrapped,historydone,rootchanged,
        mount,unmount,created,removed,inodemetamod,renamed,modified,finderinfomod,changeowner,
        xattrmod,isfile,isdir,issymlink,ownevent].

start_port(Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, ["-F", Path]}, {cd, Cwd}]).

line_to_event(Line) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),
    {Path, Flags}.
