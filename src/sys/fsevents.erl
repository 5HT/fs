-module(fsevents).
-include("api.hrl").
-include_lib("kernel/include/file.hrl").
-export(?API).

read_file(StringPath) ->
    [_,Name|RestPath] = filename:split(StringPath),
    case file:read_file(StringPath) of
    {ok,Bin} -> {local,StringPath,Bin};
    {error,_} ->
        case mad_repl:load_file(StringPath) of
        {error,_} ->
            ReleaseName = filename:join([code:lib_dir(Name)|RestPath]),
            case file:read_file(ReleaseName) of
            {ok,ReleaseFile} -> {release,ReleaseName,ReleaseFile};
            {error,_} -> {absent,"",<<>>} end;
        {ok,ETSFile} -> {ets,StringPath,ETSFile} end end.

find_executable() ->
    StringName = filename:join(code:priv_dir(fs), "mac_listener"),
    case read_file(StringName) of
      {ets,EName,Bin} ->
          filelib:ensure_dir(EName),
          file:write_file(EName,Bin),
          file:write_file_info(EName, #file_info{mode=8#00555}),
          EName;
      {release,RName,_} -> RName;
      {absent,_,_} -> false;
      {local,LName,_} -> LName end.

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
