-module(inotifywait_win32).
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
    StringName = "deps/fs/priv/inotifywait.exe",
    case read_file(StringName) of
      {ets,EName,Bin} ->
          filelib:ensure_dir(EName),
          file:write_file(EName,Bin),
          file:write_file_info(EName, #file_info{mode=8#00555}),
          EName;
      {release,RName,_} -> RName;
      {absent,_,_} -> false;
      {local,LName,_} -> LName end.


known_events() -> [created, modified, removed, renamed, undefined].

start_port(Path, Cwd) ->
    Path1 = filename:absname(Path),
    Args = ["-m", "-r", Path1],
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, Args}, {cd, Cwd}]).

line_to_event(Line) ->
    {match, [Dir, Flags1, DirEntry]} = re:run(Line, re(), [{capture, all_but_first, list}]),
    Flags = [convert_flag(F) || F <- string:tokens(Flags1, ",")],
    Path = filename:join(Dir,DirEntry),
    {Path, Flags}.

    
convert_flag("CREATE") -> created;
convert_flag("MODIFY") -> modified;
convert_flag("DELETE") -> removed;
convert_flag("MOVE") -> renamed;
convert_flag(_) -> undefined.

re() ->
    case get(inotifywait_re) of
        undefined ->
            {ok, R} = re:compile("^(.*\\\\.*) ([A-Z_,]+) (.*)$", [unicode]),
            put(inotifywait_re, R),
            R;
        V -> V
    end.

