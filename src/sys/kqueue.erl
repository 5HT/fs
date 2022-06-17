-module(kqueue).
-include("api.hrl").
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

known_events() -> [created, deleted, renamed, closed, modified, isdir, undefined].
line_to_event(Line) ->
    io:format("Line: ~p~n",[Line]),
    {".",Line}.
find_executable() -> fs:find_executable("kqueue", "deps/fs/priv/kqueue").
start_port(Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, [Path]}, {cd, Cwd}]).
