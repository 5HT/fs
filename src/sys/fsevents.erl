-module(fsevents).
-include("api.hrl").
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

find_executable() ->
    fs:find_executable("mac_listener", "deps/fs/priv/mac_listener").

known_events() ->
    [mustscansubdirs,userdropped,kerneldropped,eventidswrapped,historydone,rootchanged,
        mount,unmount,created,removed,inodemetamod,renamed,modified,finderinfomod,changeowner,
        xattrmod,isfile,isdir,issymlink,ownevent].

start_port(Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, binary, {line, 16384}, {args, ["-F", Path]}, {cd, Cwd}]).

line_to_event(Line0) ->
    Line = unicode:characters_to_list(Line0, utf8),   
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),
    {Path, Flags}.
