-module(fs).
-include_lib("kernel/include/file.hrl").
-export([start_link/1, start_link/2, subscribe/0, subscribe/1, known_events/0, known_events/1,
         start_looper/0, start_looper/1, find_executable/2]).

% sample subscriber

start_link(Name) -> start_link(Name, default_path()).
start_link(Name, Path) ->
    SupName = name(Name, "sup"),
    FileHandler = name(Name, "file"),
    fs_sup:start_link(SupName, Name, FileHandler, Path).

subscribe() -> subscribe(default_fs).
subscribe(Name) -> gen_event:add_sup_handler(Name, {fs_event_bridge, self()}, [self()]).

default_path() ->
    case application:get_env(fs, path) of
        {ok, P} -> filename:absname(P);
        undefined -> filename:absname("") end.

known_events() -> known_events(default_fs).
known_events(Name) -> gen_server:call(name(Name, "file"), known_events).

start_looper() -> start_looper(default_fs).
start_looper(Name) -> spawn(fun() -> subscribe(Name), loop() end).

loop() ->
    receive
        {_Pid, {fs, file_event}, {Path, Flags}} -> error_logger:info_msg("file_event: ~p ~p", [Path, Flags]);
        _ -> ignore end,
    loop().

find_executable(Cmd, DepsPath) ->
    case priv_file(Cmd) of
    false -> mad_file(DepsPath);
    Priv  -> Priv end.

mad_file(DepsPath) ->
    case filelib:is_regular(DepsPath) of
    true  -> DepsPath;
    false ->
        case mad_repl:load_file(DepsPath) of
        {error,_} ->
            %% This path has been already checked in find_executable/2
            false;
        {ok,ETSFile} ->
            filelib:ensure_dir(DepsPath),
            file:write_file(DepsPath, ETSFile),
            file:write_file_info(DepsPath, #file_info{mode=8#00555}) end end.

priv_file(Cmd) ->
    case code:priv_dir(fs) of
    Priv when is_list(Priv) ->
        Path = filename:join(Priv, Cmd),
        case filelib:is_regular(Path) of
        true  -> Path;
        false -> false end;
    _ ->
        false end.

name(Name, Prefix) ->
    NameList = erlang:atom_to_list(Name),
    list_to_atom(NameList ++ Prefix).
