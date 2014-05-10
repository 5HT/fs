-module(fs).
-compile(export_all).

% sample subscriber

subscribe() -> gen_event:add_sup_handler(fs_events, {fs_event_bridge, self()}, [self()]).
known_events() -> gen_server:call(fs_server, known_events).
start_looper() -> spawn(fun() -> subscribe(), loop() end).

path() ->
    case application:get_env(fs, path) of
        {ok, P} -> filename:absname(P);
        undefined -> filename:absname("") end.

loop() ->
    receive
        {_Pid, {fs, file_event}, {Path, Flags}} -> error_logger:info_msg("file_event: ~p ~p", [Path, Flags]);
        _ -> ignore end,
    loop().