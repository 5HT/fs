-module(fs_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) ->
    Backend = case os:type() of
        {unix, darwin} -> fsevents;
        {unix, linux} -> inotifywait;
        {win32, nt} -> inotifywait_win32;
        _ -> throw(os_not_supported) end,

    Children = case Backend:find_executable() of
        false -> error_logger:error_msg("Backend port not found: ~p~n",[Backend]), [];
        _ -> Path = fs:path(), [?CHILD(fs_server, worker, [Backend, Path, Path])] end,

    {ok, { {one_for_one, 5, 10},
        Children ++ [?CHILD(gen_event, worker, [{local, fs_events}])]} }.
