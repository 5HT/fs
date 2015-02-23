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
    case Backend:find_executable() of
        false -> io:format("Backend port not found: ~p~n\r",[Backend]);
        _ -> ok end,
    Path = fs:path(),
    {ok, { {one_for_one, 5, 10}, [
                ?CHILD(fs_server, worker, [Backend, Path, Path]),
                ?CHILD(gen_event, worker, [{local, fs_events}]) ] } }.
