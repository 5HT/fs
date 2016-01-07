-module(fs_sup).
-behaviour(supervisor).
-export([start_link/4]).
-export([init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(SupName, EventHandler, FileHandler, Path) ->
    supervisor:start_link({local, SupName}, ?MODULE, [EventHandler, FileHandler, Path]).
init([EventHandler, FileHandler, Path]) ->
    Backend = case os:type() of
        {unix, darwin} -> fsevents;
        {unix, linux} -> inotifywait;
        {unix, _} -> kqueue;
        {win32, nt} -> inotifywait_win32;
        _ -> undefined end,

    Children = case has_executable(Backend) of
        false -> [];
        true  -> [?CHILD(fs_server, worker, [FileHandler, EventHandler, Backend, Path, Path])] end,

    {ok, { {one_for_one, 5, 10},
        Children ++ [?CHILD(gen_event, worker, [{local, EventHandler}])]} }.

has_executable(undefined) ->
    os_not_supported(), false;
has_executable(Backend) ->
    case Backend:find_executable() of
        false -> backend_port_not_found(Backend), false;
        _     -> true end.

os_not_supported() ->
    error_logger:error_msg("fs does not support the current operating system~n",[]).

backend_port_not_found(Backend) ->
    error_logger:error_msg("backend port not found: ~p~n",[Backend]).
