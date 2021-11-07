-module(fs_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case application:get_env(fs, backwards_compatible) of
        {ok, false} -> {ok, self()};
        {ok, true} -> fs:start_link(default_fs)
    end.

stop(_State) -> ok.
