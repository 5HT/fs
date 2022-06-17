-module(fs_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/5]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {event_handler, port, path, backend, cwd, crashes}).

notify(EventHandler, file_event = A, Msg) ->
    Key = {fs, A},
    gen_event:notify(EventHandler, {self(), Key, Msg}).

start_link(Name, EventHandler, Backend, Path, Cwd) ->
    gen_server:start_link({local, Name},
                          ?MODULE,
                          [EventHandler, Backend, Path, Cwd],
                          []).

init([EventHandler, Backend, Path, Cwd]) ->
    {ok,
     #state{event_handler = EventHandler,
            port = Backend:start_port(Path, Cwd), path = Path,
            backend = Backend,cwd=Cwd,crashes=0}}.

handle_call(known_events, _From,
            #state{backend = Backend} = State) ->
    {reply, Backend:known_events(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({_Port, {data, {eol, Line}}},
            #state{event_handler = EventHandler,
                   backend = Backend} =
                State) ->
    Event = Backend:line_to_event(Line),
    notify(EventHandler, file_event, Event),
    {noreply, State};
handle_info({_Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n",
                           [?SERVER, Line]),
    {noreply, State};

handle_info({_Port, {exit_status, Status}}, #state{path=Path,backend=Backend,cwd=Cwd,crashes=Crashes} = State) ->
    error_logger:error_msg("~p port_exit ~p, retry ~p~n", [?SERVER, Status, Crashes]),
    timer:sleep(100*(Crashes+1)*(Crashes+1)),
    {noreply, State#state{port=Backend:start_port(Path, Cwd),crashes=Crashes+1}};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
