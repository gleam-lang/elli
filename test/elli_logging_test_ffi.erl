-module(elli_logging_test_ffi).

-include_lib("elli/include/elli.hrl").

%% Test API
-export([
    bad_service/1,
    start_log_spy/1,
    silence_default_handler/0,
    get_spied_reports/1
]).

%% Callbacks
-export([
    log/2,
    spy_loop/1
]).

bad_service(Request) ->
    {request, _, _, _, _, _, _, Path, {some, <<"message=", Message/binary>>}} = Request,
    case Path of
        <<"/throw">> -> throw(Message);
        <<"/error">> -> error(Message);
        <<"/exit">> -> exit(Message)
    end.

%% A spy seemed like the easiest way to test the logging,
%% and implementing in Erlang was simpler than in Gleam.
start_log_spy(IdBinary) ->
    HandlerId = binary_to_atom(IdBinary, utf8),
    register(HandlerId, spawn(?MODULE, spy_loop, [HandlerId])),
    logger:add_handler(HandlerId, ?MODULE, #{}).

%% By default the logger will print stuff to the console,
%% we don't need that in tests.
silence_default_handler() ->
    logger:remove_handler(default).

get_spied_reports(IdBinary)->
    Id = binary_to_atom(IdBinary, utf8),
    Pid = whereis(Id),
    Pid ! {get_log, self()},
    receive
        {log, LogEvents} ->
            lists:filtermap(
                fun
                    (#{ level := Level, msg := {report, Report}}) ->
                        %% massage to make it easier to use in tests
                        {true, {atom_to_binary(Level), Report}};
                    (_) ->
                        false
                end,
                LogEvents
            )
    end.

log(LogEvent, Config) ->
    Id = maps:get(id, Config),
    Pid = whereis(Id),
    Pid ! {log, LogEvent}.

spy_loop(Id) ->
    spy_loop(Id, []).

spy_loop(Id, LogEvents) ->
    receive
        {log, LogEvent} ->
            spy_loop(Id, [LogEvent | LogEvents]);
        {get_log, Pid} ->
            Pid ! {log, lists:reverse(LogEvents)},
            spy_loop(Id, LogEvents)
    end.
