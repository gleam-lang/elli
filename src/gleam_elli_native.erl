-module(gleam_elli_native).

-include_lib("kernel/include/logger.hrl").

-export([handle/2, handle_event/3, await_shutdown/1, get_host/1]).

handle(Req, Handler) ->
    Handler(Req).

handle_event(request_error, [Request, Error, Stacktrace], _) ->
    ?LOG_ERROR(#{
        <<"message">> => <<"request handler had a runtime error">>,
        <<"error">> => Error,
        <<"request">> => Request,
        <<"stacktrace">> => Stacktrace
    });
handle_event(request_throw, [Request, Exception, Stacktrace], _) ->
    ?LOG_ERROR(#{
        <<"message">> => <<"request handler threw an exception">>,
        <<"error">> => Exception,
        <<"request">> => Request,
        <<"stacktrace">> => Stacktrace
    });
handle_event(request_exit, [Request, Exit, Stacktrace], _) ->
    ?LOG_ERROR(#{
        <<"message">> => <<"request handler exited">>,
        <<"error">> => Exit,
        <<"request">> => Request,
        <<"stacktrace">> => Stacktrace
    });
handle_event(_, _, _) ->
    ok.

await_shutdown(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _, _} -> nil
    end.

get_host(Request) ->
    case elli_request:host(Request) of
        undefined -> <<>>;
        Host when is_binary(Host) -> Host
    end.
