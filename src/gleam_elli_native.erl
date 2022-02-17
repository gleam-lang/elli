-module(gleam_elli_native).

-export([handle/2, handle_event/3, await_shutdown/1, get_host/1]).

handle(Req, Handler) ->
    Handler(Req).

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
