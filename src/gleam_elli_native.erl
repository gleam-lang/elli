-module(gleam_elli_native).

-export([handle/2, handle_event/3, await_shutdown/1]).

handle(Req, Handler) ->
    Handler(Req).

handle_event(_, _, _) ->
    ok.

await_shutdown(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _, _} -> nil
    end.
