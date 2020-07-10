-module(gleam_elli_native).

-export([handle/2, handle_event/3]).

handle(Req, Handler) ->
    Handler(Req).

handle_event(_, _, _) ->
    ok.
