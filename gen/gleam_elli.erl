-module(gleam_elli).
-compile(no_auto_import).

-export([method/1, path/1, raw_path/1, headers/1, body/1, query_string/1]).

erl_query_string(A) ->
    elli_request:query_str(A).

method(A) ->
    gleam_elli_native:method(A).

path(A) ->
    elli_request:path(A).

raw_path(A) ->
    elli_request:raw_path(A).

headers(A) ->
    elli_request:headers(A).

body(A) ->
    elli_request:body(A).

query_string(Req) ->
    case erl_query_string(Req) of
        <<"">> ->
            {error, <<"none">>};

        S ->
            {ok, S}
    end.
