import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid}
import gleam/otp/actor.{StartResult}
import gleam/otp/supervisor
import gleam/http
import gleam/option
import gleam/result
import gleam/string
import gleam/bit_builder.{BitBuilder}
import gleam/bit_string

external type ElliRequest

type ElliResponse =
  tuple(Int, List(http.Header), BitBuilder)

type StartLinkOption {
  Callback(Atom)
  CallbackArgs(fn(ElliRequest) -> ElliResponse)
  Port(Int)
}

external fn erl_start_link(List(StartLinkOption)) -> Result(Pid, Dynamic) =
  "elli" "start_link"

external fn get_body(ElliRequest) -> BitString =
  "elli_request" "body"

external fn get_headers(ElliRequest) -> List(http.Header) =
  "elli_request" "headers"

external fn get_host(ElliRequest) -> String =
  "elli_request" "host"

external fn get_dynamic_method(ElliRequest) -> Dynamic =
  "elli_request" "method"

fn get_method(req) {
  req
  |> get_dynamic_method
  |> http.method_from_dynamic
  |> result.unwrap(http.Get)
}

external fn get_dynamic_port(ElliRequest) -> Dynamic =
  "elli_request" "port"

fn get_port(req) {
  req
  |> get_dynamic_port
  |> dynamic.int
  |> option.from_result
}

external fn get_dynamic_scheme(ElliRequest) -> Dynamic =
  "elli_request" "scheme"

fn get_scheme(req) -> http.Scheme {
  let scheme =
    req
    |> get_dynamic_scheme
    |> dynamic.string
    |> result.unwrap("")
    |> string.lowercase
  case scheme {
    "https" -> http.Https
    _ -> http.Http
  }
}

external fn get_query(ElliRequest) -> String =
  "elli_request" "query_str"

external fn get_path(ElliRequest) -> String =
  "elli_request" "raw_path"

// TODO: document
// TODO: test
fn service_to_elli_handler(
  service: http.Service(BitString, BitBuilder),
) -> fn(ElliRequest) -> ElliResponse {
  fn(req) {
    let resp =
      http.Request(
        scheme: get_scheme(req),
        method: get_method(req),
        host: get_host(req),
        port: get_port(req),
        path: get_path(req),
        query: option.Some(get_query(req)),
        headers: get_headers(req),
        body: get_body(req),
      )
      |> service
    let http.Response(status, headers, body) = resp
    tuple(status, headers, body)
  }
}

// TODO: refine error type
// TODO: document
// TODO: test
pub fn start(
  service: http.Service(BitString, BitBuilder),
  on_port number: Int,
) -> StartResult(a) {
  [
    Port(number),
    Callback(atom.create_from_string("gleam_elli_native")),
    CallbackArgs(service_to_elli_handler(service)),
  ]
  |> erl_start_link
  |> supervisor.from_erlang_start_result
}
