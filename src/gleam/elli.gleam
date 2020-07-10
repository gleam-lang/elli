import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/otp/process.{StartResult, UnknownMessage}
import gleam/http
import gleam/option
import gleam/result
import gleam/bit_builder.{BitBuilder}
import gleam/bit_string

pub external type ElliRequest

pub type ElliResponse =
  tuple(Int, List(http.Header), BitBuilder)

pub type ElliHandler =
  fn(ElliRequest) -> ElliResponse

type StartLinkOption {
  Callback(Atom)
  CallbackArgs(fn(ElliRequest) -> ElliResponse)
  Port(Int)
}

external fn erl_start_link(
  List(StartLinkOption),
) -> StartResult(UnknownMessage) =
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
  |> http.method_from_erlang
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

external fn get_query(ElliRequest) -> String =
  "elli_request" "query_str"

external fn get_path(ElliRequest) -> String =
  "elli_request" "raw_path"

// TODO: document
// TODO: test
pub fn service_to_elli_handler(
  service: http.Service(BitString, BitBuilder),
) -> ElliHandler {
  fn(req) {
    let resp = http.Request(
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

// TODO: document
// TODO: test
pub fn start(
  service: http.Service(BitString, BitBuilder),
  port: Int,
) -> StartResult(UnknownMessage) {
  erl_start_link(
    [
      Port(port),
      Callback(atom.create_from_string("gleam_elli_native")),
      CallbackArgs(service_to_elli_handler(service)),
    ],
  )
}
