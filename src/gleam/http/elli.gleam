import gleam/erlang/atom.{type Atom}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/http
import gleam/http/service.{type Service}
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import gleam/bit_builder.{type BitBuilder}

type ElliRequest

type ElliResponse =
  #(Int, List(http.Header), BitBuilder)

type StartLinkOption {
  Callback(Atom)
  CallbackArgs(fn(ElliRequest) -> ElliResponse)
  Port(Int)
}

@external(erlang, "binary", "split")
fn split(a: String, b: List(String)) -> List(String)

@external(erlang, "elli", "start_link")
fn erl_start_link(a: List(StartLinkOption)) -> Result(Pid, Dynamic)

@external(erlang, "elli_request", "body")
fn get_body(a: ElliRequest) -> BitArray

@external(erlang, "elli_request", "headers")
fn get_headers(a: ElliRequest) -> List(http.Header)

@external(erlang, "gleam_elli_native", "get_host")
fn get_host(a: ElliRequest) -> String

@external(erlang, "elli_request", "method")
fn get_dynamic_method(a: ElliRequest) -> Dynamic

fn get_method(req) {
  req
  |> get_dynamic_method
  |> http.method_from_dynamic
  |> result.unwrap(http.Get)
}

@external(erlang, "elli_request", "port")
fn get_dynamic_port(a: ElliRequest) -> Dynamic

fn get_port(req) {
  req
  |> get_dynamic_port
  |> dynamic.int
  |> option.from_result
}

@external(erlang, "elli_request", "scheme")
fn get_dynamic_scheme(a: ElliRequest) -> Dynamic

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

@external(erlang, "elli_request", "query_str")
fn get_query(a: ElliRequest) -> String

@external(erlang, "elli_request", "raw_path")
fn get_raw_path(a: ElliRequest) -> String

fn get_path(request: ElliRequest) -> String {
  let raw_path = get_raw_path(request)
  raw_path
  |> split(["#", "?"])
  |> list.first
  |> result.unwrap(raw_path)
}

@external(erlang, "gleam_elli_native", "await_shutdown")
fn await_shutdown(a: process.Pid) -> Nil

fn convert_header_to_lowercase(header: http.Header) -> http.Header {
  pair.map_first(header, fn(key) { string.lowercase(key) })
}

fn service_to_elli_handler(
  service: Service(BitArray, BitBuilder),
) -> fn(ElliRequest) -> ElliResponse {
  fn(req) {
    let resp =
      Request(
        scheme: get_scheme(req),
        method: get_method(req),
        host: get_host(req),
        port: get_port(req),
        path: get_path(req),
        query: option.Some(get_query(req)),
        headers: get_headers(req)
        |> list.map(convert_header_to_lowercase),
        body: get_body(req),
      )
      |> service
    let Response(status, headers, body) = resp
    #(status, headers, body)
  }
}

/// Start a new Elli web server process which runs concurrently to the current
/// process.
///
/// If you want to run the web server but don't need to do anything else with
/// the current process you may want to use the `become` function instead.
///
pub fn start(
  service: Service(BitArray, BitBuilder),
  on_port number: Int,
) -> Result(Pid, Dynamic) {
  [
    Port(number),
    Callback(atom.create_from_string("gleam_elli_native")),
    CallbackArgs(service_to_elli_handler(service)),
  ]
  |> erl_start_link
}

/// Start an Elli web server with the current process.
///
/// This function returns if the Elli web server fails to start or if it was
/// shut down after successfully starting.
///
pub fn become(
  service: Service(BitArray, BitBuilder),
  on_port number: Int,
) -> Result(Nil, Dynamic) {
  service
  |> start(number)
  |> result.map(await_shutdown)
}
