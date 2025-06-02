import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/http
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response, Response}
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string

type ElliRequest

type ElliResponse =
  #(Int, List(http.Header), BytesTree)

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

@external(erlang, "gleam_elli_native", "get_method")
fn get_method(a: ElliRequest) -> http.Method

@external(erlang, "elli_request", "port")
fn get_dynamic_port(a: ElliRequest) -> Dynamic

fn get_port(req) {
  req
  |> get_dynamic_port
  |> decode.run(decode.int)
  |> option.from_result
}

@external(erlang, "elli_request", "scheme")
fn get_dynamic_scheme(a: ElliRequest) -> Dynamic

fn get_scheme(req) -> http.Scheme {
  let scheme =
    req
    |> get_dynamic_scheme
    |> decode.run(decode.string)
    |> result.unwrap("")
    |> string.lowercase
  case scheme {
    "https" -> http.Https
    _ -> http.Http
  }
}

// pub fn method_from_dynamic(
//   value: Dynamic,
// ) -> Result(Method, List(decode.DecodeError)) {
//   case do_method_from_dynamic(value) {
//     Ok(method) -> Ok(method)
//     Error(_) ->
//       Error([decode.DecodeError("HTTP method", dynamic.classify(value), [])])
//   }
// }

// @target(erlang)
// @external(erlang, "gleam_http_native", "decode_method")
// fn do_method_from_dynamic(a: Dynamic) -> Result(ElliMethod, nil)

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
  service: fn(Request(BitArray)) -> Response(BytesTree),
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
  service: fn(Request(BitArray)) -> Response(BytesTree),
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
  service: fn(Request(BitArray)) -> Response(BytesTree),
  on_port number: Int,
) -> Result(Nil, Dynamic) {
  service
  |> start(number)
  |> result.map(await_shutdown)
}
