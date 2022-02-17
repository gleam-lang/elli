import gleam/erlang/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid}
import gleam/otp/actor.{StartResult}
import gleam/otp/supervisor
import gleam/http
import gleam/http/service.{Service}
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import gleam/bit_builder.{BitBuilder}
import gleam/bit_string

external type ElliRequest

type ElliResponse =
  #(Int, List(http.Header), BitBuilder)

type StartLinkOption {
  Callback(Atom)
  CallbackArgs(fn(ElliRequest) -> ElliResponse)
  Port(Int)
}

external fn split(String, List(String)) -> List(String) =
  "binary" "split"

external fn erl_start_link(List(StartLinkOption)) -> Result(Pid, Dynamic) =
  "elli" "start_link"

external fn get_body(ElliRequest) -> BitString =
  "elli_request" "body"

external fn get_headers(ElliRequest) -> List(http.Header) =
  "elli_request" "headers"

external fn get_host(ElliRequest) -> String =
  "gleam_elli_native" "get_host"

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

external fn get_raw_path(ElliRequest) -> String =
  "elli_request" "raw_path"

fn get_path(request: ElliRequest) -> String {
  let raw_path = get_raw_path(request)
  raw_path
  |> split(["#", "?"])
  |> list.first
  |> result.unwrap(raw_path)
}

external fn await_shutdown(process.Pid) -> Nil =
  "gleam_elli_native" "await_shutdown"

fn convert_header_to_lowercase(header: http.Header) -> http.Header {
  pair.map_first(header, fn(key) { string.lowercase(key) })
}

fn service_to_elli_handler(
  service: Service(BitString, BitBuilder),
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
  service: Service(BitString, BitBuilder),
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

/// Start an Elli web server with the current process.
///
/// This function returns if the Elli web server fails to start or if it was
/// shut down after successfully starting.
///
pub fn become(
  service: Service(BitString, BitBuilder),
  on_port number: Int,
) -> Result(Nil, actor.StartError) {
  case start(service, number) {
    Ok(sender) -> {
      let pid = process.pid(sender)
      Ok(await_shutdown(pid))
    }
    Error(e) -> Error(e)
  }
}
