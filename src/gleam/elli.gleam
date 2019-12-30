import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid, UnknownMessage}
import gleam/http
import gleam/result

// The Elli request object. Contains all information about the
// incoming HTTP request.
//
pub external type Request;

pub type Response {
  Response(
    status: Int,
    headers: List(tuple(String, String)),
    body: String,
  )
}

// Get the query string for the request. Returns `Error` string if
// request has no query.
//
pub external fn query_string(Request) -> String
  = "elli_request" "query_str";

external fn get_method(Request) -> Dynamic
  = "elli_request" "method";

// Get the request HTTP method.
//
pub fn method(request: Request) -> http.Method {
  request
  |> get_method
  |> http.method_from_erlang
  |> result.unwrap(_, http.Get)
}

// Get the request path segments.
//
pub external fn path(Request) -> List(String)
  = "elli_request" "path";

// Get the request `raw_path", i.e. not split or parsed for query params.
//
pub external fn raw_path(Request) -> String
  = "elli_request" "raw_path";

// Get the request headers.
//
pub external fn headers(Request) -> List(tuple(String, String))
  = "elli_request" "headers";

// Get the request body.
//
pub external fn body(Request) -> String
  = "elli_request" "body";

pub external fn uri_decode(String) -> String = "elli_request" "uri_decode";

type Option {
  Callback(Atom)
  CallbackArg(fn(Request) -> Response)
  Port(Int)
};

external fn erl_start_link(List(Option)) -> Result(Pid(UnknownMessage), String)
  = "elli" "start_link";

pub fn start_link(port: Int, handler: fn(Request) -> Response)
  -> Result(Pid(UnknownMessage), String)
{
  erl_start_link([
    Port(port),
    Callback(atom.create_from_string("gleam@elli")),
    CallbackArg(handler),
  ])
}

// Elli behaviour callbacks

// nodoc
pub fn handle(
  request: Request,
  handler: fn(Request) -> Response,
) -> tuple(Int, List(tuple(String, String)), String)
{
  let Response(status, headers, body) = handler(request)
  tuple(status, headers, body)
}

// nodoc
pub fn handle_event(_event: Dynamic, _data: Dynamic, _args: Dynamic) -> Atom {
  atom.create_from_string("ok")
}
