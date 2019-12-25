import gleam/http
import gleam/dynamic
import gleam/result
import gleam/iodata

// The Elli request object. Contains all information about the
// incoming HTTP request.
//
pub external type Request;

// TODO: behaviour wrapper
// pub type Response {
//   Response(
//     status: Int,
//     headers: List(tuple(String, String)),
//     body: iodata.Iodata,
//   )
// }

// Get the query string for the request. Returns `Error` string if
// request has no query.
//
pub external fn query_string(Request) -> String
  = "elli_request" "query_str";

external fn get_method(Request) -> dynamic.Dynamic
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
