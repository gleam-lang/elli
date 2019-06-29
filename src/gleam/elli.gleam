pub enum Method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Other(String)

// The Elli request object. Contains all information about the
// incoming HTTP request.
//
pub external type Request;

// The status code, headers and body to send back to the client.
//
// pub type Response =
//   {Int, List({String, String}), String};

// pub type Server(arg, r) =
//   module { r |
//     fn handle(Request, arg) -> Response;

//     fn handle_event(any:Any, any:Any, any:Any) -> Atom;
//   };

// API

external fn erl_query_string(Request) -> String
  = "elli_request" "query_str";

// external fn erl_start_link(List((Atom, Any))) -> Result(Any, Pid)
//   = "elli" "start_link";

// Get the request HTTP method.
//
pub external fn method(Request) -> Method
  = "gleam_elli_native" "method";

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
pub external fn headers(Request) -> List({String, String})
  = "elli_request" "headers";

// Get the request body.
//
pub external fn body(Request) -> String
  = "elli_request" "body";

// Get the query string for the request. Returns `Error` string if
// request has no query.
//
pub fn query_string(req) {
  case erl_query_string(req) {
  | "" -> Error("none")
  | s -> Ok(s)
  }
}

pub external fn uri_decode(String) -> String = "elli_request" "uri_decode";

// pub type Options =
//   {
//     port = Int,
//     ip = Tuple(Int, Int, Int, Int),
//   }

// pub const default_options: Options =
//   {
//     port = 8080,
//     ip = {0, 0, 0, 0},
//   }

// external fn erl_start_link(List(Tuple(Atom, Any))) -> gen_server:StartResult
//   = "elli" "start_link";

// // Start the Elli web server process tree.
// pub fn start_link(mod: Server(arg, _), arg: arg, options: Options) {
//   { options |
//     callback = mod,
//     callback_args = arg,
//   }
//     |> map:from_record
//     |> map:to_list
//     |> erl_start_link
// }
