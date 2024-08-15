import gleam/bytes_builder.{type BytesBuilder}
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/hackney
import gleam/http.{type Method, Get, Post, Put}
import gleam/http/elli
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/list
import gleam/result
import gleeunit/should

// Using FFI to make crashing in the request handler easy.
@external(erlang, "elli_logging_test_ffi", "bad_service")
fn bad_service(request request: Request(BitArray)) -> Response(BytesBuilder)

pub fn log_throw_test() {
  let port = 4712
  let assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_throw_test"
  start_log_spy(spy_name)
  silence_default_handler()

  let _ =
    make_request(port, Get, "/throw", "throw_value")
    |> hackney.send

  let assert [#(level, throw)] = get_spied_reports(spy_name)

  let assert "error" = level
  let assert Ok("request handler threw an exception") =
    get_string(throw, Message)
  let assert Ok("throw_value") = get_string(throw, Error)
  should.equal(get_method(throw, Method), atom_from_string("GET"))
  let assert Ok("/throw") = get_string(throw, Path)
  let assert Ok(throw_stack) = list_length(throw, Stacktrace)
  should.be_true(0 < throw_stack)
}

pub fn log_error_test() {
  let port = 4713
  let assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_error_test"
  start_log_spy(spy_name)
  silence_default_handler()

  let _ =
    make_request(port, Post, "/error", "error_value")
    |> hackney.send

  let assert [#(level, err)] = get_spied_reports(spy_name)

  let assert "error" = level
  let assert Ok("request handler had a runtime error") =
    get_string(err, Message)
  let assert Ok("error_value") = get_string(err, Error)
  should.equal(get_method(err, Method), atom_from_string("POST"))
  let assert Ok("/error") = get_string(err, Path)
  let assert Ok(err_stack) = list_length(err, Stacktrace)
  should.be_true(0 < err_stack)
}

pub fn log_exit_test() {
  let port = 4714
  let assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_exit_test"
  start_log_spy(spy_name)
  silence_default_handler()

  let _ =
    make_request(port, Put, "/exit", "exit_value")
    |> hackney.send

  let assert [#(level, exit)] = get_spied_reports(spy_name)

  let assert "error" = level
  let assert Ok("request handler exited") = get_string(exit, Message)
  let assert Ok("exit_value") = get_string(exit, Error)
  should.equal(get_method(exit, Method), atom_from_string("PUT"))
  let assert Ok("/exit") = get_string(exit, Path)
  let assert Ok(exit_stack) = list_length(exit, Stacktrace)
  should.be_true(0 < exit_stack)
}

@external(erlang, "elli_logging_test_ffi", "start_log_spy")
fn start_log_spy(id id: String) -> Nil

@external(erlang, "elli_logging_test_ffi", "silence_default_handler")
fn silence_default_handler() -> Nil

type ReportKey {
  Message
  Error
  Method
  Path
  Stacktrace
}

@external(erlang, "elli_logging_test_ffi", "get_spied_reports")
fn get_spied_reports(id id: String) -> List(#(String, Dict(ReportKey, Dynamic)))

fn make_request(port: Int, method: Method, path: String, message: String) {
  request.new()
  |> request.set_method(method)
  |> request.set_path(path)
  |> request.set_query([#("message", message)])
  |> request.set_host("0.0.0.0")
  |> request.set_scheme(http.Http)
  |> request.set_port(port)
  |> request.set_body("SECRET DATA")
}

fn get_string(
  report: Dict(a, Dynamic),
  key: a,
) -> Result(String, List(DecodeError)) {
  dict.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.string)
}

fn list_length(
  report: Dict(a, Dynamic),
  key: a,
) -> Result(Int, List(DecodeError)) {
  dict.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.shallow_list)
  |> result.map(list.length)
}

fn get_method(
  report: Dict(a, Dynamic),
  key: a,
) -> Result(Atom, List(DecodeError)) {
  dict.get(report, key)
  |> result.map_error(fn(_) { [] })
  // This only covers the methods we use in the tests,
  // notably not the binaries we get for unknown methods.
  |> result.then(atom.from_dynamic)
}

// a convenience wrapper for the test comparisons
fn atom_from_string(s: String) -> Result(Atom, List(DecodeError)) {
  atom.from_string(s)
  |> result.map_error(fn(_) { [] })
}
