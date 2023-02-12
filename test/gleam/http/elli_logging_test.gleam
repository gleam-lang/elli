import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam/erlang/atom.{Atom}
import gleam/hackney
import gleam/http.{Get, Method, Post, Put}
import gleam/http/elli
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/list
import gleam/map.{Map}
import gleam/result
import gleeunit/should

// Using FFI to make crashing in the request handler easy.
external fn bad_service(request: Request(BitString)) -> Response(BitBuilder) =
  "elli_logging_test_ffi" "bad_service"

pub fn log_throw_test() {
  let port = 4712
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_throw_test"
  start_log_spy(spy_name)
  silence_default_handler()

  make_request(port, Get, "/throw", "throw_value")
  |> hackney.send

  assert [#(level, throw)] = get_spied_reports(spy_name)

  assert "error" = level
  assert Ok("request handler threw an exception") = get_string(throw, Message)
  assert Ok("throw_value") = get_string(throw, Error)
  should.equal(get_method(throw, Method), atom_from_string("GET"))
  assert Ok("/throw") = get_string(throw, Path)
  assert Ok(throw_stack) = list_length(throw, Stacktrace)
  should.be_true(0 < throw_stack)
}

pub fn log_error_test() {
  let port = 4713
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_error_test"
  start_log_spy(spy_name)
  silence_default_handler()

  make_request(port, Post, "/error", "error_value")
  |> hackney.send

  assert [#(level, err)] = get_spied_reports(spy_name)

  assert "error" = level
  assert Ok("request handler had a runtime error") = get_string(err, Message)
  assert Ok("error_value") = get_string(err, Error)
  should.equal(get_method(err, Method), atom_from_string("POST"))
  assert Ok("/error") = get_string(err, Path)
  assert Ok(err_stack) = list_length(err, Stacktrace)
  should.be_true(0 < err_stack)
}

pub fn log_exit_test() {
  let port = 4714
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_exit_test"
  start_log_spy(spy_name)
  silence_default_handler()

  make_request(port, Put, "/exit", "exit_value")
  |> hackney.send

  assert [#(level, exit)] = get_spied_reports(spy_name)

  assert "error" = level
  assert Ok("request handler exited") = get_string(exit, Message)
  assert Ok("exit_value") = get_string(exit, Error)
  should.equal(get_method(exit, Method), atom_from_string("PUT"))
  assert Ok("/exit") = get_string(exit, Path)
  assert Ok(exit_stack) = list_length(exit, Stacktrace)
  should.be_true(0 < exit_stack)
}

external fn start_log_spy(id: String) -> Nil =
  "elli_logging_test_ffi" "start_log_spy"

external fn silence_default_handler() -> Nil =
  "elli_logging_test_ffi" "silence_default_handler"

type ReportKey {
  Message
  Error
  Method
  Path
  Stacktrace
}

external fn get_spied_reports(
  id: String,
) -> List(#(String, Map(ReportKey, Dynamic))) =
  "elli_logging_test_ffi" "get_spied_reports"

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
  report: Map(a, Dynamic),
  key: a,
) -> Result(String, List(DecodeError)) {
  map.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.string)
}

fn list_length(
  report: Map(a, Dynamic),
  key: a,
) -> Result(Int, List(DecodeError)) {
  map.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.shallow_list)
  |> result.map(list.length)
}

fn get_method(
  report: Map(a, Dynamic),
  key: a,
) -> Result(Atom, List(DecodeError)) {
  map.get(report, key)
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
