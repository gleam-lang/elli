import gleam/bit_builder.{BitBuilder}
import gleam/dynamic.{DecodeError, Dynamic}
import gleam/hackney
import gleam/http.{Get}
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

  make_request(port, "/throw", "throw_value")
  |> hackney.send

  assert [throw] = get_spied_reports(spy_name)

  assert Ok("error") = get_string(throw, Level)
  assert Ok("request handler threw an exception") = get_string(throw, Message)
  assert Ok("throw_value") = get_string(throw, Error)
  assert Ok(throw_stack) = list_length(throw, Stacktrace)
  should.be_true(0 < throw_stack)
}

pub fn log_error_test() {
  let port = 4713
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_error_test"
  start_log_spy(spy_name)
  silence_default_handler()

  make_request(port, "/error", "error_value")
  |> hackney.send

  assert [err] = get_spied_reports(spy_name)

  assert Ok("error") = get_string(err, Level)
  assert Ok("request handler had a runtime error") = get_string(err, Message)
  assert Ok("error_value") = get_string(err, Error)
  assert Ok(err_stack) = list_length(err, Stacktrace)
  should.be_true(0 < err_stack)
}

pub fn log_exit_test() {
  let port = 4714
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_exit_test"
  start_log_spy(spy_name)
  silence_default_handler()

  make_request(port, "/exit", "exit_value")
  |> hackney.send

  assert [exit] = get_spied_reports(spy_name)

  assert Ok("error") = get_string(exit, Level)
  assert Ok("request handler exited") = get_string(exit, Message)
  assert Ok("exit_value") = get_string(exit, Error)
  assert Ok(exit_stack) = list_length(exit, Stacktrace)
  should.be_true(0 < exit_stack)
}

external fn start_log_spy(id: String) -> Nil =
  "elli_logging_test_ffi" "start_log_spy"

external fn silence_default_handler() -> Nil =
  "elli_logging_test_ffi" "silence_default_handler"

type ReportKey {
  Level
  Message
  Error
  Request
  Stacktrace
}

external fn get_spied_reports(id: String) -> List(Map(ReportKey, Dynamic)) =
  "elli_logging_test_ffi" "get_spied_reports"

fn make_request(port: Int, path: String, message: String) {
  request.new()
  |> request.set_method(Get)
  |> request.set_path(path)
  |> request.set_query([#("message", message)])
  |> request.set_host("0.0.0.0")
  |> request.set_scheme(http.Http)
  |> request.set_port(port)
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
