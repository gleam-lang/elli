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

pub fn log_errors_test() {
  let port = 4711
  assert Ok(_) = elli.start(bad_service, on_port: port)

  let spy_name = "log_errors_test"
  start_log_spy(spy_name)
  silence_default_handler()

  list.each(
    [
      make_request(port, "/throw", "throw_value"),
      make_request(port, "/error", "error_value"),
      make_request(port, "/exit", "exit_value"),
    ],
    fn(req) {
      assert Ok(resp) = hackney.send(req)
      assert 500 = resp.status
      assert "Internal server error" = resp.body
    },
  )

  assert [throw, err, exit] = get_spied_reports(spy_name)

  assert Ok("error") = get_string(throw, "level")
  assert Ok("request handler threw an exception") = get_string(throw, "message")
  assert Ok("throw_value") = get_string(throw, "error")
  assert Ok(throw_stack) = list_length(exit, "stacktrace")
  should.be_true(0 < throw_stack)

  assert Ok("error") = get_string(err, "level")
  assert Ok("request handler had a runtime error") = get_string(err, "message")
  assert Ok("error_value") = get_string(err, "error")
  assert Ok(err_stack) = list_length(exit, "stacktrace")
  should.be_true(0 < err_stack)

  assert Ok("error") = get_string(exit, "level")
  assert Ok("request handler exited") = get_string(exit, "message")
  assert Ok("exit_value") = get_string(exit, "error")
  assert Ok(exit_stack) = list_length(exit, "stacktrace")
  should.be_true(0 < exit_stack)
}

external fn start_log_spy(id: String) -> Nil =
  "elli_logging_test_ffi" "start_log_spy"

external fn silence_default_handler() -> Nil =
  "elli_logging_test_ffi" "silence_default_handler"

external fn get_spied_reports(id: String) -> List(Map(String, Dynamic)) =
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
  report: Map(String, Dynamic),
  key: String,
) -> Result(String, List(DecodeError)) {
  map.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.string)
}

fn list_length(
  report: Map(String, Dynamic),
  key: String,
) -> Result(Int, List(DecodeError)) {
  map.get(report, key)
  |> result.map_error(fn(_) { [] })
  |> result.then(dynamic.shallow_list)
  |> result.map(list.length)
}
