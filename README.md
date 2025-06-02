# Elli

A basic Gleam HTTP service adapter for the Elli web server.

```
gleam add gleam_elli gleam_http
```
```gleam
import gleam/http/elli
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/bytes_tree.{type BytesTree}

// Define a HTTP service
//
pub fn my_service(req: Request(t)) -> Response(BytesBuilder) {
  let body = bytes_tree.from_string("Hello, world!")

  response.new(200)
  |> response.prepend_header("made-with", "Gleam")
  |> response.set_body(body)
}

// Start it on port 3000!
//
pub fn main() {
  elli.become(my_service, on_port: 3000)
}
```
