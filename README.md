# Gleam Elli!

A Gleam HTTP service adapter for the Elli web server.

```rust
import gleam/erlang
import gleam/http/elli
import gleam/http/response.{Response}
import gleam/http/request.{Request}
import gleam/bit_builder.{BitBuilder}

// Define a HTTP service
//
pub fn my_service(req: Request(BitString)) -> Response(BitBuilder) {
  let body = bit_builder.from_string("Hello, world!")

  response.new(200)
  |> response.prepend_header("made-with","Gleam")
  |> response.set_body(body)
}

// Start it on port 3000!
//
pub fn start() {
  elli.start(my_service, on_port: 3000)
  erlang.sleep_forever()
}
```
