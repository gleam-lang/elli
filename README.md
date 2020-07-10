# Gleam Elli!

A Gleam HTTP service adapter for the Elli web server.

```rust
import gleam/http
import gleam/elli
import gleam/bit_string
import gleam/bit_builder

// Define a HTTP service
//
pub fn my_service(req: http.Request) -> http.Response {
  let body = "Hello, world!"
    |> bit_string.from_string
    |> bit_builder.from_bit_string

  http.response(200)
  |> http.put_resp_header("served-by", "Elli")
  |> http.put_resp_body(body)
}

// Start it on port 3000!
//
pub fn start() {
  elli.start(my_service, 3000)
}
```
