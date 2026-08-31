# local network feed URLs are rejected

    Code
      validate_public_http_url("http://127.0.0.1/feed")
    Condition
      Error in `validate_public_http_url()`:
      ! `url` must not refer to a private or local network.

---

    Code
      validate_public_http_url("http://192.168.1.2/rss")
    Condition
      Error in `validate_public_http_url()`:
      ! `url` must not refer to a private or local network.

# feed URLs require an HTTP scheme

    Code
      validate_public_http_url("example.com/feed")
    Condition
      Error in `validate_public_http_url()`:
      ! `url` must be a complete `http://` or `https://` URL.
