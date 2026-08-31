# polling requires durable configuration

    Code
      poll_feeds()
    Condition
      Error in `poll_feeds()`:
      ! Can't poll feeds without a durable store.
      i Set `DATABASE_URL` to a PostgreSQL connection string.
