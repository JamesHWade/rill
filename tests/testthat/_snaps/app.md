# polling requires durable configuration

    Code
      poll_feeds()
    Condition
      Error in `poll_feeds()`:
      ! Can't poll feeds without a durable store.
      i Set `DATABASE_URL` to a PostgreSQL connection string.

# preparing today requires durable configuration

    Code
      prepare_today()
    Condition
      Error in `prepare_today()`:
      ! Can't prepare today's articles without a durable store.
      i Set `DATABASE_URL` to a PostgreSQL connection string.
