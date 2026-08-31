# read_opml explains malformed input

    Code
      read_opml(file)
    Condition
      Error in `read_opml()`:
      ! The file is not an OPML document.

# write_opml requires HTTP feed URLs

    Code
      write_opml(feeds, file)
    Condition
      Error in `normalize_opml_subscriptions()`:
      ! Every feed_url must be a complete HTTP or HTTPS URL.
