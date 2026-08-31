# capture IDs cannot be reused for different evidence

    Code
      capture_document(store, capture_test_payload(markdown = "Different evidence."),
      "reader")
    Condition
      Error in `document_conflict_abort()`:
      ! The producer record ID was already used for different content. Create a new capture ID and retry.
