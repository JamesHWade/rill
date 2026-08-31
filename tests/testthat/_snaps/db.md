# state fields are validated

    Code
      store_toggle_state(store, "reader", "entry", "archived")
    Condition
      Error in `store_toggle_state()`:
      ! `field` must be one of "starred" or "saved", not "archived".
