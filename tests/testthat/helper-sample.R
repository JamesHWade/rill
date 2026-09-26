# Many tests were written when the six demo stories were spread across the
# three real sample feeds. The demo now keeps them in its own Rill feed, so
# these tests recreate that earlier layout.
three_feed_sample <- function(sample) {
  real <- sample$feeds$feed_id != "sample-rill"
  sample$feeds <- sample$feeds[real, , drop = FALSE]
  sample$entries$feed_id <- rep(
    sample$feeds$feed_id,
    length.out = nrow(sample$entries)
  )
  sample
}

local_three_feed_demo <- function(env = parent.frame()) {
  sample <- sample_rill_data
  testthat::local_mocked_bindings(
    sample_rill_data = \() three_feed_sample(sample()),
    .env = env
  )
}
