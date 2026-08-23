# Test helper: drive the column-filter "Continue" step.
#
# After an upload sets `state$upload_source`, the app shows the column-filter
# modal (column names only) and does NOT read data until the user clicks
# Continue. Harness tests that simulate an upload and then expect `raw_data`
# (profiling, role detection, synthesis, ...) must drive Continue in between.
#
# `cf_continue()` keeps every column by default (or drops the named ones) and
# flushes reactives, so downstream state (raw_data, profile) is populated as it
# would be in the running app.

cf_buckets <- function(columns, drop = character(0)) {
  as.list(stats::setNames(
    ifelse(columns %in% drop, "drop", "synthesize"),
    columns
  ))
}

cf_continue <- function(session, state, drop = character(0)) {
  cols <- state$upload_source$columns
  session$setInputs(`column_filter-buckets` = cf_buckets(cols, drop))
  session$flushReact()
}
