# Schema-only synthesis (Level 1)
#
# Internal function. Returns a 0-row tibble with column names and types
# matching the original. No values are synthesized.
#
# Used by: demo preset, level = "schema"

synthesize_schema <- function(data, spec, roles = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  n <- spec$n %||% nrow(data)
  nms <- names(data)
  types <- lapply(data, function(col) {
    if (haven::is.labelled(col)) {
      rep(NA_character_, n)
    } else if (inherits(col, "Date")) {
      rep(as.Date(NA), n)
    } else if (inherits(col, "POSIXct")) {
      rep(as.POSIXct(NA, tz = attr(col, "tzone") %||% "UTC"), n)
    } else if (is.factor(col)) {
      factor(rep(NA_character_, n), levels = levels(col))
    } else if (is.numeric(col)) {
      rep(NA_real_, n)
    } else if (is.character(col)) {
      rep(NA_character_, n)
    } else if (is.logical(col)) {
      rep(NA, n)
    } else {
      rep(NA_character_, n)
    }
  })

  out <- tibble::as_tibble(stats::setNames(types, nms))
  out
}
