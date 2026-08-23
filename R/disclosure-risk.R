#' Assess k-anonymity over a set of quasi-identifier columns
#'
#' Cross-tabulates the quasi-identifier columns and reports how many records
#' fall in combinations (equivalence classes) smaller than `k`. `NA` is treated
#' as a distinct level so that missing values cannot mask a small cell.
#'
#' @param data A data frame.
#' @param qi_cols Character vector of quasi-identifier column names.
#' @param k Minimum acceptable cell size (default 5).
#'
#' @return A list with `no_qi` (logical), `smallest_cell` (integer),
#'   `n_below`, `pct_below`, and `worst_cells` (a tibble of the smallest
#'   combinations and their counts).
#' @export
assess_kanonymity <- function(data, qi_cols, k = 5) {
  qi_cols <- intersect(qi_cols, names(data))
  n <- nrow(data)

  if (length(qi_cols) == 0L || n == 0L) {
    return(list(
      no_qi = length(qi_cols) == 0L,
      smallest_cell = NA_integer_,
      n_below = 0L,
      pct_below = 0,
      worst_cells = tibble::tibble()
    ))
  }

  # Shares kanon_key() with the enforcement path so that what this reports and
  # what enforce_kanon() acts on can never disagree about what a combination is.
  key <- kanon_key(data, qi_cols)
  counts <- table(key)
  cell_n <- as.integer(counts[key])

  below <- cell_n < k
  smallest <- as.integer(min(cell_n))

  uniq <- !duplicated(key)
  worst <- tibble::as_tibble(data[uniq, qi_cols, drop = FALSE])
  worst$n <- as.integer(counts[key[uniq]])
  worst <- worst[order(worst$n), , drop = FALSE]
  worst <- worst[worst$n < k, , drop = FALSE]
  worst <- utils::head(worst, 10L)

  list(
    no_qi = FALSE,
    smallest_cell = smallest,
    n_below = as.integer(sum(below)),
    pct_below = round(100 * sum(below) / n, 1),
    worst_cells = worst
  )
}

#' Heuristic: does this data frame look pre-aggregated (a table of counts)?
#'
#' Disclosure control assumes individual-level microdata. A positive result
#' should drive a non-blocking warning, not a separate policy.
#'
#' @param data A data frame.
#' @return A list with `aggregated` (logical) and `reason` (character).
#' @export
looks_aggregated <- function(data) {
  nm <- tolower(names(data))
  count_cols <- names(data)[nm %in% c("n", "count", "freq", "frequency", "total")]
  has_count <- length(count_cols) > 0L &&
    any(vapply(data[count_cols], function(x) {
      is.numeric(x) && all(x >= 0, na.rm = TRUE) && all(x == round(x), na.rm = TRUE)
    }, logical(1)))

  dim_cols <- setdiff(names(data), count_cols)
  few_rows <- nrow(data) > 0L && nrow(data) <= 1000L
  unique_dims <- length(dim_cols) > 0L &&
    !any(duplicated(data[dim_cols])) &&
    nrow(data) > 0L

  aggregated <- has_count && few_rows && unique_dims
  reason <- if (aggregated) {
    sprintf(
      "count column(s) %s with unique dimension rows",
      paste(count_cols, collapse = ", ")
    )
  } else {
    "no count column / looks like individual records"
  }

  list(aggregated = aggregated, reason = reason)
}
