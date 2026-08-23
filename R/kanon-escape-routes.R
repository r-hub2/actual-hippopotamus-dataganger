#' Compute user-facing escape routes for infeasible k-anonymity
#'
#' @param data A data frame.
#' @param roles A roles object/data frame.
#' @param k Current k target.
#'
#' @return A list describing the largest feasible k, the smallest probed row
#'   count that makes the current k feasible, the QI driver column, and the QI
#'   columns examined.
#' @keywords internal
#' @noRd
kanon_escape_routes <- function(data, roles, k) {
  qi_cols <- intersect(dg_kanon_columns(roles), names(data))
  if (!length(qi_cols)) {
    return(list(
      qi_cols = character(0),
      feasible_k = NULL,
      feasible_k_suppressed_cells = NULL,
      suggested_n = NULL,
      suggested_n_suppressed_cells = NULL,
      skipped_n_probe = FALSE,
      driver_col = NULL
    ))
  }

  distinct_n <- vapply(
    data[qi_cols],
    function(col) length(unique(col[!is.na(col)])),
    integer(1)
  )
  driver_col <- names(distinct_n)[which.max(distinct_n)][[1]]

  feasible_k <- NULL
  feasible_k_suppressed_cells <- NULL
  for (candidate_k in seq.int(from = k, to = 3L, by = -1L)) {
    probe <- quiet_kanon_probe(data, roles, candidate_k)
    info <- attr(probe, "kanon", exact = TRUE)
    if (!is.null(info) && !isTRUE(info$infeasible)) {
      feasible_k <- candidate_k
      feasible_k_suppressed_cells <- info$suppressed_cells %||% 0L
      break
    }
  }

  suggested_n <- NULL
  suggested_n_suppressed_cells <- NULL
  skipped_n_probe <- nrow(data) > 50000L
  if (!skipped_n_probe) {
    probe_rows <- unique(pmin(as.integer(c(2L, 5L, 10L) * nrow(data)), 10000L))
    probe_rows <- probe_rows[probe_rows > nrow(data)]

    for (target_n in probe_rows) {
      probe <- quiet_kanon_synthesis_probe(data, roles, k, target_n)
      info <- attr(probe, "kanon", exact = TRUE)
      if (!is.null(info) && !isTRUE(info$infeasible)) {
        suggested_n <- target_n
        suggested_n_suppressed_cells <- info$suppressed_cells %||% 0L
        break
      }
    }
  }

  list(
    qi_cols = qi_cols,
    feasible_k = feasible_k,
    feasible_k_suppressed_cells = feasible_k_suppressed_cells,
    suggested_n = suggested_n,
    suggested_n_suppressed_cells = suggested_n_suppressed_cells,
    skipped_n_probe = skipped_n_probe,
    driver_col = driver_col
  )
}

#' @keywords internal
#' @noRd
quiet_kanon_probe <- function(data, roles, k) {
  withCallingHandlers(
    suppressMessages(enforce_kanon(data, roles = roles, k = k)),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

#' @keywords internal
#' @noRd
quiet_kanon_synthesis_probe <- function(data, roles, k, n) {
  # Pin the internal engine: the probe estimates cell density, which does not
  # need correlation-aware synthesis, and an unpinned development spec would
  # route through synthpop when installed (slow, engine-dependent suggestions).
  spec <- synth_spec(
    purpose = "development",
    n = n,
    seed = 1L,
    k_anon = k,
    engine = "internal"
  )

  withCallingHandlers(
    suppressMessages(synthesize_data(data, spec = spec, roles = roles)),
    warning = function(w) invokeRestart("muffleWarning")
  )
}
