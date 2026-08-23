#' Suggest a sufficient synthetic row count
#'
#' Given a [profile_data()] profile (which carries cross-column coverage
#' information), suggests how many rows to synthesize so that the synthetic
#' data can still represent every category combination and every category
#' level observed in the original data, without blindly matching a large
#' original row count.
#'
#' The rule (coverage-based) is:
#' \itemize{
#'   \item For small inputs (fewer than `threshold` rows, default 1000) the
#'     original row count is kept --- there is nothing to gain from reducing.
#'   \item Otherwise the suggestion is the number of observed cross-column
#'     category combinations, capped at `cap` (default 5000) to avoid
#'     suggesting millions of rows on wide data, and floored at the largest
#'     per-column distinct count so every level remains representable. The
#'     suggestion never exceeds the original row count.
#' }
#'
#' Continuous columns are covered by preserving their min/max (already handled
#' by the synthesis engine); they do not raise the suggested count.
#'
#' @param profile A `dataganger_profile` from [profile_data()].
#' @param roles Optional; a `dataganger_roles` object. When provided together
#'   with `data`, the coverage computation is filtered to only the columns whose
#'   effective role is synthesizable (excludes alphanumeric IDs, free text, and
#'   user-excluded columns).
#' @param data Optional; the original data frame. When provided alongside
#'   `roles`, coverage is recomputed on the filtered column subset so that the
#'   suggestion reacts to role changes on the Configure page.
#' @param k Reserved for a future k-anonymity-style cell-size floor; unused by
#'   the current coverage rule.
#' @param threshold Row count at or above which a reduction is suggested.
#' @param cap Maximum suggested row count from combination coverage.
#'
#' @return A list with:
#'   \describe{
#'     \item{n}{Suggested integer row count.}
#'     \item{rationale}{Human-readable explanation.}
#'     \item{original_n}{Original row count.}
#'     \item{combination_count}{Observed category-combination count (or `NA`).}
#'     \item{floor}{Per-column distinct floor used (or `NA`).}
#'     \item{capped}{`TRUE` if the cap bound the suggestion.}
#'     \item{reduced}{`TRUE` if the suggestion is below the original count.}
#'   }
#' @export
#'
#' @examples
#' p <- profile_data(datasets::iris)
#' suggest_min_rows(p)
suggest_min_rows <- function(profile, roles = NULL, data = NULL, k = 5L,
                             threshold = 1000L, cap = 5000L) {
  if (!inherits(profile, "dataganger_profile")) {
    cli::cli_abort("{.arg profile} must be a {.cls dataganger_profile} object")
  }

  n_orig <- profile$n_rows %||% NA_integer_

  # When roles + raw data are provided, recompute coverage using only the
  # columns that roles say should be synthesized (excludes alphanumeric IDs,
  # free text, and user-excluded columns). This makes the suggestion react to
  # role changes on the Configure page (P3 UX polish).
  cov <- if (!is.null(roles) && !is.null(data) &&
              "recommended_role" %in% names(roles) &&
              "variable" %in% names(roles)) {
    eff_role <- ifelse(
      !is.na(roles$user_role) & nzchar(roles$user_role),
      roles$user_role, roles$recommended_role
    )
    excl_roles <- c("alphanumeric ID", "free text", "none", "unknown")
    keep <- roles$variable[!eff_role %in% excl_roles]
    keep <- intersect(keep, names(data))
    if (length(keep) == 0L) {
      list(combination_count = NA_integer_, max_distinct = NA_integer_,
           columns = character(0), max_levels = 50L)
    } else {
      profile_coverage(data[, keep, drop = FALSE],
                       profile$profile[profile$profile$variable %in% keep, ])
    }
  } else {
    profile$coverage
  }

  build <- function(n, rationale, combos = NA_integer_, floor_n = NA_integer_,
                    capped = FALSE) {
    list(
      n                 = as.integer(n),
      rationale         = rationale,
      original_n        = as.integer(n_orig),
      combination_count = combos,
      floor             = floor_n,
      capped            = capped,
      reduced           = !is.na(n) && !is.na(n_orig) && n < n_orig
    )
  }

  # Small-N: keep the original size.
  if (is.na(n_orig) || n_orig < threshold) {
    return(build(
      n_orig,
      sprintf(
        "Original is small (%s rows); synthesizing the same number.",
        format(n_orig, big.mark = ",")
      )
    ))
  }

  combos  <- if (is.null(cov)) NA_integer_ else cov$combination_count
  floor_n <- if (is.null(cov)) NA_integer_ else cov$max_distinct

  # No usable coverage signal (e.g. no low-cardinality columns): fall back to
  # the original count rather than guess.
  if (is.na(combos)) {
    return(build(
      n_orig,
      sprintf(
        "No category combinations to cover; keeping the original %s rows.",
        format(n_orig, big.mark = ",")
      ),
      combos = combos, floor_n = floor_n
    ))
  }

  target    <- min(combos, cap)
  floor_use <- if (is.na(floor_n)) 0L else floor_n
  suggested <- max(floor_use, target)
  suggested <- min(suggested, n_orig)
  capped    <- combos > cap

  rationale <- sprintf(
    "Covers all %s observed category combination(s) across %s column(s); floor %s (largest level count). Original: %s rows.",
    format(combos, big.mark = ","),
    if (is.null(cov)) "0" else length(cov$columns),
    format(floor_use, big.mark = ","),
    format(n_orig, big.mark = ",")
  )
  if (capped) {
    rationale <- paste0(
      rationale,
      sprintf(" Combination count exceeded the cap (%s), so the suggestion is capped.",
              format(cap, big.mark = ","))
    )
  }

  build(suggested, rationale, combos = combos, floor_n = floor_n, capped = capped)
}
