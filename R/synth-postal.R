# ===========================================================================
# Postal code synthesis
# ===========================================================================

#' Generate synthetic postal codes from a format specification
#'
#' Produces n random postal codes using the slot specification from a
#' detected format entry. Generated values are fully random and never
#' sampled from the input column (zero source-value leakage).
#'
#' @param x Original character vector (used only for missingness estimation).
#' @param n Number of synthetic values to produce.
#' @param postal_info A registry entry from detect_postal_format().
#' @param missing_strategy Missingness strategy passed to apply_missingness().
#' @return A character vector of length n.
#' @keywords internal
#' @noRd
synth_postal_code_generate <- function(x, n, postal_info,
                                       missing_strategy = "approx") {
  slots <- postal_info$slots

  out <- vapply(seq_len(n), function(i) {
    parts <- vapply(slots, function(slot) {
      if (slot$type == "digit") {
        sample(strsplit(slot$chars, "")[[1L]], 1L)
      } else if (slot$type == "letter") {
        sample(strsplit(slot$chars, "")[[1L]], 1L)
      } else {
        slot$chars
      }
    }, character(1L))
    paste0(parts, collapse = "")
  }, character(1L))

  apply_missingness(out, x, n, missing_strategy)
}

#' Resample postal codes from observed values
#'
#' Samples with replacement from the observed non-NA values using their
#' empirical frequencies. No rare-level merging is applied because postal
#' codes are geographic codes, not categories.
#'
#' @param x Original character vector of postal codes.
#' @param n Number of synthetic values to produce.
#' @param missing_strategy Missingness strategy passed to apply_missingness().
#' @return A character vector of length n.
#' @keywords internal
#' @noRd
synth_postal_code_resample <- function(x, n, missing_strategy = "approx") {
  x_obs <- x[!is.na(x)]

  if (length(x_obs) == 0L) {
    return(rep(NA_character_, n))
  }

  tbl <- table(x_obs)
  probs <- as.integer(tbl) / sum(tbl)
  out <- sample(names(tbl), size = n, replace = TRUE, prob = probs)

  apply_missingness(out, x, n, missing_strategy)
}
