#' Check whether synthetic data is code-compatible with the original
#'
#' Evaluates whether code written against the synthetic development twin will
#' run against the original data without errors. Checks column presence, R
#' class compatibility, factor level coverage, all-NA columns, zero-variance
#' columns, missingness spikes, and ID uniqueness. `haven_labelled` columns
#' currently round-trip as character in synthetic data, so that class change is
#' expected for now.
#' @param original The original data frame.
#' @param synthetic The synthetic data frame (from [synthesize_data()]).
#' @param roles Optional; a \code{dataganger_roles} object from
#'   [detect_roles()]. Used for ID-uniqueness checks. Computed internally if
#'   \code{NULL}.
#' @return An S3 object of class \code{dataganger_code_readiness} with
#'   components:
#'   \describe{
#'     \item{checks}{A tibble with one row per check: \code{check}, \code{scope},
#'       \code{column}, \code{status} ("pass"/"warn"/"fail"), \code{message}.}
#'     \item{summary}{List with \code{n_pass}, \code{n_warn}, \code{n_fail},
#'       \code{ready} (TRUE when n_fail == 0).}
#'     \item{meta}{List with dimensions and \code{generated_at}.}
#'   }
#' @export
#'
#' @examples
#' orig <- data.frame(x = 1:10, y = factor(letters[1:10]))
#' spec <- synth_spec(purpose = "demo")
#' syn  <- synthesize_data(orig, spec)
#' check_code_readiness(orig, syn)
check_code_readiness <- function(original, synthetic, roles = NULL) {
  if (!is.data.frame(original)) {
    cli::cli_abort("{.arg original} must be a data frame")
  }
  if (!is.data.frame(synthetic)) {
    cli::cli_abort("{.arg synthetic} must be a data frame")
  }

  if (is.null(roles) && nrow(original) > 0L) {
    roles <- tryCatch(
      detect_roles(original),
      error = function(e) NULL
    )
  }

  rows <- list()

  # Dataset-level checks

  missing_cols <- setdiff(names(original), names(synthetic))
  if (length(missing_cols) == 0L) {
    rows <- c(rows, list(cr_row("column_names_match", "dataset", NA_character_,
                                "pass", "All original columns present in synthetic")))
  } else {
    rows <- c(rows, list(cr_row("column_names_match", "dataset", NA_character_,
                                "fail",
                                sprintf("Missing from synthetic: %s",
                                        paste(missing_cols, collapse = ", ")))))
  }

  extra_cols <- setdiff(names(synthetic), names(original))
  if (length(extra_cols) == 0L) {
    rows <- c(rows, list(cr_row("no_extra_columns", "dataset", NA_character_,
                                "pass", "No extra columns in synthetic")))
  } else {
    rows <- c(rows, list(cr_row("no_extra_columns", "dataset", NA_character_,
                                "warn",
                                sprintf("Extra columns in synthetic (not in original): %s",
                                        paste(extra_cols, collapse = ", ")))))
  }

  # Per-column checks

  common <- intersect(names(original), names(synthetic))

  id_cols <- if (!is.null(roles)) {
    roles$variable[roles$recommended_role == "alphanumeric ID"]
  } else {
    character()
  }

  for (col in common) {
    orig_col <- original[[col]]
    syn_col  <- synthetic[[col]]

    # class_match
    if (identical(class(orig_col), class(syn_col))) {
      rows <- c(rows, list(cr_row("class_match", "column", col,
                                  "pass", sprintf("class '%s' matches", paste(class(orig_col), collapse = "/")))))
    } else {
      expected_labelled <- haven::is.labelled(orig_col) && is.character(syn_col)
      class_msg <- sprintf("class mismatch: original '%s' vs synthetic '%s'%s",
                           paste(class(orig_col), collapse = "/"),
                           paste(class(syn_col), collapse = "/"),
                           if (expected_labelled) "; haven_labelled -> character is expected for now" else "")
      rows <- c(rows, list(cr_row("class_match", "column", col, "fail", class_msg)))
    }

    # all_na
    orig_all_na <- all(is.na(orig_col))
    syn_all_na  <- all(is.na(syn_col))
    if (syn_all_na && !orig_all_na) {
      rows <- c(rows, list(cr_row("all_na", "column", col,
                                  "fail",
                                  "Synthetic column is all-NA; original was not - code touching this column will break")))
    } else {
      rows <- c(rows, list(cr_row("all_na", "column", col, "pass",
                                  "No all-NA issue")))
    }

    # zero_variance
    orig_uq <- length(unique(stats::na.omit(orig_col)))
    syn_uq  <- length(unique(stats::na.omit(syn_col)))
    if (syn_uq <= 1L && orig_uq > 1L) {
      rows <- c(rows, list(cr_row("zero_variance", "column", col,
                                  "warn",
                                  "Synthetic column has <= 1 unique value; model formulas using this column may fail")))
    } else {
      rows <- c(rows, list(cr_row("zero_variance", "column", col, "pass",
                                  "Sufficient variance")))
    }

    # factor_levels
    if (is.factor(orig_col) && is.factor(syn_col)) {
      missing_levels <- setdiff(levels(orig_col), levels(syn_col))
      if (length(missing_levels) > 0L) {
        rows <- c(rows, list(cr_row("factor_levels", "column", col,
                                    "warn",
                                    sprintf("Original levels missing from synthetic: %s",
                                            paste(missing_levels, collapse = ", ")))))
      } else {
        rows <- c(rows, list(cr_row("factor_levels", "column", col, "pass",
                                    "All original factor levels present in synthetic")))
      }
    }

    # missingness_spike
    n_orig <- length(orig_col)
    n_syn  <- length(syn_col)
    pct_na_orig <- if (n_orig > 0L) sum(is.na(orig_col)) / n_orig else 0
    pct_na_syn  <- if (n_syn  > 0L) sum(is.na(syn_col))  / n_syn  else 0
    if (pct_na_orig < 0.05 && pct_na_syn > 0.50) {
      rows <- c(rows, list(cr_row("missingness_spike", "column", col,
                                  "warn",
                                  sprintf("NA%% jumped from %.0f%% (original) to %.0f%% (synthetic); code that assumes non-NA may break",
                                          pct_na_orig * 100, pct_na_syn * 100))))
    } else {
      rows <- c(rows, list(cr_row("missingness_spike", "column", col, "pass",
                                  "Missingness within acceptable range")))
    }

    # id_uniqueness
    if (col %in% id_cols) {
      n_dup <- sum(duplicated(stats::na.omit(syn_col)))
      if (n_dup > 0L) {
        rows <- c(rows, list(cr_row("id_uniqueness", "column", col,
                                    "warn",
                                    sprintf("ID column has %d duplicate values in synthetic; join code assuming unique keys may produce wrong row counts",
                                            n_dup))))
      } else {
        rows <- c(rows, list(cr_row("id_uniqueness", "column", col, "pass",
                                    "ID values are unique in synthetic")))
      }
    }
  }

  checks  <- dplyr::bind_rows(rows)
  n_pass  <- sum(checks$status == "pass")
  n_warn  <- sum(checks$status == "warn")
  n_fail  <- sum(checks$status == "fail")

  out <- list(
    checks  = checks,
    summary = list(
      n_pass = n_pass,
      n_warn = n_warn,
      n_fail = n_fail,
      ready  = n_fail == 0L
    ),
    meta = list(
      generated_at = Sys.time(),
      nrow_orig    = nrow(original),
      ncol_orig    = ncol(original),
      nrow_syn     = nrow(synthetic),
      ncol_syn     = ncol(synthetic)
    )
  )
  class(out) <- "dataganger_code_readiness"
  out
}

cr_row <- function(check, scope, column, status, message) {
  tibble::tibble(
    check   = check,
    scope   = scope,
    column  = column,
    status  = status,
    message = message
  )
}

#' @export
print.dataganger_code_readiness <- function(x, ...) {
  s <- x$summary
  cli::cli_h1("DataGangeR Code Readiness")
  cli::cli_text(
    "{.val {s$n_pass}} pass, {.val {s$n_warn}} warn, {.val {s$n_fail}} fail"
  )
  if (s$ready) {
    cli::cli_alert_success("Ready: no blocking issues found")
  } else {
    cli::cli_alert_danger("Not ready: {.val {s$n_fail}} blocking issue{?s}")
  }
  cli::cli_text("")

  fails <- x$checks[x$checks$status == "fail", ]
  warns <- x$checks[x$checks$status == "warn", ]

  if (nrow(fails) > 0L) {
    cli::cli_h2("Failures")
    for (i in seq_len(nrow(fails))) {
      r <- fails[i, ]
      label <- if (r$scope == "column") sprintf("[%s] ", r$column) else ""
      cli::cli_alert_danger("{label}{r$message}")
    }
  }

  if (nrow(warns) > 0L) {
    cli::cli_h2("Warnings")
    for (i in seq_len(nrow(warns))) {
      r <- warns[i, ]
      label <- if (r$scope == "column") sprintf("[%s] ", r$column) else ""
      cli::cli_alert_warning("{label}{r$message}")
    }
  }

  invisible(x)
}
