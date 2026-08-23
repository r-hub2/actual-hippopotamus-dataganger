#' Profile a dataset column-by-column
#'
#' Profiles each column in a data frame, detecting type, computing summary
#' statistics, missingness, cardinality, and flags for free-text, dates,
#' and haven-labelled vectors.
#'
#' @param data A data frame or tibble.
#'
#' @return An S3 object of class `dataganger_profile`, which is a list
#'   containing:
#'   \itemize{
#'     \item \code{profile}: a tibble with one row per column.
#'     \item \code{n_rows}: total number of rows.
#'     \item \code{n_cols}: total number of columns.
#'     \item \code{generated_at}: POSIXct timestamp of when profiling ran.
#'   }
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   name = letters[1:5],
#'   score = c(10.1, 15.2, NA, 13.8, 11.0)
#' )
#' profile_data(df)
profile_data <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}")
  }

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  col_profiles <- lapply(seq_len(n_cols), function(i) {
    col <- data[[i]]
    col_name <- colnames(data)[i]
    profile_column(col, col_name)
  })

  profile <- dplyr::bind_rows(col_profiles)

  out <- list(
    profile = profile,
    n_rows = n_rows,
    n_cols = n_cols,
    coverage = profile_coverage(data, profile),
    generated_at = Sys.time()
  )
  class(out) <- "dataganger_profile"
  out
}

# ---------------------------------------------------------------------------
# Cross-column coverage (powers suggest_min_rows())
# ---------------------------------------------------------------------------

# Count the distinct joint combinations observed across the low-cardinality
# ("combinable") columns, plus the largest single-column distinct count. These
# drive how many synthetic rows are needed to represent every observed
# combination and every level. Combinable columns are those with between 2 and
# `max_levels` distinct values, which excludes identifiers, free text, and
# continuous measures while capturing categoricals and low-cardinality codes.
profile_coverage <- function(data, col_profile, max_levels = 50L) {
  nd <- col_profile$n_distinct
  names(nd) <- col_profile$variable
  combinable <- col_profile$variable[!is.na(nd) & nd >= 2 & nd <= max_levels]
  combinable <- intersect(combinable, names(data))

  if (length(combinable) == 0L) {
    return(list(
      combination_count = NA_integer_,
      max_distinct      = if (length(nd) && any(!is.na(nd))) max(nd, na.rm = TRUE) else NA_integer_,
      columns           = character(0),
      max_levels        = max_levels
    ))
  }

  combos <- nrow(unique(as.data.frame(data)[, combinable, drop = FALSE]))

  list(
    combination_count = as.integer(combos),
    max_distinct      = as.integer(max(nd[combinable], na.rm = TRUE)),
    columns           = combinable,
    max_levels        = max_levels
  )
}

# ---------------------------------------------------------------------------
# Per-column profiler dispatcher
# ---------------------------------------------------------------------------

profile_column <- function(x, name) {
  if (all(is.na(x))) {
    return(profile_all_na(x, name))
  }

  if (haven::is.labelled(x)) {
    return(profile_labelled(x, name))
  }

  if (is.numeric(x)) {
    return(profile_numeric(x, name))
  }

  if (is.character(x)) {
    return(profile_character(x, name))
  }

  if (is.factor(x)) {
    return(profile_factor(x, name))
  }

  if (is.logical(x)) {
    return(profile_logical(x, name))
  }

  if (inherits(x, "Date")) {
    return(profile_date(x, name))
  }

  if (inherits(x, "POSIXct")) {
    return(profile_posixct(x, name))
  }

  cli::cli_warn(
    "Column {.val {name}} has unrecognised type {.obj_type_friendly {x}}; treating as character"
  )
  profile_character(as.character(x), name)
}

# ---------------------------------------------------------------------------
# Type-specific helpers
# ---------------------------------------------------------------------------

profile_numeric <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  tibble::tibble(
    variable       = name,
    type           = "numeric",
    n_missing      = as.integer(n_miss),
    pct_missing    = n_miss / n * 100,
    n_distinct     = length(unique(x2)),
    min            = if (length(x2) > 0) min(x2) else NA_real_,
    max            = if (length(x2) > 0) max(x2) else NA_real_,
    mean           = if (length(x2) > 0) mean(x2) else NA_real_,
    sd             = if (length(x2) > 0) stats::sd(x2) else NA_real_,
    median         = if (length(x2) > 0) stats::median(x2) else NA_real_,
    q25            = if (length(x2) > 0) stats::quantile(x2, 0.25, names = FALSE) else NA_real_,
    q75            = if (length(x2) > 0) stats::quantile(x2, 0.75, names = FALSE) else NA_real_
  )
}

profile_character <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  char_lens <- nchar(as.character(x2))
  tibble::tibble(
    variable       = name,
    type           = "character",
    n_missing      = as.integer(n_miss),
    pct_missing    = n_miss / n * 100,
    n_distinct     = if (length(x2) > 0) length(unique(x2)) else 0L,
    mean_nchar     = if (length(x2) > 0) mean(char_lens) else NA_real_,
    is_free_text   = !is.na(mean(char_lens)) && mean(char_lens) > 50 &&
                     length(unique(x2)) > n * 0.5
  )
}

profile_factor <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  levs <- levels(x)
  if (length(x2) == 0) {
    most <- NA_character_
    most_n <- 0L
    most_pct <- NA_real_
  } else {
    tbl <- sort(table(x2, useNA = "no"), decreasing = TRUE)
    most <- names(tbl)[1]
    most_n <- as.integer(tbl[1])
    most_pct <- most_n / length(x2) * 100
  }
  tibble::tibble(
    variable          = name,
    type              = "factor",
    n_missing         = as.integer(n_miss),
    pct_missing       = n_miss / n * 100,
    n_distinct        = length(levs),
    most_common_level = most,
    most_common_n     = as.integer(most_n),
    most_common_pct   = most_pct
  )
}

profile_logical <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  n_true <- sum(x2, na.rm = TRUE)
  tibble::tibble(
    variable    = name,
    type        = "logical",
    n_missing   = as.integer(n_miss),
    pct_missing = n_miss / n * 100,
    n_distinct  = if (length(x2) > 0) length(unique(x2)) else 0L,
    n_true      = as.integer(n_true),
    pct_true    = if (length(x2) > 0) n_true / length(x2) * 100 else NA_real_
  )
}

profile_date <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  tibble::tibble(
    variable    = name,
    type        = "Date",
    n_missing   = as.integer(n_miss),
    pct_missing = n_miss / n * 100,
    n_distinct  = if (length(x2) > 0) length(unique(x2)) else 0L,
    min_date    = if (length(x2) > 0) min(x2) else as.Date(NA),
    max_date    = if (length(x2) > 0) max(x2) else as.Date(NA)
  )
}

profile_posixct <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  tibble::tibble(
    variable    = name,
    type        = "POSIXct",
    n_missing   = as.integer(n_miss),
    pct_missing = n_miss / n * 100,
    n_distinct  = if (length(x2) > 0) length(unique(x2)) else 0L,
    min_datetime = if (length(x2) > 0) min(x2) else as.POSIXct(NA),
    max_datetime = if (length(x2) > 0) max(x2) else as.POSIXct(NA)
  )
}

profile_labelled <- function(x, name) {
  n_miss <- sum(is.na(x))
  n <- length(x)
  x2 <- x[!is.na(x)]
  n_labs <- length(attr(x, "labels", exact = TRUE))
  n_dist <- if (length(x2) > 0) length(unique(haven::as_factor(x2))) else 0L
  tibble::tibble(
    variable        = name,
    type            = "haven_labelled",
    n_missing       = as.integer(n_miss),
    pct_missing     = n_miss / n * 100,
    n_distinct      = n_dist,
    n_labels        = as.integer(n_labs),
    labelled_under  = typeof(x)
  )
}

profile_all_na <- function(x, name) {
  n <- length(x)
  tibble::tibble(
    variable    = name,
    type        = "all_NA",
    n_missing   = n,
    pct_missing = 100,
    n_distinct  = 0L
  )
}

# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.dataganger_profile <- function(x, ...) {
  p <- x$profile
  n_rows <- x$n_rows
  n_cols <- x$n_cols

  cli::cli_h1("DataGangeR Profile")
  cli::cli_text("{.val {n_rows}} row{?s} x {.val {n_cols}} column{?s}")

  cli::cli_h2("Column types")
  type_counts <- table(p$type)
  for (t in names(type_counts)) {
    cli::cli_li("{.field {t}}: {type_counts[t]}")
  }

  cli::cli_h2("Missingness summary")
  total_missing <- sum(p$n_missing)
  total_cells <- n_rows * n_cols
  cli::cli_text("Total missing: {total_missing} / {total_cells} ({round(total_missing / total_cells * 100, 1)}%)")

  cli::cli_h2("Per-column details")
  for (i in seq_len(nrow(p))) {
    r <- p[i, ]
    type <- r$type
    cli::cli_h3("{r$variable} ({type})")
    cli::cli_li("Missing: {r$n_missing} ({round(r$pct_missing, 1)}%)")
    cli::cli_li("Distinct values: {r$n_distinct}")

    switch(type,
      numeric = {
        cli::cli_li("Range: [{round(r$min, 2)}, {round(r$max, 2)}]")
        cli::cli_li("Mean (SD): {round(r$mean, 2)} ({round(r$sd, 2)})")
        cli::cli_li("Median (IQR): {round(r$median, 2)} ({round(r$q25, 2)} -- {round(r$q75, 2)})")
      },
      character = {
        cli::cli_li("Mean char length: {round(r$mean_nchar, 1)}")
        if (isTRUE(r$is_free_text)) {
          cli::cli_li("{.strong Free-text column detected}")
        }
      },
      factor = {
        cli::cli_li("Levels: {r$n_distinct}")
        if (!is.na(r$most_common_level)) {
          cli::cli_li("Most common: {r$most_common_level} ({r$most_common_n}; {round(r$most_common_pct, 1)}%)")
        }
      },
      logical = {
        cli::cli_li("TRUE: {r$n_true} ({round(r$pct_true, 1)}%)")
      },
      Date = {
        cli::cli_li("Range: {r$min_date} -- {r$max_date}")
      },
      POSIXct = {
        cli::cli_li("Range: {r$min_datetime} -- {r$max_datetime}")
      },
      haven_labelled = {
        cli::cli_li("Value labels: {r$n_labels}")
        cli::cli_li("Underlying type: {r$labelled_under}")
      },
      all_NA = {
        cli::cli_li("All values are NA")
      }
    )
  }

  cli::cli_text("")
  cli::cli_alert_info("Generated at {x$generated_at}")

  invisible(x)
}
