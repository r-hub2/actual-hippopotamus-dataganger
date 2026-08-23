#' Enforce k-anonymity on a synthetic dataset (output guarantee)
#'
#' Shapes the synthetic output so that no quasi-identifier combination appears
#' in fewer than `k` records. Direct identifiers are removed. Quasi-identifiers
#' are coarsened step-by-step and any residual cell still below `k` has its QI
#' values blanked (`NA`). Operates on the output only.
#'
#' @param synthetic A synthetic data frame.
#' @param roles A roles object/data frame with `variable` + `disclosure_role`.
#' @param k Minimum cell size (default 5).
#' @param max_steps Maximum coarsening iterations (default 6).
#' @param max_suppress_frac Feasibility backstop. If satisfying `k` over the
#'   quasi-identifier set would require blanking more than this fraction of
#'   rows, k-anonymity is treated as infeasible for the chosen quasi-identifier
#'   (QI) set: the coarsening and suppression steps are *not* applied, the
#'   synthetic output is returned populated, and a warning explains that no
#'   k-anonymity protection was applied to that output. Default 0.2.
#'
#' @return The shaped `synthetic` data frame, with an attribute `kanon`
#'   recording the achieved state (`smallest_cell`, `suppressed_cells`,
#'   `suppressed_rows`, `suppressed_row_frac`, `qi_cols`, `k`, `infeasible`).
#'   `suppressed_rows`/`suppressed_row_frac` count actual blanked rows across
#'   the QI columns -- distinct from `suppressed_cells`, which counts the
#'   number of distinct QI combinations folded into suppression. The two can
#'   differ a lot: reaching k can require absorbing a few whole neighbouring
#'   cells (suppression works at cell granularity, not row granularity), and
#'   a handful of small cells sitting next to one dominant cell can end up
#'   suppressing most or all of a QI column even though only a few original
#'   cells were actually below k.
#' @export
enforce_kanon <- function(synthetic, roles, k = 5, max_steps = 6L,
                          max_suppress_frac = 0.2) {
  if (is.null(roles) || !"disclosure_role" %in% names(roles)) {
    attr(synthetic, "kanon") <- list(
      qi_cols = character(0), k = k, smallest_cell = NA_integer_,
      suppressed_cells = 0L, suppressed_rows = 0L, suppressed_row_frac = 0
    )
    return(synthetic)
  }

  dr <- stats::setNames(roles$disclosure_role, roles$variable)

  direct <- names(dr)[dr %in% "direct"]  # %in% is NA-safe; == returns NA for unselected roles

  # A pass_through or scramble simulation is a keep-decision for the column
  # (pass_through keeps values verbatim; scramble replaces them), and it takes
  # precedence over the disclosure-role drop. For alphanumeric IDs the scramble
  # is the derived default rather than a hand-picked choice, but it is still a
  # keep-decision the caller can override, so it is honoured the same way.
  # Only "synthesize" (untouched) and the explicit "drop" reach the direct-ID
  # drop below.
  if ("simulation" %in% names(roles)) {
    sim <- stats::setNames(roles$simulation, roles$variable)
    kept_by_action <- names(sim)[sim %in% c("pass_through", "scramble")]
    direct <- setdiff(direct, kept_by_action)
  }

  drop_cols <- intersect(direct, names(synthetic))
  if (length(drop_cols)) {
    synthetic <- synthetic[, !names(synthetic) %in% drop_cols, drop = FALSE]
  }

  qi_cols <- intersect(dg_kanon_columns(roles), names(synthetic))
  if (length(qi_cols) == 0L) {
    attr(synthetic, "kanon") <- list(
      qi_cols = qi_cols, k = k, smallest_cell = NA_integer_,
      suppressed_cells = 0L, suppressed_rows = 0L, suppressed_row_frac = 0,
      infeasible = FALSE
    )
    return(synthetic)
  }

  # Coarsen on a working copy so the populated original can be restored if the
  # k-anonymity target turns out to be infeasible for this QI set.
  base <- synthetic
  for (step in seq_len(max_steps)) {
    res <- assess_kanonymity(synthetic, qi_cols, k)
    if (is.na(res$smallest_cell) || res$smallest_cell >= k) {
      break
    }
    for (col in qi_cols) {
      synthetic[[col]] <- coarsen_qi_step(synthetic[[col]], step)
    }
  }

  # Feasibility backstop. If reaching k would blank more than `max_suppress_frac`
  # of rows, the QI set is too wide to anonymise without destroying the data
  # (e.g. 9 quasi-identifiers over a few hundred rows). Rather than ship a
  # mostly-NA dataset, back off entirely: return the populated (uncoarsened)
  # synthetic and tell the user how to make enforcement feasible.
  res <- assess_kanonymity(synthetic, qi_cols, k)
  n_rows <- nrow(synthetic)
  would_suppress <- if (!is.na(res$smallest_cell) && res$smallest_cell < k) {
    key <- kanon_key(synthetic, qi_cols)
    counts <- table(key)
    sum(as.integer(counts[key]) < k)
  } else {
    0L
  }
  if (n_rows > 0L && would_suppress / n_rows > max_suppress_frac) {
    qi_text <- paste(qi_cols, collapse = ", ")
    cli::cli_warn(c(
      "Could not apply k-anonymity at k = {k} to the selected quasi-identifier (QI) columns: {qi_text}.",
      "i" = "Reaching k would blank {would_suppress}/{n_rows} rows ({round(100 * would_suppress / n_rows)}%).",
      "i" = "To avoid destroying the dataset, no k-anonymity protection was applied to this output.",
      "i" = "Try a smaller k, generate more rows, or mark fewer columns as quasi-identifiers."
    ))
    base_res <- assess_kanonymity(base, qi_cols, k)
    attr(base, "kanon") <- list(
      qi_cols = qi_cols, k = k, smallest_cell = base_res$smallest_cell,
      suppressed_cells = 0L, suppressed_rows = 0L, suppressed_row_frac = 0,
      infeasible = TRUE
    )
    return(base)
  }

  suppressed <- 0L
  if (!is.na(res$smallest_cell) && res$smallest_cell < k) {
    key <- kanon_key(synthetic, qi_cols)
    counts <- table(key)
    small <- as.integer(counts[key]) < k
    suppressed <- length(unique(key[small]))
    for (col in qi_cols) {
      synthetic[[col]][small] <- NA
    }
  }

  # The NA bucket created by blanking may itself be smaller than k.
  # Absorb rows from the smallest remaining non-NA cell until the bucket reaches k.
  repeat {
    na_rows <- rowSums(is.na(synthetic[qi_cols])) == length(qi_cols)
    na_count <- sum(na_rows)
    if (na_count == 0L || na_count >= k) break
    non_na <- which(!na_rows)
    if (!length(non_na)) break
    key_non_na <- kanon_key(synthetic[non_na, , drop = FALSE], qi_cols)
    counts_non_na <- table(key_non_na)
    smallest_key <- names(which.min(counts_non_na))
    to_blank <- non_na[key_non_na == smallest_key]
    for (col in qi_cols) synthetic[[col]][to_blank] <- NA
    suppressed <- suppressed + 1L
  }

  final <- assess_kanonymity(synthetic, qi_cols, k)
  suppressed_rows <- sum(rowSums(is.na(synthetic[qi_cols])) == length(qi_cols))
  attr(synthetic, "kanon") <- list(
    qi_cols = qi_cols,
    k = k,
    smallest_cell = final$smallest_cell,
    suppressed_cells = suppressed,
    suppressed_rows = suppressed_rows,
    suppressed_row_frac = if (n_rows > 0L) suppressed_rows / n_rows else 0,
    infeasible = FALSE
  )
  synthetic
}

# Builds one key per row identifying that row's combination of
# quasi-identifier values. Rows are keyed on per-column factor codes rather
# than on the values themselves, so no separator can collide with real data:
# an integer code cannot contain the comma that joins them. A previous version
# pasted the values under a "\u0001" separator and mapped NA to the literal
# "<NA>"; a value containing that control character, or the literal text
# "<NA>", could merge two distinct combinations into one and understate the
# risk. `exclude = NULL` keeps NA as its own level, so missing values cannot
# mask a small cell.
kanon_key <- function(data, qi_cols) {
  parts <- lapply(data[qi_cols], function(col) {
    as.integer(factor(as.character(col), exclude = NULL))
  })
  do.call(paste, c(parts, sep = ","))
}

coarsen_qi_step <- function(x, step) {
  is_range_label <- function(values) {
    vals <- values[!is.na(values) & nzchar(trimws(values))]
    if (!length(vals)) {
      return(FALSE)
    }
    all(grepl("^\\[.*\\]$|^\\(.*\\]$", vals))
  }

  if (inherits(x, "Date")) {
    return(switch(
      min(step, 3L),
      coarsen_to_month(x),
      coarsen_to_quarter(x),
      coarsen_to_year(x)
    ))
  }
  if (inherits(x, "POSIXct")) {
    return(as.Date(x))
  }
  if (is.character(x)) {
    chr <- x
    if (is_range_label(chr)) {
      return(chr)
    }
    # ISO date strings (YYYY-MM-DD) -- coarsen as Date to avoid 366-level
    # merge_rarest_level loop that leaves every row unique after 6 steps.
    chr_nna <- chr[!is.na(chr) & nzchar(trimws(chr))]
    if (length(chr_nna) > 0L &&
        mean(grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(chr_nna))) >= 0.9) {
      dates <- suppressWarnings(as.Date(chr, format = "%Y-%m-%d"))
      if (sum(!is.na(dates)) > 0L) {
        return(switch(min(step, 3L),
          coarsen_to_month(dates),
          coarsen_to_quarter(dates),
          coarsen_to_year(dates)
        ))
      }
    }
    return(merge_rarest_level(chr))
  }
  if (is.numeric(x)) {
    bins <- max(2L, 8L - step)
    br <- stats::quantile(
      x,
      probs = seq(0, 1, length.out = bins + 1L),
      na.rm = TRUE,
      names = FALSE
    )
    br <- unique(br)
    if (length(br) < 2L) {
      return(x)
    }
    return(as.character(cut(x, breaks = br, include.lowest = TRUE, ordered_result = TRUE)))
  }
  x
}

merge_rarest_level <- function(chr) {
  tab <- sort(table(chr[!is.na(chr)]))
  if (length(tab) <= 1L) {
    return(chr)
  }
  rarest <- names(tab)[1]
  chr[!is.na(chr) & chr == rarest] <- NA_character_
  chr
}
