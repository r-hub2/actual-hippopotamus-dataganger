#' Run disclosure-risk privacy checks
#'
#' Scans original and (optionally) synthetic data for disclosure-risk flags.
#' Supports two stages: `"pre"` (before synthesis, requires only the
#' original dataset and roles) and `"post"` (after synthesis, requires both
#' original and synthetic).
#'
#' @param original The original data frame.
#' @param synthetic Optional; the synthetic data frame (required for
#'   `stage = "post"`).
#' @param roles Optional; a `dataganger_roles` object from [detect_roles()].
#'   Recommended for pre-stage flag detection. When omitted, fallback name/type heuristics are used.
#' @param stage Character. `"pre"` or `"post"`.
#' @param spec Optional; a `dataganger_spec` object. When provided at
#'   `stage = "post"`, cross-checks that synthesis parameters were applied
#'   (e.g. date coarsening, ID removal).
#'
#' @return An S3 object of class `dataganger_privacy_check`, a tibble with
#'   columns `variable`, `flag`, `severity`, `stage`, and `recommendation`.
#' @export
#'
#' @examples
#' df <- data.frame(id = 1:50, x = rnorm(50), city = rep("Toronto", 50))
#' roles <- detect_roles(df)
#' privacy_check(df, roles = roles, stage = "pre")
privacy_check <- function(original, synthetic = NULL, roles = NULL,
                          stage = c("pre", "post"), spec = NULL) {
  stage <- match.arg(stage)

  if (!is.data.frame(original)) {
    cli::cli_abort("{.arg original} must be a data frame")
  }

  if (stage == "post") {
    if (is.null(synthetic) || !is.data.frame(synthetic)) {
      cli::cli_abort("{.arg synthetic} must be a data frame for {.code stage = \"post\"}")
    }
  }

  if (stage == "pre") {
    flags <- privacy_check_pre(original, roles)
    attr(flags, "exact_row_matches") <- 0L
  } else {
    flags <- privacy_check_post(original, synthetic, roles, spec)
    flags <- augment_synthpop_disclosure(flags, original, synthetic, roles)
  }

  attr(flags, "stage")   <- stage
  attr(flags, "n_flags") <- nrow(flags)
  class(flags) <- c("dataganger_privacy_check", class(flags))

  flags
}

# ===========================================================================
# Pre-stage flags [3.6]
# ===========================================================================

privacy_check_pre <- function(original, roles) {
  flags <- list()

  # Build role lookup from roles object if available
  role_map <- NULL
  disclosure_map <- NULL
  if (!is.null(roles) && "variable" %in% names(roles)) {
    if ("recommended_role" %in% names(roles)) {
      role_map <- stats::setNames(roles$recommended_role, roles$variable)
    }
    if ("disclosure_role" %in% names(roles)) {
      disclosure_map <- stats::setNames(roles$disclosure_role, roles$variable)
    }
  }

  for (nm in names(original)) {
    x <- original[[nm]]
    role <- dg_named_lookup(role_map, nm); if (is.na(role)) role <- "unknown"
    disclosure <- dg_named_lookup(disclosure_map, nm); if (is.na(disclosure)) disclosure <- "none"

    # ID columns -> HIGH
    if (role == "alphanumeric ID" || grepl(dg_id_name_pattern(), nm, perl = TRUE)) {
      flags[[length(flags) + 1]] <- make_flag(nm, "ID column detected", "HIGH",
        "Review whether this column should be excluded from synthetic output")
      next
    }

    # Direct identifier -> HIGH
    if (identical(disclosure, "direct")) {
      flags[[length(flags) + 1]] <- make_flag(nm, "Direct identifier", "HIGH",
        "Direct identifiers are removed from synthetic output")
      next
    }

    # Sensitive target -> MEDIUM (informational; not yet enforced)
    if (identical(disclosure, "sensitive")) {
      flags[[length(flags) + 1]] <- make_flag(nm, "Sensitive target", "MEDIUM",
        "Kept for analysis; attribute-disclosure protection is not yet applied")
    }

    # Free-text detection -> MEDIUM
    if (is.character(x) && !all(is.na(x))) {
      x_obs <- x[!is.na(x)]
      mean_nchar <- mean(nchar(as.character(x_obs)))
      n_dist <- length(unique(x_obs))
      if (mean_nchar > 50 && n_dist > length(x) * 0.5) {
        flags[[length(flags) + 1]] <- make_flag(nm, "Free-text column detected", "MEDIUM",
          "Free-text columns can contain identifying information; consider exclusion")
      }
    }

    # Date columns with day precision -> LOW
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      flags[[length(flags) + 1]] <- make_flag(nm, "Date column detected", "LOW",
        "Consider coarsening dates to reduce disclosure risk")
    }

    # Geography columns -> LOW
    geo_pattern <- "(?i)(zip|postal|fsa|county|region|province|state|city|geo|lat|lon|coord)"
    if (grepl(geo_pattern, nm, perl = TRUE)) {
      flags[[length(flags) + 1]] <- make_flag(nm, "Geography column detected", "LOW",
        "Geography columns can be re-identifying; consider coarsening or aggregation")
    }
  }

  if (!is.null(disclosure_map)) {
    qi_cols <- names(disclosure_map)[disclosure_map == "quasi"]
    qi_cols <- intersect(qi_cols, names(original))
    if (length(qi_cols) >= 1L) {
      res <- assess_kanonymity(original, qi_cols, k = 5)
      if (!isTRUE(res$no_qi) && !is.na(res$smallest_cell) && res$n_below > 0L) {
        flags[[length(flags) + 1]] <- make_flag(
          "(quasi-identifiers)",
          sprintf(
            "%d record(s) (%.1f%%) in QI combinations smaller than k=5; smallest cell = %d",
            res$n_below, res$pct_below, res$smallest_cell
          ),
          "HIGH",
          "These combinations are re-identifying; synthesis will coarsen or suppress them"
        )
      }
    }
  }

  if (length(flags) == 0) {
    return(tibble::tibble(
      variable       = character(0),
      flag           = character(0),
      severity       = character(0),
      recommendation = character(0)
    ))
  }

  dplyr::bind_rows(flags)
}

# ===========================================================================
# Post-stage flags [3.7]
# ===========================================================================

privacy_check_post <- function(original, synthetic, roles, spec) {
  synthetic_match <- dg_original_names(synthetic)
  flags <- list()
  exact_row_matches <- 0L

  role_map <- NULL
  if (!is.null(roles) && "variable" %in% names(roles) &&
      "recommended_role" %in% names(roles)) {
    role_map <- stats::setNames(roles$recommended_role, roles$variable)
  }

  # 1. ID columns still present in synthetic. Fully-NA masking or scrambling
  # (no synthetic ID value equal to any original ID value) both count as
  # properly masked; only a value that survives verbatim is a genuine leak.
  for (nm in intersect(names(original), names(synthetic_match))) {
    role <- dg_named_lookup(role_map, nm); if (is.na(role)) role <- "unknown"
    if (role == "alphanumeric ID") {
      id_vals <- synthetic_match[[nm]]
      all_masked <- all(is.na(id_vals))
      orig_vals <- as.character(original[[nm]])
      no_verbatim_match <- length(orig_vals) == length(id_vals) &&
        !any(!is.na(id_vals) & as.character(id_vals) %in% orig_vals)
      if (!all_masked && !no_verbatim_match) {
        flags[[length(flags) + 1]] <- make_flag(nm,
          "ID column not fully masked in synthetic output", "HIGH",
          "ID columns should be fully masked (all-NA) or scrambled in synthetic data")
      }
    }
  }

  # 2. Exact-row match check (C8: nrow >= 20 only)
  if (nrow(original) >= 20) {
    exact_row_matches <- exact_row_match_count(original, synthetic_match, role_map)
    if (exact_row_matches > 0) {
      flags[[length(flags) + 1]] <- make_flag("(dataset)",
          sprintf("%d exact-row match(es) between synthetic and original", exact_row_matches),
          "HIGH",
          "Exact-row matches increase disclosure risk; consider re-synthesizing with different seed or settings")
    }
  }

  # 3. Rare-category survival
  if (!is.null(spec)) {
    rare_min_n <- spec$rare_level_min_n %||% 5
  } else {
    rare_min_n <- 5
  }

  cat_cols <- names(original)[vapply(original, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  cat_cols <- intersect(cat_cols, names(synthetic_match))

  for (nm in cat_cols) {
    x_orig <- as.character(original[[nm]])
    x_syn  <- as.character(synthetic_match[[nm]])
    tx <- table(x_orig[!is.na(x_orig)])
    rare_vals <- names(tx)[tx < rare_min_n & tx > 0]
    if (length(rare_vals) > 0) {
      survived <- rare_vals[rare_vals %in% x_syn[!is.na(x_syn)]]
      if (length(survived) > 0) {
        flags[[length(flags) + 1]] <- make_flag(nm,
          sprintf("Rare categories survived synthesis: %s", paste(survived, collapse = ", ")),
          "MEDIUM",
          "Rare categories may be identifying; verify they are safe to release")
      }
    }
  }

  dr <- NULL
  if (!is.null(roles) && "disclosure_role" %in% names(roles)) {
    dr <- stats::setNames(roles$disclosure_role, roles$variable)
  }
  if (!is.null(dr)) {
    k_target <- if (!is.null(spec)) spec$k_anon %||% 5 else 5
    qi_cols <- intersect(names(dr)[dr %in% "quasi"], names(synthetic_match))  # %in% is NA-safe
    if (length(qi_cols) >= 1L) {
      res <- assess_kanonymity(synthetic_match, qi_cols, k = k_target)
      if (!is.na(res$smallest_cell) && res$smallest_cell < k_target) {
        flags[[length(flags) + 1]] <- make_flag(
          "(quasi-identifiers)",
          sprintf(
            "Synthetic output has a QI cell of size %d (< k=%d)",
            res$smallest_cell, k_target
          ),
          "HIGH",
          "k-anonymity enforcement did not reach the target; review enforce_kanon settings"
        )
      }
    }
  }

  # 4. Date precision not coarsened
  if (!is.null(spec) && isTRUE(spec$coarsen_dates)) {
    date_cols <- names(original)[vapply(original, function(x) {
      inherits(x, "Date") || inherits(x, "POSIXct")
    }, logical(1))]
    date_cols <- intersect(date_cols, names(synthetic_match))
    for (nm in date_cols) {
      if (inherits(synthetic_match[[nm]], "Date")) {
        days <- unique(format(synthetic_match[[nm]], "%d"))
        days <- days[!is.na(days)]
        if (length(days) > 1 && !all(days == "01")) {
          flags[[length(flags) + 1]] <- make_flag(nm,
            "Date column retains day-level precision despite coarsen_dates = TRUE",
            "MEDIUM",
            "Check that date coarsening was properly applied during synthesis")
        }
      }
    }
  }

  if (length(flags) == 0) {
    out <- tibble::tibble(
      variable       = character(0),
      flag           = character(0),
      severity       = character(0),
      recommendation = character(0)
    )
    attr(out, "exact_row_matches") <- exact_row_matches
    return(out)
  }

  out <- dplyr::bind_rows(flags)
  attr(out, "exact_row_matches") <- exact_row_matches
  out
}

# ===========================================================================
# Helpers
# ===========================================================================

make_flag <- function(variable, flag, severity, recommendation) {
  tibble::tibble(
    variable       = variable,
    flag           = flag,
    severity       = severity,
    recommendation = recommendation
  )
}

# The columns an exact-row match is computed over: every column present in
# both frames, minus alphanumeric-ID columns (those are regenerated by design,
# so including them would mask real record reproduction). Single source of
# truth -- the flags, the count and the per-column breakdown all call this, so
# they cannot drift apart.
exact_match_columns <- function(original, synthetic, role_map = NULL) {
  common_cols <- intersect(names(original), names(synthetic))
  id_cols <- character(0)
  if (!is.null(role_map)) {
    id_cols <- names(role_map)[role_map == "alphanumeric ID"]
  }
  setdiff(common_cols, id_cols)
}

# Columns the user answered "yes" to in question 2 (Is it sensitive?) on the
# Configure step. `sensitive` is the synced axis that dg_sync_roles_axes()
# maintains from the user's answer, so it already reflects any override.
dg_sensitive_columns <- function(roles) {
  if (is.null(roles) || !"variable" %in% names(roles) ||
      !"sensitive" %in% names(roles)) {
    return(character(0))
  }
  as.character(roles$variable[!is.na(roles$sensitive) & roles$sensitive])
}

# Per-row exact-match flags between original and synthetic, on the same
# columns the count uses (shared columns minus alphanumeric-ID columns), and
# only when the original has >= 20 rows. Returns a list with two logical
# vectors: `original` (which original rows are reproduced verbatim in the
# synthetic output) and `synthetic` (which synthetic rows are verbatim copies
# of an original row). `sum(<result>$synthetic)` equals exact_row_match_count()
# by construction, so highlight and stat box can never disagree.
exact_row_match_flags <- function(original, synthetic, role_map = NULL) {
  empty <- list(
    original  = rep(FALSE, nrow(original)),
    synthetic = rep(FALSE, nrow(synthetic))
  )
  if (nrow(original) < 20 || nrow(synthetic) == 0) {
    return(empty)
  }

  match_cols <- exact_match_columns(original, synthetic, role_map)

  if (length(match_cols) == 0) {
    return(empty)
  }

  orig_key <- row_key(original[, match_cols, drop = FALSE])
  syn_key <- row_key(synthetic[, match_cols, drop = FALSE])
  list(
    original  = unname(orig_key %in% syn_key),
    synthetic = unname(syn_key %in% orig_key)
  )
}

exact_row_match_count <- function(original, synthetic, role_map = NULL) {
  as.integer(sum(exact_row_match_flags(original, synthetic, role_map)$synthetic))
}

# Long-form breakdown of the exact-row matches, for the "Exact matches" tab in
# the data panel: one row per (matched synthetic row x match column), so the
# user can jump straight to the cell rather than eyeballing a wide table.
#
# A match is a *whole-row* collision, so every match column participates in
# every match -- there is no single "offending column". What does vary per row
# is whether the sensitive columns actually carry a value there: k-anonymity
# suppression blanks cells, and a reproduced row whose sensitive cells are all
# NA discloses nothing sensitive. `discloses` captures exactly that, and is
# what the red highlight and the export gate key off.
#
# Returns a list: `breakdown` (the long table), plus `original_severity` and
# `synthetic_severity` integer vectors aligned to each frame's rows --
# 0 = no match, 1 = match with no sensitive value exposed, 2 = match exposing
# at least one sensitive value.
exact_match_detail <- function(original, synthetic, roles = NULL,
                               role_map = NULL) {
  breakdown <- data.frame(
    column         = character(0),
    synthetic_row  = integer(0),
    original_row   = integer(0),
    sensitive      = logical(0),
    value          = character(0),
    stringsAsFactors = FALSE
  )
  empty <- list(
    breakdown          = breakdown,
    original_severity  = rep(0L, nrow(original)),
    synthetic_severity = rep(0L, nrow(synthetic))
  )

  # Reuse the shipped flags rather than recomputing the match, so this view can
  # never report a different set of rows than the EXACT MATCHES stat box.
  flags <- exact_row_match_flags(original, synthetic, role_map)
  if (!any(flags$synthetic)) {
    return(empty)
  }

  match_cols <- exact_match_columns(original, synthetic, role_map)
  sens_cols <- intersect(dg_sensitive_columns(roles), match_cols)

  orig_key <- row_key(original[, match_cols, drop = FALSE])
  syn_key <- row_key(synthetic[, match_cols, drop = FALSE])

  syn_rows <- which(flags$synthetic)
  # First original row carrying the same key; the row numbers shown to the user
  # are 1-based positions in the previewed tables.
  orig_rows <- match(syn_key[syn_rows], orig_key)

  as_text <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    out
  }

  n_col <- length(match_cols)
  breakdown <- data.frame(
    column        = rep(match_cols, times = length(syn_rows)),
    synthetic_row = rep(syn_rows, each = n_col),
    original_row  = rep(orig_rows, each = n_col),
    stringsAsFactors = FALSE
  )
  breakdown$sensitive <- breakdown$column %in% sens_cols
  breakdown$value <- vapply(
    seq_len(nrow(breakdown)),
    function(i) {
      v <- as_text(synthetic[[breakdown$column[i]]][breakdown$synthetic_row[i]])
      if (is.na(v)) NA_character_ else v
    },
    character(1)
  )

  # A row "discloses" only if at least one of its sensitive cells is populated.
  disclosing_syn <- if (length(sens_cols) == 0) {
    integer(0)
  } else {
    is_disclosing <- breakdown$sensitive & !is.na(breakdown$value)
    unique(breakdown$synthetic_row[is_disclosing])
  }

  syn_sev <- integer(nrow(synthetic))
  syn_sev[syn_rows] <- 1L
  syn_sev[disclosing_syn] <- 2L

  # Carry the same severity back to the original rows that were reproduced.
  # Match on the row key, not on `orig_rows` -- several synthetic rows can share
  # one key, and match() only reports the first original hit, which would leave
  # the remaining identical original rows marked amber despite being disclosed.
  orig_sev <- integer(nrow(original))
  orig_sev[flags$original] <- 1L
  disclosing_keys <- unique(syn_key[syn_sev == 2L])
  orig_sev[orig_key %in% disclosing_keys] <- 2L

  list(
    breakdown          = breakdown,
    original_severity  = orig_sev,
    synthetic_severity = syn_sev
  )
}

# Number of reproduced synthetic rows that expose at least one sensitive value.
# This is the quantity the browser export gate blocks on.
exact_match_sensitive_count <- function(detail) {
  if (is.null(detail)) {
    return(0L)
  }
  as.integer(sum(detail$synthetic_severity == 2L))
}

dg_original_names <- function(synthetic) {
  spec <- attr(synthetic, "spec", exact = TRUE)
  name_map <- spec$name_map %||% NULL
  if (is.null(name_map) || !length(name_map)) {
    return(synthetic)
  }
  reverse_map <- stats::setNames(names(name_map), unname(name_map))
  out <- synthetic
  mapped <- names(out) %in% names(reverse_map)
  names(out)[mapped] <- unname(reverse_map[names(out)[mapped]])
  out
}

augment_synthpop_disclosure <- function(flags, original, synthetic, roles) {
  if (!identical(attr(synthetic, "engine", exact = TRUE), "synthpop")) {
    return(flags)
  }

  # Match on original names so generic/dictionary_only renaming does not
  # silently drop the disclosure metrics (same policy as privacy_check_post).
  disclosure <- synthpop_disclosure_panel(original, dg_original_names(synthetic), roles)
  if (is.null(disclosure)) {
    return(flags)
  }

  rows <- synthpop_disclosure_flags(disclosure)
  if (nrow(rows) > 0L) {
    flags <- dplyr::bind_rows(flags, rows)
  }
  attr(flags, "synthpop_disclosure") <- disclosure
  flags
}

synthpop_disclosure_panel <- function(original, synthetic, roles) {
  if (!requireNamespace("synthpop", quietly = TRUE)) {
    return(NULL)
  }

  qi_cols <- synthpop_disclosure_cols(roles)
  qi_cols <- intersect(qi_cols, intersect(names(original), names(synthetic)))
  if (length(qi_cols) < 2L) {
    return(NULL)
  }

  target <- qi_cols[[length(qi_cols)]]
  keys <- setdiff(qi_cols, target)
  if (length(keys) == 0L) {
    return(NULL)
  }

  # synthpop::disclosure() does base `data[, j]` column extraction internally,
  # which returns sub-tibbles (not vectors) for tbl_df input and fails with
  # "cannot xtfrm data frames"; coerce to plain data.frame first.
  original_qi <- as.data.frame(original[, qi_cols, drop = FALSE])
  synthetic_qi <- as.data.frame(synthetic[, qi_cols, drop = FALSE])

  # Keep disclosure() scoped to role-flagged QI columns; sample rows later if
  # this is still too costly on very large data.
  result <- tryCatch(
    synthpop::disclosure(
      synthetic_qi,
      original_qi,
      keys = keys,
      target = target,
      print.flag = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(NULL)
  }

  list(
    keys = keys,
    target = target,
    identity_repu = disclosure_numeric(result$ident, "repU"),
    attribute_disco = disclosure_numeric(result$attrib, "DiSCO"),
    raw = result
  )
}

synthpop_disclosure_cols <- function(roles) {
  if (is.null(roles) || !"variable" %in% names(roles)) {
    return(character())
  }

  role <- if ("recommended_role" %in% names(roles)) {
    roles$recommended_role
  } else {
    rep(NA_character_, nrow(roles))
  }

  disclosure <- if ("disclosure_role" %in% names(roles)) {
    roles$disclosure_role
  } else {
    rep("none", nrow(roles))
  }

  roles$variable[
    role %in% c("alphanumeric ID", "date", "categorical candidate", "label_check") |
      disclosure %in% c("quasi", "direct", "sensitive")
  ]
}

synthpop_disclosure_flags <- function(disclosure) {
  rows <- list()
  if (!is.na(disclosure$identity_repu)) {
    rows[[length(rows) + 1L]] <- make_flag(
      "(synthpop disclosure)",
      sprintf("Identity disclosure repU: %.2f", disclosure$identity_repu),
      "LOW",
      "Review synthpop disclosure metrics before sharing relationship-preserving synthetic data"
    )
  }
  if (!is.na(disclosure$attribute_disco)) {
    rows[[length(rows) + 1L]] <- make_flag(
      "(synthpop disclosure)",
      sprintf("Attribute disclosure DiSCO: %.2f", disclosure$attribute_disco),
      "LOW",
      "Review synthpop disclosure metrics before sharing relationship-preserving synthetic data"
    )
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      variable       = character(0),
      flag           = character(0),
      severity       = character(0),
      recommendation = character(0)
    ))
  }

  dplyr::bind_rows(rows)
}

disclosure_numeric <- function(x, name) {
  if (is.null(x)) {
    return(NA_real_)
  }

  if (is.data.frame(x) || is.matrix(x)) {
    nms <- colnames(x)
    idx <- which(tolower(nms) == tolower(name))
    if (length(idx)) {
      return(as.numeric(x[1, idx[[1]]]))
    }
  }

  if (is.list(x) && !is.null(names(x))) {
    idx <- which(tolower(names(x)) == tolower(name))
    if (length(idx)) {
      return(as.numeric(x[[idx[[1]]]][[1]]))
    }
  }

  NA_real_
}

# ===========================================================================
# Print method [3.8]
# ===========================================================================

#' @export
print.dataganger_privacy_check <- function(x, ...) {
  cli::cli_h1("DataGangeR Privacy Check ({attr(x, \"stage\")} stage)")

  if (nrow(x) == 0) {
    cli::cli_alert_success("No flags raised.")
    return(invisible(x))
  }

  sevs <- c("HIGH", "MEDIUM", "LOW")
  for (s in sevs) {
    rows <- x[x$severity == s, ]
    if (nrow(rows) == 0) next

    icon <- switch(s, HIGH = "x", MEDIUM = "!", LOW = "i")
    header <- sprintf("%s %s severity (%d)", icon, s, nrow(rows))
    cli::cli_h2(header)

    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      cli::cli_li("{.field {r$variable}}: {r$flag}")
      cli::cli_text("  {.emph Recommendation}: {r$recommendation}")
    }
  }

  invisible(x)
}
