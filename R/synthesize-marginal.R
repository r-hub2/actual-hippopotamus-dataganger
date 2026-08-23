# Marginal synthesis (Level 2)
#
# Internal function. Synthesizes each column independently using
# the helpers from synth-helpers.R. Honors spec parameters for
# missingness, rare-level merging, date coarsening, and free text.

dg_effective_label_strategy <- function(spec, roles = NULL, idx = NA_integer_) {
  label_strategy <- NA_character_
  if (!is.null(roles) && !is.na(idx) && "label_strategy" %in% names(roles)) {
    label_strategy <- roles$label_strategy[[idx]]
  }
  if (is.na(label_strategy) || !nzchar(label_strategy)) {
    label_strategy <- spec$label_strategy %||% "preserve"
  }
  validate_label_strategy(label_strategy)
  label_strategy
}

synthesize_marginal <- function(data, spec, roles = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  n <- spec$n %||% nrow(data)

  if (n == 0) {
    # 0-row request: return schema
    return(synthesize_schema(data, spec, roles))
  }

  rare_min_n <- spec$rare_level_min_n %||% 5
  # FALSE matches every purpose preset and the synth_categorical() default;
  # a TRUE fallback here would merge rare labels into ".other" for any spec
  # that omits the field.
  merge_rare  <- spec$merge_rare %||% FALSE
  coarsen     <- spec$coarsen_dates %||% TRUE
  missingness <- spec$preserve_missingness %||% "approx"
  free_text_s <- spec$free_text_strategy %||% "categorical"

  # Build role lookup if available
  role_lookup <- NULL
  if (!is.null(roles) && "variable" %in% names(roles) &&
      "recommended_role" %in% names(roles)) {
    role_lookup <- stats::setNames(roles$recommended_role, roles$variable)
  }

  cols <- vector("list", ncol(data))
  names(cols) <- names(data)

  # --- remove_ids guard (C11 privacy hardening) ---
  if (isTRUE(spec$remove_ids) && !is.null(role_lookup)) {
    id_cols <- names(role_lookup)[role_lookup == "alphanumeric ID"]
    if (length(id_cols) > 0) {
      cli::cli_inform(c(
        "i" = "{.arg remove_ids} is TRUE: masking {length(id_cols)} ID column{?s}",
        " " = "{.val {id_cols}}"
      ))
    }
  }

  for (i in seq_len(ncol(data))) {
    col_name <- names(data)[i]
    x <- data[[i]]

    # Determine role for this column
    role <- dg_named_lookup(role_lookup, col_name); if (is.na(role)) role <- "unknown"
    idx <- if (!is.null(roles) && "variable" %in% names(roles)) match(col_name, roles$variable) else NA_integer_
    label_strategy <- dg_effective_label_strategy(spec, roles, idx)

    # remove_ids: mask ID columns with NA
    if (isTRUE(spec$remove_ids) && !is.null(role_lookup) &&
        role == "alphanumeric ID") {
      cols[[i]] <- typed_missing_vector(x, n)
      next
    }

    # Free text handling. Trust the detected role when we have one \u2014 detect_roles
    # already ran is_free_text_candidate (its free-text test precedes every other
    # role), so a column carrying any concrete role was already found not to be
    # free text. Only probe directly when the role is unknown, to avoid
    # recomputing the free-text heuristic on every non-free-text column.
    # "free text" stays a distinct detection label, but internally it is
    # synthesized the same way as any other categorical column (see
    # synth_free_text()'s "categorical" strategy) unless drop/redact is
    # explicitly requested (e.g. by privacy hardening).
    if (role == "free text" || (role == "unknown" && is_free_text_candidate(x))) {
      cols[[i]] <- synth_free_text(x, n,
        strategy = free_text_s,
        rare_level_min_n = rare_min_n,
        merge_rare = merge_rare,
        missing_strategy = missingness,
        label_strategy = label_strategy
      )
      next
    }

    # Dispatch by type
    if (all(is.na(x))) {
      cols[[i]] <- typed_missing_vector(x, n)
      next
    }

    # Character-stored date/time strings (e.g. "01/08/2020", "Jun 8, 2019",
    # a full datetime, or a bare time). detect_roles() already recognised
    # these as role "date", but the column is still is.character(x) -- without
    # this branch it would fall into the generic character dispatch below and
    # get resampled as plain categorical text instead of synthesized as a
    # date/time. A date role without a validated parser is an internal contract
    # violation and must not silently fall through to categorical resampling.
    if (role == "date" && is.character(x)) {
      date_info <- parse_date_like_character(x)
      if (is.null(date_info)) {
        cli::cli_abort(
          "Column {.val {col_name}} has a date role but no validated date/time format."
        )
      }
      cols[[i]] <- synth_date_like_character(x, n, date_info,
        coarsen_dates = coarsen,
        missing_strategy = missingness
      )
      next
    }

    is_user_postal <- !is.null(roles) && !is.na(idx) && "user_role" %in% names(roles) &&
      identical(roles$user_role[[idx]], "postal_code")
    if (is.character(x) && (identical(role, "postal code") || is_user_postal)) {
      postal_strategy <- NA_character_
      postal_country <- NA_character_
      if (!is.null(roles) && !is.na(idx) && "postal_strategy" %in% names(roles)) {
        postal_strategy <- roles$postal_strategy[[idx]]
        postal_country <- roles$postal_country[[idx]]
      }
      if (is.na(postal_strategy) || !nzchar(postal_strategy)) {
        postal_strategy <- "generate"
      }
      postal_info <- detect_postal_format(x, country_hint = postal_country)
      if (!is.null(postal_info)) {
        cols[[i]] <- if (identical(postal_strategy, "resample")) {
          synth_postal_code_resample(x, n, missing_strategy = missingness)
        } else {
          synth_postal_code_generate(x, n, postal_info, missing_strategy = missingness)
        }
        next
      }
    }

    if (is.numeric(x)) {
      cols[[i]] <- synth_numeric(x, n, missing_strategy = missingness)
    } else if (is.character(x)) {
      cols[[i]] <- synth_character(x, n,
        rare_level_min_n = rare_min_n,
        merge_rare = merge_rare,
        missing_strategy = missingness,
        label_strategy = label_strategy
      )
    } else if (inherits(x, "Date")) {
      cols[[i]] <- synth_date(x, n,
        coarsen_dates = coarsen,
        missing_strategy = missingness
      )
    } else if (inherits(x, "POSIXct")) {
      # POSIXct: sample uniformly within observed range
      cols[[i]] <- synth_posixct(x, n,
        coarsen_dates = coarsen,
        missing_strategy = missingness
      )
    } else if (is.logical(x)) {
      cols[[i]] <- synth_logical(x, n, missing_strategy = missingness)
    } else {
      # Fallback: treat as character
      cli::cli_warn(c(
        "Column {.val {col_name}} has unrecognised type; treating as character"
      ))
      cols[[i]] <- synth_character(as.character(x), n,
        rare_level_min_n = rare_min_n,
        merge_rare = merge_rare,
        missing_strategy = missingness,
        label_strategy = label_strategy
      )
    }
  }

  out <- tibble::as_tibble(cols)
  if (missingness == "exact") {
    mask <- build_na_mask(data, n)
    out  <- apply_joint_mask(out, mask)
  }
  out
}

# POSIXct synthesis helper
synth_posixct <- function(x, n, coarsen_dates = TRUE, missing_strategy = "approx") {
  if (all(is.na(x))) {
    return(rep(as.POSIXct(NA), n))
  }

  x_obs <- x[!is.na(x)]
  min_ts <- min(x_obs)
  max_ts <- max(x_obs)

  range_secs <- as.numeric(difftime(max_ts, min_ts, units = "secs"))
  if (range_secs <= 0) {
    synth <- rep(min_ts, n)
  } else {
    synth <- min_ts + stats::runif(n, min = 0, max = range_secs)
  }

  if (isTRUE(coarsen_dates)) {
    synth <- coarsen_posixct_to_day(synth)
  }

  synth <- apply_missingness(synth, x, n, missing_strategy)
  synth
}

coarsen_posixct_to_day <- function(ts) {
  tz <- attr(ts, "tzone") %||% ""
  as.POSIXct(format(ts, "%Y-%m-%d", tz = tz), tz = tz)
}

typed_missing_vector <- function(x, n) {
  if (inherits(x, "Date")) {
    return(rep(as.Date(NA), n))
  }
  if (inherits(x, "POSIXct")) {
    return(rep(as.POSIXct(NA, tz = attr(x, "tzone") %||% "UTC"), n))
  }
  if (is.numeric(x)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(x)) {
    return(rep(NA, n))
  }
  rep(NA_character_, n)
}
