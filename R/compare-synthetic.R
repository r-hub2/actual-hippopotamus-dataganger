#' Map a fidelity p-value to a colour band.
#'
#' Lower p = a more significant original-vs-synthetic difference = poorer
#' fidelity. `NA` means no inference was run (min/max) -> "none".
#' @keywords internal
#' @noRd
fidelity_color <- function(p) {
  if (length(p) != 1L || is.na(p)) return("none")
  if (p < 0.01) return("bad")
  if (p < 0.05) return("warn")
  "good"
}

#' Compare original and synthetic datasets
#'
#' Compares an original dataset with its synthetic double across dataset-level
#' dimensions, numeric distributions, categorical distributions, and numeric
#' correlations. Returns a structured `dataganger_comparison` object.
#'
#' @param original The original data frame.
#' @param synthetic The synthetic data frame (from [synthesize_data()]).
#' @param roles Optional; a `dataganger_roles` object from [detect_roles()].
#'
#' @return An S3 object of class `dataganger_comparison`, a list with
#'   components `dataset`, `numeric`, `categorical`, `relationship`, `interaction`,
#'   `privacy_flags`, and `meta`.
#' @export
#'
#' @examples
#' dat <- data.frame(x = 1:10, y = letters[1:10])
#' spec <- synth_spec(purpose = "demo")
#' syn <- synthesize_data(dat, spec)
#' compare_synthetic(dat, syn)
compare_synthetic <- function(original, synthetic, roles = NULL) {
  if (!is.data.frame(original)) {
    cli::cli_abort("{.arg original} must be a data frame")
  }
  if (!is.data.frame(synthetic)) {
    cli::cli_abort("{.arg synthetic} must be a data frame")
  }
  synthetic_for_match <- dg_original_names(synthetic)

  # -- dataset-level --
  ds <- compare_dataset(original, synthetic_for_match)

  # -- numeric comparison --
  num_cmp <- compare_numeric(original, synthetic_for_match)

  # -- categorical comparison --
  cat_cmp <- compare_categorical(original, synthetic_for_match)

  # -- relationship (correlation) --
  rel_cmp <- compare_relationship(original, synthetic_for_match)

  # -- relationship modification (interaction) --
  int_cmp <- compare_relationship_interaction(original, synthetic_for_match, roles)

  out <- list(
    dataset       = ds,
    numeric       = num_cmp,
    categorical   = cat_cmp,
    relationship  = rel_cmp,
    interaction   = int_cmp,
    privacy_flags = NULL,
    meta          = list(
      generated_at = Sys.time(),
      nrow_orig    = nrow(original),
      ncol_orig    = ncol(original),
      nrow_syn     = nrow(synthetic),
      ncol_syn     = ncol(synthetic)
    )
  )
  class(out) <- "dataganger_comparison"
  out
}

# ===========================================================================
# Dataset-level comparison
# ===========================================================================

compare_dataset <- function(orig, syn) {
  common_cols <- intersect(names(orig), names(syn))
  type_match <- vapply(common_cols, function(nm) {
    identical(class(orig[[nm]]), class(syn[[nm]]))
  }, logical(1))

  miss_orig <- sum(is.na(orig)) / (nrow(orig) * ncol(orig)) * 100
  miss_syn  <- sum(is.na(syn))  / (nrow(syn)  * ncol(syn))  * 100

  tibble::tibble(
    metric             = c("nrow", "ncol", "n_common_cols", "type_match_pct",
                           "missing_orig_pct", "missing_syn_pct"),
    original           = c(nrow(orig), ncol(orig), length(common_cols),
                           NA_real_, miss_orig, NA_real_),
    synthetic          = c(nrow(syn),  ncol(syn),  NA_integer_,
                           NA_real_, NA_real_, miss_syn),
    value              = c(NA_real_, NA_real_, NA_real_,
                            if (length(type_match) == 0) NA_real_ else sum(type_match) / length(type_match) * 100,
                           NA_real_, NA_real_)
  )
}

# ===========================================================================
# Numeric comparison
# ===========================================================================

#' Safe two-sample test p-value.
#'
#' Returns `NA_real_` instead of erroring on degenerate input.
#' @keywords internal
#' @noRd
safe_test_p <- function(expr) {
  tryCatch(suppressWarnings(expr$p.value), error = function(e) NA_real_)
}

#' Distributional p-value for one categorical column (original vs synthetic).
#'
#' Tests whether the level frequencies differ between the two samples. Uses
#' Pearson's chi-square on the 2 x k contingency table; when the table is small
#' enough, falls back to Fisher's exact test (more reliable with sparse cells).
#' Lower p = a more significant original-vs-synthetic difference. `NA` when there
#' is nothing to test.
#' @keywords internal
#' @noRd
safe_categorical_p <- function(x_obs, y_obs, all_levels) {
  if (length(x_obs) < 1L || length(y_obs) < 1L || length(all_levels) < 2L) {
    return(NA_real_)
  }
  cx <- as.numeric(table(factor(x_obs, levels = all_levels)))
  cy <- as.numeric(table(factor(y_obs, levels = all_levels)))
  tab <- rbind(cx, cy)
  tab <- tab[, colSums(tab) > 0, drop = FALSE]
  if (ncol(tab) < 2L) return(NA_real_)

  # Fisher is more reliable on small/sparse tables; chi-square scales better.
  small <- sum(tab) <= 200L && ncol(tab) <= 6L
  if (small) {
    p <- tryCatch(
      suppressWarnings(stats::fisher.test(tab)$p.value),
      error = function(e) NA_real_
    )
    if (!is.na(p)) return(p)
  }
  p <- tryCatch(
    suppressWarnings(stats::chisq.test(tab)$p.value),
    error = function(e) NA_real_
  )
  if (is.na(p)) {
    p <- tryCatch(
      suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000)$p.value),
      error = function(e) NA_real_
    )
  }
  p
}

compare_numeric <- function(orig, syn) {
  num_cols <- names(orig)[vapply(orig, function(x) {
    is.numeric(x) && !haven::is.labelled(x)
  }, logical(1))]
  num_cols <- intersect(num_cols, names(syn))
  num_cols <- num_cols[vapply(num_cols, function(nm) is.numeric(syn[[nm]]), logical(1))]

  if (length(num_cols) == 0) {
    return(tibble::tibble(
      variable = character(0), mean_orig = double(0), mean_syn = double(0),
      sd_orig = double(0), sd_syn = double(0),
      median_orig = double(0), median_syn = double(0),
      iqr_orig = double(0), iqr_syn = double(0),
      missing_orig_pct = double(0), missing_syn_pct = double(0),
      std_diff = double(0), sd_ratio = double(0),
      median_std_diff = double(0),
      mean_p = double(0), sd_p = double(0), median_p = double(0)
    ))
  }

  out <- lapply(num_cols, function(nm) {
    x <- orig[[nm]]
    y <- syn[[nm]]

    x_obs <- x[!is.na(x)]
    y_obs <- y[!is.na(y)]

    if (length(x_obs) == 0) {
      return(tibble::tibble(
        variable = nm, mean_orig = NA_real_, mean_syn = mean(y_obs),
        sd_orig = NA_real_, sd_syn = stats::sd(y_obs),
        median_orig = NA_real_, median_syn = stats::median(y_obs),
        iqr_orig = NA_real_, iqr_syn = stats::IQR(y_obs),
        missing_orig_pct = 100, missing_syn_pct = sum(is.na(y)) / length(y) * 100,
        std_diff = NA_real_, sd_ratio = NA_real_,
        median_std_diff = NA_real_,
        mean_p = NA_real_, sd_p = NA_real_, median_p = NA_real_
      ))
    }

    mean_o <- mean(x_obs)
    sd_o   <- stats::sd(x_obs)
    mean_s <- if (length(y_obs) > 0) mean(y_obs) else NA_real_
    sd_s   <- if (length(y_obs) > 0) stats::sd(y_obs) else NA_real_

    std_diff <- if (sd_o > 0 && length(y_obs) > 0) {
      (mean_s - mean_o) / sd_o
    } else {
      NA_real_
    }

    iqr_o <- stats::IQR(x_obs)
    median_std_diff <- if (iqr_o > 0 && length(y_obs) > 0) {
      (stats::median(y_obs) - stats::median(x_obs)) / iqr_o
    } else {
      NA_real_
    }

    tibble::tibble(
      variable       = nm,
      mean_orig      = mean_o,
      mean_syn       = mean_s,
      sd_orig        = sd_o,
      sd_syn         = sd_s,
      median_orig    = stats::median(x_obs),
      median_syn     = if (length(y_obs) > 0) stats::median(y_obs) else NA_real_,
      iqr_orig       = stats::IQR(x_obs),
      iqr_syn        = if (length(y_obs) > 0) stats::IQR(y_obs) else NA_real_,
      missing_orig_pct = sum(is.na(x)) / length(x) * 100,
      missing_syn_pct  = sum(is.na(y)) / length(y) * 100,
      std_diff       = std_diff,
      sd_ratio       = if (!is.na(sd_o) && sd_o > 0 && length(y_obs) > 0) sd_s / sd_o else NA_real_,
      median_std_diff = median_std_diff,
      mean_p         = if (length(y_obs) > 1 && length(x_obs) > 1) safe_test_p(stats::t.test(x_obs, y_obs)) else NA_real_,
      sd_p           = if (length(y_obs) > 1 && length(x_obs) > 1) safe_test_p(stats::var.test(x_obs, y_obs)) else NA_real_,
      median_p       = if (length(y_obs) > 1 && length(x_obs) > 1) safe_test_p(stats::wilcox.test(x_obs, y_obs)) else NA_real_
    )
  })

  dplyr::bind_rows(out)
}

# ===========================================================================
# Categorical comparison
# ===========================================================================

compare_categorical <- function(orig, syn) {
  cat_types <- c("character", "factor", "logical")
  cat_cols <- names(orig)[vapply(orig, function(x) {
    any(cat_types %in% class(x)) || is.character(x) || is.factor(x) || is.logical(x) || haven::is.labelled(x)
  }, logical(1))]
  cat_cols <- intersect(cat_cols, names(syn))

  if (length(cat_cols) == 0) {
    return(tibble::tibble(
      variable = character(0), n_levels_orig = integer(0),
      n_levels_syn = integer(0), top_5_orig = character(0),
      top_5_syn = character(0), missing_orig_pct = double(0),
      missing_syn_pct = double(0), tvd = double(0), dist_p = double(0)
    ))
  }

  out <- lapply(cat_cols, function(nm) {
    x <- as.character(orig[[nm]])
    y <- as.character(syn[[nm]])

    x_obs <- x[!is.na(x)]
    y_obs <- y[!is.na(y)]

    tx <- sort(table(x_obs), decreasing = TRUE)
    ty <- sort(table(y_obs), decreasing = TRUE)

    top5_orig <- paste(utils::head(names(tx), 5), collapse = "; ")
    top5_syn  <- paste(utils::head(names(ty), 5), collapse = "; ")

    # total variation distance
    all_levels <- union(names(tx), names(ty))
    px <- stats::setNames(rep(0, length(all_levels)), all_levels)
    py <- stats::setNames(rep(0, length(all_levels)), all_levels)
    if (length(x_obs) > 0) px[names(tx)] <- as.numeric(tx) / length(x_obs)
    if (length(y_obs) > 0) py[names(ty)] <- as.numeric(ty) / length(y_obs)
    tvd <- 0.5 * sum(abs(px - py))

    tibble::tibble(
      variable        = nm,
      n_levels_orig   = length(tx),
      n_levels_syn    = length(ty),
      top_5_orig      = top5_orig,
      top_5_syn       = top5_syn,
      missing_orig_pct = sum(is.na(x)) / length(x) * 100,
      missing_syn_pct  = sum(is.na(y)) / length(y) * 100,
      tvd             = tvd,
      dist_p          = safe_categorical_p(x_obs, y_obs, all_levels)
    )
  })

  dplyr::bind_rows(out)
}

# ===========================================================================
# Relationship comparison (Pearson correlations)
# ===========================================================================

#' Test whether a predictor-outcome relationship changes in synthetic data.
#'
#' @param x_orig,y_orig Predictor and outcome vectors from the original data.
#' @param x_synth,y_synth Predictor and outcome vectors from the synthetic data.
#' @param kind_x,kind_y Effective variable kinds. Dates are numeric and logical
#'   variables are categorical.
#' @return A list describing the interaction estimate and joint test.
#' @keywords internal
#' @noRd
relationship_interaction <- function(x_orig, y_orig, x_synth, y_synth,
                                     kind_x, kind_y) {
  normalize_kind <- function(kind) {
    if (identical(kind, "date")) return("numeric")
    if (identical(kind, "logical")) return("categorical")
    kind
  }
  kind_x <- normalize_kind(kind_x)
  kind_y <- normalize_kind(kind_y)

  numeric_base <- function(x) {
    if (is.factor(x) || is.character(x)) {
      suppressWarnings(as.numeric(as.character(x)))
    } else {
      suppressWarnings(as.numeric(x))
    }
  }
  numeric_compatible <- function(x) {
    converted <- numeric_base(x)
    all(is.na(x) | !is.na(converted))
  }
  if (kind_x == "numeric" &&
      (!numeric_compatible(x_orig) || !numeric_compatible(x_synth))) {
    kind_x <- "categorical"
  }
  if (kind_y == "numeric" &&
      (!numeric_compatible(y_orig) || !numeric_compatible(y_synth))) {
    kind_y <- "categorical"
  }

  if (kind_x == "numeric" && kind_y == "numeric") {
    continuous_empty <- function(note, n = 0L) {
      list(
        family = "continuous", effect_label = "Difference in correlation",
        estimate = NA_real_, null_value = 0, p_value = NA_real_,
        n_terms = 0L, n = as.integer(n), note = note
      )
    }
    pair_stats <- function(x, y) {
      dat <- data.frame(x = numeric_base(x), y = numeric_base(y))
      dat <- dat[stats::complete.cases(dat), , drop = FALSE]
      if (nrow(dat) < 4L) return(NULL)
      if (length(unique(dat$x)) < 2L || length(unique(dat$y)) < 2L) return(NULL)
      list(n = nrow(dat), r = stats::cor(dat$x, dat$y))
    }
    orig <- pair_stats(x_orig, y_orig)
    synth <- pair_stats(x_synth, y_synth)
    n_total <- sum(vapply(list(orig, synth), function(z) z$n %||% 0L, integer(1)))
    if (is.null(orig) || is.null(synth)) {
      return(continuous_empty("too few complete rows or no variation", n_total))
    }
    limit <- 1 - sqrt(.Machine$double.eps)
    r_orig <- max(-limit, min(limit, orig$r))
    r_synth <- max(-limit, min(limit, synth$r))
    z <- (atanh(r_synth) - atanh(r_orig)) /
      sqrt(1 / (orig$n - 3) + 1 / (synth$n - 3))
    return(list(
      family = "continuous", effect_label = "Difference in correlation",
      estimate = r_synth - r_orig, null_value = 0,
      p_value = 2 * stats::pnorm(-abs(z)), n_terms = 1L,
      n = as.integer(orig$n + synth$n), note = ""
    ))
  }

  base_vector <- function(x, kind) {
    if (kind == "numeric") return(numeric_base(x))
    if (inherits(x, "haven_labelled") && requireNamespace("haven", quietly = TRUE)) {
      return(as.character(haven::as_factor(x, levels = "labels")))
    }
    as.character(x)
  }
  x <- c(base_vector(x_orig, kind_x), base_vector(x_synth, kind_x))
  y <- c(base_vector(y_orig, kind_y), base_vector(y_synth, kind_y))
  s <- c(rep.int(0, length(x_orig)), rep.int(1, length(x_synth)))
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  s <- s[keep]
  if (kind_x == "numeric") x <- as.numeric(x) else x <- droplevels(factor(x))
  if (kind_y == "numeric") y <- as.numeric(y) else y <- droplevels(factor(y))

  y_values <- unique(y)
  is_binary <- length(y_values) == 2L
  is_count <- kind_y == "numeric" && length(y_values) > 0L &&
    all(y >= 0 & y == round(y))
  family <- if (is_binary) {
    "binary"
  } else if (kind_y != "numeric") {
    "multinomial"
  } else if (is_count) {
    "count"
  } else {
    "continuous"
  }
  labels <- c(
    binary = "Odds ratio", count = "Slope ratio",
    continuous = "Difference in slope", multinomial = "Joint interaction"
  )
  nulls <- c(binary = 1, count = 1, continuous = 0, multinomial = NA_real_)
  empty_result <- function(note, n_terms = 0L) {
    list(
      family = family, effect_label = unname(labels[[family]]),
      estimate = NA_real_, null_value = unname(nulls[[family]]),
      p_value = NA_real_, n_terms = as.integer(n_terms), n = length(y),
      note = note
    )
  }

  if (length(y) < 10L || any(tabulate(s + 1L, nbins = 2L) < 4L)) {
    return(empty_result("too few complete rows"))
  }
  if (length(unique(x)) < 2L) return(empty_result("predictor has no variation"))
  if (length(y_values) < 2L) return(empty_result("outcome has no variation"))
  for (group in 0:1) {
    if (length(unique(x[s == group])) < 2L) {
      return(empty_result("a data group has fewer than two predictor values"))
    }
    if (length(unique(y[s == group])) < 2L) {
      return(empty_result("a data group has a degenerate outcome"))
    }
  }

  if (family == "binary") y <- as.integer(factor(y)) - 1L
  dat <- data.frame(Y = y, X = x, S = s)
  warning_seen <- FALSE
  guarded <- function(expr) {
    tryCatch(
      withCallingHandlers(
        expr,
        warning = function(w) {
          warning_seen <<- TRUE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) NULL
    )
  }

  reduced_formula <- stats::as.formula("Y ~ X + S")
  full_formula <- stats::as.formula("Y ~ X * S")
  if (family == "multinomial") {
    if (!requireNamespace("nnet", quietly = TRUE)) {
      return(empty_result("not estimable (install nnet)"))
    }
    reduced <- guarded(nnet::multinom(reduced_formula, data = dat, trace = FALSE))
    full <- guarded(nnet::multinom(full_formula, data = dat, trace = FALSE))
    if (is.null(reduced) || is.null(full) || warning_seen ||
        reduced$convergence != 0L || full$convergence != 0L) {
      return(empty_result("multinomial model did not converge"))
    }
    df <- as.integer(attr(stats::logLik(full), "df") -
                       attr(stats::logLik(reduced), "df"))
    statistic <- 2 * (as.numeric(stats::logLik(full)) -
                        as.numeric(stats::logLik(reduced)))
    p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)
    if (!is.finite(p_value) || df < 1L) {
      return(empty_result("multinomial interaction is not estimable", df))
    }
    return(list(
      family = family, effect_label = "Joint interaction",
      estimate = NA_real_, null_value = NA_real_, p_value = p_value,
      n_terms = df, n = nrow(dat), note = ""
    ))
  }

  if (family == "continuous") {
    reduced <- guarded(stats::lm(reduced_formula, data = dat))
    full <- guarded(stats::lm(full_formula, data = dat))
    test <- guarded(stats::anova(reduced, full))
    p_value <- if (is.null(test)) NA_real_ else test$`Pr(>F)`[[2L]]
  } else {
    model_family <- if (family == "binary") stats::binomial() else stats::poisson()
    reduced <- guarded(stats::glm(reduced_formula, data = dat, family = model_family))
    full <- guarded(stats::glm(full_formula, data = dat, family = model_family))
    if (is.null(reduced) || is.null(full) || !isTRUE(reduced$converged) ||
        !isTRUE(full$converged)) {
      return(empty_result("interaction model did not converge"))
    }
    test <- guarded(stats::anova(reduced, full, test = "LRT"))
    p_value <- if (is.null(test)) NA_real_ else test$`Pr(>Chi)`[[2L]]
  }
  if (is.null(reduced) || is.null(full) || warning_seen || !is.finite(p_value)) {
    return(empty_result("interaction model is not estimable"))
  }

  interaction_names <- grep(":S$", names(stats::coef(full)), value = TRUE)
  n_terms <- length(interaction_names)
  interaction_coef <- stats::coef(full)[interaction_names]
  if (n_terms < 1L || any(!is.finite(interaction_coef))) {
    return(empty_result("interaction coefficient is not estimable", n_terms))
  }
  scalar <- n_terms == 1L
  estimate <- if (!scalar) {
    NA_real_
  } else if (family %in% c("binary", "count")) {
    exp(unname(interaction_coef[[1L]]))
  } else {
    unname(interaction_coef[[1L]])
  }
  list(
    family = family,
    effect_label = if (scalar) unname(labels[[family]]) else "Joint interaction",
    estimate = estimate, null_value = unname(nulls[[family]]),
    p_value = as.numeric(p_value), n_terms = as.integer(n_terms),
    n = nrow(dat), note = ""
  )
}

#' Compare relationship modification across all eligible variable pairs.
#'
#' Pairs follow original data order. For each unordered pair, the earlier
#' column is the predictor and the later column is the outcome.
#'
#' @param original,synthetic Original and synthetic data frames.
#' @param roles Optional `dataganger_roles` object.
#' @return A tibble with one interaction-test row per comparable pair.
#' @keywords internal
#' @noRd
compare_relationship_interaction <- function(original, synthetic, roles = NULL) {
  empty <- function() {
    tibble::tibble(
      predictor = character(0), outcome = character(0), family = character(0),
      effect_label = character(0), estimate = double(0), null_value = double(0),
      p_value = double(0), n_terms = integer(0), note = character(0)
    )
  }
  # Logical/boolean is not a distinct kind -- it is treated as categorical.
  # Free text is internally synthesized as categorical, so it is compared
  # (and pairable) the same way. Any ID (alphanumeric ID -- there is no
  # separate pseudo-identifier type any more) maps to "identifier" and is
  # never comparable -- its scrambled/dropped values carry no distributional
  # meaning.
  role_to_kind <- function(role) {
    if (length(role) == 0L || is.na(role) || !nzchar(role)) return(NA_character_)
    lc <- tolower(role)
    if (grepl("alphanumeric", lc)) return("identifier")
    if (grepl("id\\b|identifier", lc)) return("identifier")
    if (grepl("categor", lc)) return("categorical")
    if (grepl("\\bdate\\b", lc)) return("date")
    if (grepl("logic|boolean", lc)) return("categorical")
    if (grepl("free.text|free_text", lc)) return("categorical")
    if (grepl("geograph", lc)) return("categorical")
    if (grepl("numeric", lc)) return("numeric")
    if (grepl("drop", lc)) return("drop")
    role
  }
  effective_kind <- function(variable, column) {
    if (!is.null(roles) && "variable" %in% names(roles)) {
      idx <- match(variable, roles$variable)
      if (!is.na(idx)) {
        user <- if ("user_role" %in% names(roles)) roles$user_role[[idx]] else NA_character_
        recommended <- if ("recommended_role" %in% names(roles)) {
          roles$recommended_role[[idx]]
        } else {
          NA_character_
        }
        user_kind <- role_to_kind(user)
        if (!is.na(user_kind)) return(user_kind)
        recommended_kind <- role_to_kind(recommended)
        if (!is.na(recommended_kind)) return(recommended_kind)
      }
    }
    if (is.logical(column)) return("categorical")
    if (inherits(column, c("Date", "POSIXct", "POSIXt"))) return("date")
    if (is.character(column) || is.factor(column)) return("categorical")
    "numeric"
  }

  variables <- intersect(names(original), names(synthetic))
  kinds <- stats::setNames(vapply(
    variables,
    function(variable) effective_kind(variable, original[[variable]]),
    character(1)
  ), variables)
  variables <- variables[!kinds %in% c("identifier", "drop")]
  if (length(variables) < 2L) return(empty())

  pairs <- utils::combn(variables, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    result <- relationship_interaction(
      original[[pair[[1L]]]], original[[pair[[2L]]]],
      synthetic[[pair[[1L]]]], synthetic[[pair[[2L]]]],
      kinds[[pair[[1L]]]], kinds[[pair[[2L]]]]
    )
    tibble::tibble(
      predictor = pair[[1L]], outcome = pair[[2L]], family = result$family,
      effect_label = result$effect_label, estimate = result$estimate,
      null_value = result$null_value, p_value = result$p_value,
      n_terms = result$n_terms, note = result$note
    )
  })
  dplyr::bind_rows(rows)
}

compare_relationship <- function(orig, syn) {
  num_cols <- names(orig)[vapply(orig, function(x) {
    is.numeric(x) && !haven::is.labelled(x)
  }, logical(1))]
  num_cols <- intersect(num_cols, names(syn))
  num_cols <- num_cols[vapply(num_cols, function(nm) {
    if (!is.numeric(syn[[nm]])) {
      return(FALSE)
    }
    vo <- stats::var(orig[[nm]], na.rm = TRUE)
    vs <- stats::var(syn[[nm]], na.rm = TRUE)
    isTRUE(vo > 0) && isTRUE(vs > 0)
  }, logical(1))]

  if (length(num_cols) < 2) {
    cli::cli_inform(c(
      "i" = "Not enough numeric columns ({length(num_cols)}) for correlation comparison.",
      " " = "Need at least 2 numeric columns with non-zero variance."
    ))
    return(tibble::tibble(
      var1 = character(0), var2 = character(0),
      cor_orig = double(0), cor_syn = double(0), cor_diff = double(0)
    ))
  }

  orig_num <- orig[, num_cols, drop = FALSE]
  syn_num  <- syn[, num_cols, drop = FALSE]

  cor_orig <- stats::cor(orig_num, use = "pairwise.complete.obs")
  cor_syn  <- stats::cor(syn_num,  use = "pairwise.complete.obs")
  cor_diff <- cor_syn - cor_orig

  # Convert to long form
  rows <- list()
  for (i in seq_len(nrow(cor_diff))) {
    for (j in seq_len(ncol(cor_diff))) {
      if (i < j) {
        rows[[length(rows) + 1]] <- tibble::tibble(
          var1     = num_cols[i],
          var2     = num_cols[j],
          cor_orig = cor_orig[i, j],
          cor_syn  = cor_syn[i, j],
          cor_diff = cor_diff[i, j]
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      var1 = character(0), var2 = character(0),
      cor_orig = double(0), cor_syn = double(0), cor_diff = double(0)
    ))
  }

  dplyr::bind_rows(rows)
}

# ===========================================================================
# Print method
# ===========================================================================

#' @export
print.dataganger_comparison <- function(x, ...) {
  cli::cli_h1("DataGangeR Comparison")

  # Dataset-level
  cli::cli_h2("Dataset")
  ds <- x$dataset
  nrow_orig <- ds$original[ds$metric == "nrow"]
  nrow_syn  <- ds$synthetic[ds$metric == "nrow"]
  ncol_orig <- ds$original[ds$metric == "ncol"]
  ncol_syn  <- ds$synthetic[ds$metric == "ncol"]
  type_match <- ds$value[ds$metric == "type_match_pct"]

  cli::cli_li("Rows: {nrow_orig} (original) -> {nrow_syn} (synthetic)")
  cli::cli_li("Columns: {ncol_orig} (original) -> {ncol_syn} (synthetic)")
  if (!is.na(type_match)) {
    cli::cli_li("Type match: {round(type_match, 1)}%")
  }
  miss_orig <- ds$original[ds$metric == "missing_orig_pct"]
  miss_syn  <- ds$synthetic[ds$metric == "missing_syn_pct"]
  cli::cli_li("Missing: {round(miss_orig, 1)}% (original) -> {round(miss_syn, 1)}% (synthetic)")

  # Numeric -- top 3 by |std_diff|
  if (nrow(x$numeric) > 0) {
    cli::cli_h2("Numeric -- top 3 by |standardized difference|")
    num <- x$numeric
    num <- num[order(abs(num$std_diff), decreasing = TRUE), ]
    n_show <- min(3, nrow(num))
    for (i in seq_len(n_show)) {
      r <- num[i, ]
      cli::cli_li("{.field {r$variable}}: std diff = {round(r$std_diff, 3)}")
      cli::cli_text("  Orig mean (SD): {round(r$mean_orig, 2)} ({round(r$sd_orig, 2)})")
    }
  }

  # Categorical -- top 3 by distributional significance (lowest p first)
  if (nrow(x$categorical) > 0) {
    cli::cli_h2("Categorical -- top 3 by distributional difference")
    cat <- x$categorical
    ord_p <- if ("dist_p" %in% names(cat)) cat$dist_p else rep(NA_real_, nrow(cat))
    cat <- cat[order(ord_p, -cat$tvd, na.last = TRUE), ]
    n_show <- min(3, nrow(cat))
    for (i in seq_len(n_show)) {
      r <- cat[i, ]
      p_txt <- if (!is.null(r$dist_p) && !is.na(r$dist_p)) sprintf("p = %.3g", r$dist_p) else "p = NA"
      cli::cli_li("{.field {r$variable}}: {p_txt}, TVD = {round(r$tvd, 3)}")
      cli::cli_text("  Levels: {r$n_levels_orig} (orig) -> {r$n_levels_syn} (syn)")
    }
  }

  # Relationship
  if (nrow(x$relationship) > 0) {
    cli::cli_h2("Relationship -- top 3 correlation diffs")
    rel <- x$relationship
    rel <- rel[order(abs(rel$cor_diff), decreasing = TRUE), ]
    n_show <- min(3, nrow(rel))
    for (i in seq_len(n_show)) {
      r <- rel[i, ]
      cli::cli_li("{.field {r$var1}} x {.field {r$var2}}: diff = {round(r$cor_diff, 3)}")
    }
  }

  # Privacy flags
  if (!is.null(x$privacy_flags) && nrow(x$privacy_flags) > 0) {
    cli::cli_h2("Privacy flags")
    pf <- x$privacy_flags
    high_n <- sum(pf$severity == "HIGH")
    med_n  <- sum(pf$severity == "MEDIUM")
    low_n  <- sum(pf$severity == "LOW")
    cli::cli_li("HIGH: {high_n}  MEDIUM: {med_n}  LOW: {low_n}")
  }

  invisible(x)
}

# ===========================================================================
# plot_comparison() helper [3.9]
# ===========================================================================

#' Plot comparison summaries
#'
#' Produces two bar charts comparing original and synthetic data:
#' standardized differences for numeric columns and total variation
#' distance for categorical columns. Requires `ggplot2` (Suggests).
#'
#' @param comparison A `dataganger_comparison` object from
#'   [compare_synthetic()].
#'
#' @return Invisibly, a list with two `ggplot` objects: `numeric` and
#'   `categorical`. Each is `NULL` if no columns of that type exist.
#' @export
#'
#' @examples
#' dat <- data.frame(x = 1:10, y = letters[1:10])
#' spec <- synth_spec(purpose = "demo")
#' syn <- synthesize_data(dat, spec)
#' cmp <- compare_synthetic(dat, syn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_comparison(cmp)
#' }
plot_comparison <- function(comparison) {
  rlang::check_installed("ggplot2", reason = "to use `plot_comparison()`")

  plots <- list(numeric = NULL, categorical = NULL)

  # Numeric plot
  if (nrow(comparison$numeric) > 0) {
    num <- comparison$numeric
    num$abs_diff <- abs(num$std_diff)
    num$color <- cut(num$abs_diff,
      breaks = c(-Inf, 0.1, 0.2, Inf),
      labels = c("green", "yellow", "red")
    )
    num <- num[order(num$abs_diff), ]
    num$variable <- factor(num$variable, levels = num$variable)

    color_map <- c(green = "#4CAF50", yellow = "#FFC107", red = "#F44336")

    plots$numeric <- ggplot2::ggplot(
      num, ggplot2::aes(x = variable, y = std_diff, fill = color)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = color_map, guide = "none") +
      ggplot2::labs(
        title = "Standardized difference (numeric columns)",
        x = "", y = "Standardized difference"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  # Categorical plot
  if (nrow(comparison$categorical) > 0) {
    cat <- comparison$categorical
    p_vec <- if ("dist_p" %in% names(cat)) cat$dist_p else rep(NA_real_, nrow(cat))
    cat$color <- vapply(p_vec, fidelity_color, character(1))
    cat <- cat[order(cat$tvd), ]
    cat$variable <- factor(cat$variable, levels = cat$variable)

    color_map <- c(good = "#4CAF50", warn = "#FFC107", bad = "#F44336", none = "#BDBDBD")

    plots$categorical <- ggplot2::ggplot(
      cat, ggplot2::aes(x = variable, y = tvd, fill = color)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = color_map, guide = "none") +
      ggplot2::labs(
        title = "Categorical columns (bar = TVD effect size, colour = chi-square/Fisher p)",
        x = "", y = "Total variation distance"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  invisible(plots)
}
