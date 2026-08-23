# Test seam: a single mockable point for synthpop availability so the
# graceful-fallback path can be exercised even when synthpop is installed.
synthpop_available <- function() {
  requireNamespace("synthpop", quietly = TRUE)
}

synthesize_synthpop <- function(data, spec, roles = NULL) {
  if (!synthpop_available()) {
    cli::cli_abort(c(
      "Package {.pkg synthpop} is required for {.code engine = 'synthpop'}.",
      "i" = "Install it with: {.run install.packages(\"synthpop\")}"
    ))
  }

  data <- synthpop_mask_rare_inputs(data, spec, roles)
  dg_log("synthesize_synthpop: building synthpop args")
  syn_args <- spec_to_synthpop_args(spec, roles, data)
  dg_log(
    "synthesize_synthpop: calling synthpop::syn() on ",
    ncol(syn_args$data), " column(s), k=", syn_args$k %||% nrow(data)
  )
  result   <- do.call(synthpop::syn, syn_args)
  dg_log("synthesize_synthpop: synthpop done")
  syn <- tibble::as_tibble(result$syn)

  # Stitch back "bridge" columns that were excluded from synthpop to prevent
  # CART hangs (character-stored dates and other high-cardinality char columns).
  # They are synthesized independently via the marginal engine.
  bridge <- synthpop_bridge_cols(roles, data)
  if (length(bridge) > 0L) {
    dg_log("synthesize_synthpop: marginal bridge for ", length(bridge), " column(s)")
    bridge_syn <- synthesize_marginal(
      data[, bridge, drop = FALSE], spec, roles = roles
    )
    syn <- dplyr::bind_cols(syn, bridge_syn)
    # Restore the original column order, excluding truly-excluded cols.
    expected <- setdiff(names(data), synthpop_role_excluded_cols(roles))
    syn <- syn[, intersect(expected, names(syn)), drop = FALSE]
  }

  syn
}

synthpop_mask_rare_inputs <- function(data, spec, roles = NULL) {
  role_data <- roles %||% detect_roles(data)
  out <- data

  for (col_name in names(out)) {
    idx <- if (!is.null(role_data) && "variable" %in% names(role_data)) {
      match(col_name, role_data$variable)
    } else {
      NA_integer_
    }
    action <- synthpop_role_action(role_data, idx)
    if (action %in% c("drop", "pass_through", "scramble")) {
      next
    }

    x <- out[[col_name]]
    role <- synthpop_effective_role(role_data, idx, x)
    if (!identical(role, "categorical")) {
      next
    }
    if (!identical(dg_effective_label_strategy(spec, role_data, idx), "mask_rare")) {
      next
    }

    out[[col_name]] <- synthpop_mask_rare_level_values(
      x,
      rare_level_min_n = spec$rare_level_min_n %||% 5
    )
  }

  out
}

synthpop_role_action <- function(roles, idx) {
  if (is.null(roles) || is.na(idx)) {
    return("synthesize")
  }
  action_col <- if ("simulation" %in% names(roles)) {
    "simulation"
  } else if ("treatment" %in% names(roles)) {
    "treatment"
  } else {
    NULL
  }
  if (is.null(action_col)) {
    return("synthesize")
  }
  action <- roles[[action_col]][[idx]]
  if (is.na(action) || !nzchar(action)) "synthesize" else action
}

synthpop_effective_role <- function(roles, idx, x) {
  if (is.null(roles) || is.na(idx)) {
    return(dg_class_to_role(class(x)[[1L]]))
  }
  user_role <- if ("user_role" %in% names(roles)) roles$user_role[[idx]] else NA_character_
  recommended_role <- if ("recommended_role" %in% names(roles)) {
    roles$recommended_role[[idx]]
  } else {
    NA_character_
  }
  class_col <- if ("class" %in% names(roles)) roles$class[[idx]] else class(x)[[1L]]
  eff_role(user_role, recommended_role, class_col)
}

synthpop_mask_rare_level_values <- function(x, rare_level_min_n = 5L) {
  values <- as.character(x)
  counts <- table(values[!is.na(values)])
  rare <- names(counts)[counts < rare_level_min_n]
  if (length(rare) == 0L) {
    return(values)
  }

  rare <- rare[order(-as.integer(counts[rare]), rare, method = "radix")]
  placeholder_numbers <- integer(length(rare))
  occupied <- unique(values[!is.na(values)])
  next_number <- 1L
  for (i in seq_along(rare)) {
    while (paste("Other category", next_number) %in% occupied) {
      next_number <- next_number + 1L
    }
    placeholder_numbers[[i]] <- next_number
    next_number <- next_number + 1L
  }
  placeholders <- stats::setNames(
    paste("Other category", placeholder_numbers),
    rare
  )

  unname(ifelse(values %in% names(placeholders), placeholders[values], values))
}

spec_to_synthpop_args <- function(spec, roles, data) {
  excl <- synthpop_excluded_cols(roles, data)
  work <- data[, !names(data) %in% excl, drop = FALSE]

  if (ncol(work) == 0L) {
    cli::cli_abort(
      "No synthesizable columns remain after excluding ID, free-text, and high-cardinality columns; cannot use synthpop engine."
    )
  }

  args <- list(data = work, print.flag = FALSE)
  if (!is.null(spec$seed)) args$seed <- as.integer(spec$seed)
  if (!is.null(spec$n))    args$k    <- as.integer(spec$n)

  num_cont <- names(work)[vapply(work, is_continuous_numeric, logical(1))]
  if (length(num_cont)) {
    # synthpop::syn() requires `smoothing` as a named list, not a named vector
    args$smoothing <- stats::setNames(as.list(rep("density", length(num_cont))), num_cont)
  }

  args
}

# Columns excluded from the synthpop call itself by role alone (alphanumeric
# IDs and free text) -- synthpop's sequential CART cannot handle their high
# cardinality. They are absent from synthpop::syn()'s own output, but
# apply_simulation_treatment() adds them back afterwards from the original
# data (scrambled or resampled as categorical), so they still appear in the
# final synthetic output unless their role/action says otherwise.
synthpop_role_excluded_cols <- function(roles) {
  if (is.null(roles) || !"recommended_role" %in% names(roles)) {
    return(character())
  }
  excl <- roles$variable[roles$recommended_role %in% c("alphanumeric ID", "free text")]
  excl[!is.na(excl)]
}

# Columns that must stay out of synthpop's CART to avoid hangs, but ARE still
# synthesized (via the marginal engine) and stitched back into the output.
# Criteria: character "date" role columns, and any character column with
# more than 100 distinct values (e.g. date strings, free-form text that slipped
# past the free-text detector, high-cardinality codes).
synthpop_bridge_cols <- function(roles, data) {
  if (is.null(data)) return(character())
  true_excl <- synthpop_role_excluded_cols(roles)
  role_lookup <- if (!is.null(roles) && "variable" %in% names(roles)) {
    stats::setNames(roles$recommended_role, roles$variable)
  } else {
    NULL
  }

  bridge <- character()
  for (col in names(data)) {
    if (col %in% true_excl) next
    x <- data[[col]]
    if (!is.character(x)) next
    col_role   <- if (!is.null(role_lookup) && col %in% names(role_lookup))
                    role_lookup[[col]] else "unknown"
    n_dist     <- length(unique(x[!is.na(x)]))
    # > 20 distinct values: CART enumerates 2^(k-1) factor splits for any
    # factor predictor used in subsequent column models; k>20 hangs reliably.
    if (identical(col_role, "date") || n_dist > 20L) {
      bridge <- c(bridge, col)
    }
  }
  bridge
}

# All columns excluded from synthpop::syn() -- both truly-excluded and bridge.
synthpop_excluded_cols <- function(roles, data = NULL) {
  unique(c(synthpop_role_excluded_cols(roles), synthpop_bridge_cols(roles, data)))
}

is_continuous_numeric <- function(x) {
  if (!is.numeric(x) || inherits(x, "integer64")) {
    return(FALSE)
  }

  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(FALSE)
  }

  if (all(abs(x - round(x)) < .Machine$double.eps^0.5)) {
    return(FALSE)
  }

  length(unique(x)) > 10L
}
