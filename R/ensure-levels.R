#' Restore categorical level presence before k-anonymity enforcement
#'
#' @keywords internal
#' @noRd
ensure_levels_present <- function(syn, original, roles, spec) {
  if (identical(spec$purpose %||% "demo", "analytics")) {
    return(syn)
  }

  if (is.null(roles)) {
    roles <- detect_roles(original)
  }
  if (!"variable" %in% names(roles)) {
    return(syn)
  }

  restore <- function() {
    k <- as.integer(spec$k_anon %||% 5)
    treatment <- dg_role_treatment(roles)
    common_cols <- intersect(names(original), names(syn))
    capacity_warnings <- list()

    for (col_name in common_cols) {
      role_idx <- match(col_name, roles$variable)
      if (is.na(role_idx)) {
        next
      }

      action <- treatment[[col_name]] %||% "synthesize"
      if (!identical(action, "synthesize")) {
        next
      }

      role_class <- role_value(roles, role_idx, "class", NA_character_)
      user_role <- role_value(roles, role_idx, "user_role", NA_character_)
      recommended_role <- role_value(
        roles,
        role_idx,
        "recommended_role",
        NA_character_
      )
      effective_type <- eff_role(user_role, recommended_role, role_class)
      is_labelled <- identical(role_class, "haven_labelled") ||
        haven::is.labelled(original[[col_name]])
      is_textual <- is.factor(original[[col_name]]) ||
        is.character(original[[col_name]])
      is_categorical_text <- is_textual &&
        identical(effective_type, "categorical")
      if (!is_labelled && !is_categorical_text) {
        next
      }

      source_is_labelled <- haven::is.labelled(original[[col_name]])
      default_label_strategy <- if (source_is_labelled ||
          is_categorical_text) {
        spec$label_strategy %||% "preserve"
      } else {
        "preserve"
      }
      label_strategy <- role_value(
        roles,
        role_idx,
        "label_strategy",
        default_label_strategy
      )
      expected <- expected_levels_for_output(
        original[[col_name]],
        syn[[col_name]],
        label_strategy,
        spec$rare_level_min_n %||% 5
      )
      if (!length(expected)) {
        next
      }

      syn[[col_name]] <- prepare_level_column(syn[[col_name]], expected)
      syn[[col_name]] <- restore_level_counts(syn[[col_name]], expected, k)

      # Warn on the measured outcome, not on a capacity estimate. The old
      # check compared nrow(syn) against k * length(expected) before doing any
      # work, which fires whenever the arithmetic is tight even though the
      # restoration often still lands every level at k -- levels that already
      # held k or more copies need no injection at all, so the budget is
      # usually smaller than the worst-case product. Counting afterwards warns
      # only when a level really did end below k, which is the condition under
      # which enforce_kanon() will subsequently merge it away.
      final_counts <- table(as.character(syn[[col_name]]))
      observed_n <- as.integer(final_counts[expected])
      observed_n[is.na(observed_n)] <- 0L
      if (any(observed_n < k)) {
        capacity_warnings[[length(capacity_warnings) + 1L]] <- list(
          column = col_name,
          levels = length(expected),
          required_n = k * length(expected)
        )
      }
    }

    if (length(capacity_warnings)) {
      details <- vapply(
        capacity_warnings,
        function(item) {
          paste0(
            item$column, " (", item$levels, " levels at k = ", k,
            " require minimum n = ", item$required_n, ")"
          )
        },
        character(1)
      )
      largest_required_n <- max(vapply(
        capacity_warnings,
        function(item) item$required_n,
        integer(1)
      ))
      cli::cli_warn(
        paste0(
          "Cannot guarantee level presence for columns: ",
          paste(details, collapse = "; "),
          ". Largest minimum n = ", largest_required_n,
          "; output has n = ", nrow(syn), ". Restoring as many levels ",
          "as fit without removing another level's last copy."
        )
      )
    }

    syn
  }

  if (is.null(spec$seed)) {
    restore()
  } else {
    withr::with_seed(as.integer(spec$seed), restore())
  }
}

role_value <- function(roles, row, column, default) {
  if (!column %in% names(roles)) {
    return(default)
  }
  value <- roles[[column]][[row]]
  if (is.null(value) || length(value) != 1L || is.na(value) ||
    (is.character(value) && !nzchar(value))) {
    return(default)
  }
  value
}

expected_levels_for_output <- function(original, synthetic, label_strategy,
                                       rare_level_min_n) {
  observed <- if (haven::is.labelled(original)) {
    as.character(haven::as_factor(original))
  } else {
    as.character(original)
  }
  observed <- observed[!is.na(observed)]
  source_levels <- sort(unique(observed))

  if (identical(label_strategy, "mask_rare")) {
    if (!length(observed)) {
      return(source_levels)
    }
    counts <- table(observed)
    masked <- mask_rare_category_labels(
      observed,
      source_levels,
      names(counts),
      as.integer(counts),
      rare_level_min_n
    )
    return(sort(unique(c(
      masked$x_obs,
      setdiff(source_levels, unique(observed))
    ))))
  }

  source_levels
}

prepare_level_column <- function(column, expected) {
  if (haven::is.labelled(column)) {
    return(as.character(haven::as_factor(column)))
  }
  as.character(column)
}

level_keys <- function(x) {
  if (haven::is.labelled(x)) {
    return(as.character(haven::as_factor(x)))
  }
  as.character(x)
}

restore_level_counts <- function(column, expected, k) {
  expected_keys <- level_keys(expected)
  current_keys <- level_keys(column)
  current_counts <- vapply(
    expected_keys,
    function(key) sum(!is.na(current_keys) & current_keys == key),
    integer(1)
  )
  target_order <- order(current_counts > 0L, current_counts)

  for (target_idx in target_order) {
    target_key <- expected_keys[[target_idx]]
    target <- expected[[target_idx]]
    deficit <- k - sum(!is.na(current_keys) & current_keys == target_key)
    if (deficit <= 0L) {
      next
    }

    for (copy in seq_len(deficit)) {
      current_keys <- level_keys(column)
      observed_counts <- table(current_keys[!is.na(current_keys)])
      donor_counts <- unname(observed_counts[current_keys])
      eligible <- !is.na(current_keys) & current_keys != target_key &
        donor_counts > 1L

      if (any(eligible)) {
        largest <- max(donor_counts[eligible])
        donor_rows <- which(eligible & donor_counts == largest)
      } else {
        donor_rows <- integer(0)
      }
      if (!length(donor_rows)) {
        break
      }

      donor <- sample(donor_rows, size = 1L)
      column[donor] <- target
    }
  }

  column
}
