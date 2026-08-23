#' Synthesize a data double
#'
#' Creates a synthetic copy of a dataset using the specified specification
#' and engine. The internal engine supports schema-only (Level 1) and
#' marginal (Level 2) synthesis. The optional synthpop engine is used for
#' objectives that request moderate or high relationship preservation.
#'
#' @param data A data frame to synthesize from.
#' @param spec A `dataganger_spec` object from [synth_spec()].
#' @param roles Optional; a `dataganger_roles` object from [detect_roles()].
#'   Informs column treatment but does not override the spec.
#' @param engine Character or `NULL`. Engine to use: `"auto"`, `"internal"`,
#'   `"marginal"` (alias for `"internal"`), or `"synthpop"`. When
#'   `NULL`, defaults to `spec$engine` or derives from
#'   `spec$preserve_correlations`.
#'
#' @return An S3 object of class `dataganger_synthetic`, a tibble with
#'   attributes `spec`, `original_dims`, `seed_used`, and `generated_at`.
#'   Categorical and other non-numeric text columns are returned as plain
#'   `character`; numeric, logical, date, and datetime columns retain their
#'   corresponding types.
#'
#' @section Disabling synthpop:
#' Set `options(dataganger.disable_synthpop = TRUE)` to steer
#' auto-derived synthesis onto the internal engine even when synthpop is
#' installed. This is intended for environments where a synthpop synthesis
#' is undesirable or can hang unattended (for example continuous
#' integration). An explicit `engine = "synthpop"` request is still
#' honoured; only objective-derived routing is affected.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = 1:5, y = letters[1:5])
#' spec <- synth_spec(purpose = "demo")
#' syn <- synthesize_data(dat, spec)
synthesize_data <- function(data, spec, roles = NULL,
                            engine = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}")
  }

  if (!inherits(spec, "dataganger_spec")) {
    cli::cli_abort("{.arg spec} must be a {.cls dataganger_spec} object")
  }

  data <- normalize_categorical_input(data)

  # "marginal" is a user-friendly alias for "internal" (per todo.md API)
  spec_engine <- spec[["engine", exact = TRUE]]
  explicit <- engine %||% spec_engine
  engine <- explicit %||% engine_from_correlations(spec)
  engine <- match.arg(engine, c("auto", "internal", "marginal", "synthpop"))
  if (identical(engine, "auto")) {
    explicit <- NULL
    engine <- engine_from_correlations(spec)
  }
  if (engine == "marginal") engine <- "internal"
  # Safety valve: steer auto-derived synthpop onto the internal engine when
  # synthpop is intentionally disabled (e.g. unattended CI, where a synthpop
  # synthesis can hang). Only affects objective-derived routing; an explicit
  # engine = "synthpop" request is still honoured. Scoped to the case where
  # synthpop *is* available so the unavailable-fallback path below is unchanged.
  if (engine == "synthpop" && is.null(explicit) && synthpop_available() &&
      isTRUE(getOption("dataganger.disable_synthpop", FALSE))) {
    engine <- "internal"
  }

  if (engine == "synthpop" && is.null(explicit) && !synthpop_available()) {
    cli::cli_warn(
      "Install {.pkg synthpop} for full-fidelity synthesis; using the marginal engine for now."
    )
    engine <- "internal"
  }

  # Record dimensions before synthesis
  original_dims <- list(nrow = nrow(data), ncol = ncol(data))

  if (engine == "synthpop") {
    # Without roles, ID and free-text columns are not excluded from synthpop.
    # synthpop's sequential CART grinds to a halt on a high-cardinality column
    # (e.g. a 200-unique-value identifier), so derive roles when none are given.
    roles <- roles %||% detect_roles(data)
    syn <- synthesize_synthpop(data, spec, roles = roles)
    syn <- apply_simulation_treatment_seeded(syn, data, roles, spec$seed)
    syn <- match_decimal_precision(syn, data)
    attr(syn, "spec")          <- spec
    attr(syn, "original_dims") <- list(nrow = nrow(data), ncol = ncol(data))
    attr(syn, "seed_used")     <- spec$seed
    attr(syn, "generated_at")  <- Sys.time()
    class(syn) <- c("dataganger_synthetic", class(syn))
    # synthpop uses factors inside its models. Flatten before our own level
    # restoration and k-anonymity passes so both engines see character data.
    syn <- flatten_to_character(syn)
    syn <- ensure_levels_present(syn, data, roles, spec)
    syn <- enforce_kanon(syn, roles = roles, k = spec$k_anon %||% 5)
    # Before apply_name_strategy: columns are matched to the original by name,
    # and the "generic" strategy renames them to col_1, col_2, ...
    syn <- restore_logical_columns(syn, data)
    syn <- apply_name_strategy(syn, spec, data)
    syn <- flatten_to_character(syn)
    attr(syn, "engine")        <- "synthpop"
    return(syn)
  }

  # Execute synthesis - wrap in with_seed if seed is set (C4)
  run_synthesis <- function() {
    level <- spec$level %||% "marginal"
    if (identical(level, "hifi")) {
      level <- "marginal"
    }

    syn <- switch(level,
      schema = synthesize_schema(data, spec, roles),
      marginal = synthesize_marginal(data, spec, roles),
      {
        cli::cli_abort(c(
          "Unknown synthesis level: {.val {level}}",
          "i" = "Valid levels: {.val {c('schema', 'marginal')}}"
        ))
      }
    )

    syn
  }

  if (!is.null(spec$seed)) {
    syn <- withr::with_seed(spec$seed, run_synthesis())
    seed_used <- spec$seed
  } else {
    syn <- run_synthesis()
    seed_used <- NULL
  }

  # Seeded here too: scramble draws from the RNG, and a re-seeded run must
  # scramble identically (deterministic seeded synthesis contract).
  syn <- apply_simulation_treatment_seeded(syn, data, roles, spec$seed)
  syn <- match_decimal_precision(syn, data)

  # Build S3 object
  attr(syn, "spec")          <- spec
  attr(syn, "original_dims") <- original_dims
  attr(syn, "seed_used")     <- seed_used
  attr(syn, "generated_at")  <- Sys.time()
  class(syn) <- c("dataganger_synthetic", class(syn))

  syn <- ensure_levels_present(syn, data, roles, spec)
  syn <- enforce_kanon(syn, roles = roles, k = spec$k_anon %||% 5)
  # [2.13] Apply name_strategy after k-anonymity so the mapping only records
  # columns that survive direct-ID dropping and suppression shaping.
  syn <- apply_name_strategy(syn, spec, data)
  syn <- flatten_to_character(syn)
  attr(syn, "engine") <- "internal"

  syn
}

# ===========================================================================
# Name strategy application [2.13]
# ===========================================================================

apply_name_strategy <- function(syn, spec, original) {
  strategy <- spec$name_strategy %||% "preserve"

  if (strategy == "preserve") {
    return(syn)
  }

  n_cols <- ncol(syn)
  generic_names <- paste0("col_", seq_len(n_cols))
  name_map <- stats::setNames(generic_names, names(syn))

  if (strategy %in% c("generic", "dictionary_only")) {
    # Store the original -> synthetic mapping inside the spec attribute so it
    # survives tibble operations that silently drop bare attributes.
    spec_attr <- attr(syn, "spec")
    spec_attr$name_map <- name_map
    attr(syn, "spec") <- spec_attr
    names(syn) <- unname(name_map)
    return(syn)
  }

  syn
}

# Deterministic seeded synthesis contract: scramble draws from the RNG, so
# when a seed is set the treatment pass must be reproducible too. apply_
# simulation_treatment() runs after the engine's own seeded step, so wrap its
# randomness in the same seed rather than letting it consume ambient RNG
# state (which differs between sessions).
apply_simulation_treatment_seeded <- function(syn, original, roles = NULL,
                                              seed = NULL) {
  if (is.null(seed)) {
    return(apply_simulation_treatment(syn, original, roles))
  }
  withr::with_seed(seed, apply_simulation_treatment(syn, original, roles))
}

apply_simulation_treatment <- function(syn, original, roles = NULL) {
  if (is.null(roles) || !"variable" %in% names(roles)) {
    return(syn)
  }

  treatment_col <- if ("simulation" %in% names(roles)) {
    "simulation"
  } else if ("treatment" %in% names(roles)) {
    "treatment"
  } else {
    NULL
  }

  if (is.null(treatment_col)) {
    return(syn)
  }

  treatment <- roles[[treatment_col]]
  treatment[is.na(treatment) | !nzchar(treatment)] <- "synthesize"
  treatment <- stats::setNames(treatment, roles$variable)

  pass_cols <- intersect(names(treatment)[treatment == "pass_through"], names(original))
  scramble_cols <- intersect(names(treatment)[treatment == "scramble"], names(original))
  drop_cols <- intersect(names(treatment)[treatment == "drop"], names(syn))

  if (length(pass_cols) > 0L && nrow(syn) != nrow(original)) {
    cli::cli_warn(c(
      "Pass-through columns {.val {pass_cols}} require the same row count as the original data.",
      "i" = "Row count changed ({nrow(original)} \u2192 {nrow(syn)}); synthesizing those columns instead.",
      "i" = "Set row count back to {nrow(original)} to use pass-through."
    ))
    pass_cols <- character(0)
  }

  if (length(scramble_cols) > 0L && nrow(syn) != nrow(original)) {
    cli::cli_warn(c(
      "Scrambled columns {.val {scramble_cols}} require the same row count as the original data.",
      "i" = "Row count changed ({nrow(original)} \u2192 {nrow(syn)}); synthesizing those columns instead.",
      "i" = "Set row count back to {nrow(original)} to use scramble."
    ))
    scramble_cols <- character(0)
  }

  # Set (not just overwrite) so these columns are added back even when an
  # engine excluded them from its own output entirely -- e.g. the synthpop
  # engine drops alphanumeric-ID/free-text columns from its own call rather
  # than synthesizing them, so pass_through/scramble must be able to add
  # them, not merely overwrite an existing column.
  for (col in pass_cols) {
    syn[[col]] <- original[[col]]
  }

  for (col in scramble_cols) {
    syn[[col]] <- scramble_alphanumeric_id(as.character(original[[col]]))
  }

  if (length(drop_cols) > 0L) {
    syn <- syn[, !names(syn) %in% drop_cols, drop = FALSE]
  }

  syn
}

# ===========================================================================
# Decimal-precision matching
# ===========================================================================

# Round each synthetic numeric column to the same number of decimal places as
# the matching original column, so synthetic values read at the original's
# granularity (e.g. 27.7 -> 23.6, not 23.56576648668623). Integer columns stay
# integer. Columns are still matched by name here (before name_strategy renames).
match_decimal_precision <- function(syn, original) {
  common <- intersect(names(syn), names(original))
  for (col in common) {
    o <- original[[col]]
    s <- syn[[col]]
    if (!is.numeric(s) || !is.numeric(o)) next
    # Round to the original's decimal count; integer-valued columns -> 0
    # decimals (whole numbers), keeping the numeric type the engine produced.
    syn[[col]] <- round(s, decimal_places(o))
  }
  syn
}

# Max number of decimal places used by the finite values in `x` (capped at 10,
# estimated from a sample for large vectors).
decimal_places <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(0L)
  if (length(x) > 1000L) {
    idx <- unique(as.integer(seq(1L, length(x), length.out = 1000L)))
    x <- x[idx]
  }
  d <- vapply(x, function(v) {
    s <- sub("0+$", "", sprintf("%.10f", v))
    if (grepl(".", s, fixed = TRUE)) nchar(sub("^.*\\.", "", s)) else 0L
  }, integer(1))
  min(max(d), 10L)
}
