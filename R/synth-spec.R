#' Create a synthesis specification
#'
#' Builds a synthesis specification from a purpose preset with optional
#' user overrides. The specification records the synthesis parameters but
#' does not check engine availability - that is done by [synthesize_data()].
#'
#' @param purpose Character. A single non-missing string: `"demo"`,
#'   `"development"`, or `"analytics"`.
#' @param level Character or `NULL`. Synthesis level: `"schema"` or
#'   `"marginal"`. If `NULL`, derived from the preset.
#' @param n Integer or `NULL`. Number of rows to synthesize. If `NULL`,
#'   defaults to `nrow(original)` at synthesis time.
#' @param roles A `dataganger_roles` object or `NULL`. Column role assignments.
#' @param privacy A `dataganger_privacy_check` object or `NULL`. When
#'   `stage == "pre"`, flags harden defaults (e.g. IDs dropped, free text
#'   removed).
#' @param name_strategy Character or `NULL`. How synthetic column names are
#'   handled: `"preserve"` keeps your original column names, `"generic"`
#'   replaces them with neutral names (`col_1`, `col_2`, ...), and
#'   `"dictionary_only"` anonymizes the names but records the mapping in the
#'   exported data dictionary. If `NULL`, derived from the preset.
#' @param seed Integer or `NULL`. Reproducibility seed. Fixes the random draw
#'   so the same spec and data reproduce the exact same synthetic output.
#' @param engine Character or `NULL`. Optional explicit synthesis engine:
#'   `"auto"` clears any explicit engine choice so [synthesize_data()] derives
#'   the engine from the objective, `"internal"`/`"marginal"` synthesizes
#'   each column from its own distribution (fast, dependency-free, ignores
#'   cross-column relationships), and `"synthpop"` models columns
#'   conditionally so correlations and joint structure are preserved (higher
#'   fidelity, needs the synthpop package). If `NULL`, [synthesize_data()]
#'   derives the engine from the objective.
#' @param acknowledge_risk Logical. Required to be `TRUE` when
#'   `purpose = "analytics"`.
#' @param ... Additional decision parameters passed to the spec list. These are
#'   the same settings exposed under *Synthesis Settings* in the app:
#'   \itemize{
#'     \item `preserve_correlations` --- how strongly cross-variable
#'       relationships are retained (`"none"`, `"moderate"`, `"high"`).
#'     \item `coarsen_dates` --- logical; round dates (e.g. to month or year)
#'       so an exact event date cannot single out an individual.
#'     \item `merge_rare` --- logical; combine infrequent category values into
#'       an `"other"` group to reduce re-identification risk.
#'     \item `label_strategy` --- whether categorical labels are preserved or
#'       rare labels are replaced with distinct neutral placeholders.
#'     \item `k_anon` --- minimum cell size for k-anonymity. Here, a
#'       quasi-identifier (QI) is a column that can identify someone only when
#'       combined with others, a cell is one shared QI combination, and
#'       suppression means blanking QI values in cells that still fall below
#'       the target. The validator allows values down to 2, but automated
#'       escape-route suggestions never pick a value below 3.
#'     \item `rare_level_min_n` --- integer; category values seen fewer than
#'       this many times count as rare (then merged or suppressed).
#'     \item `free_text_strategy` --- how free-text columns are treated:
#'       `"categorical"` (default) synthesizes them the same way as any other
#'       categorical column, with rare/near-unique values collapsed to
#'       `".other"`; `"drop"` and `"redact"` are also available and are used
#'       to reinforce privacy hardening when a pre-stage check flags a
#'       free-text column as high risk.
#'     \item `preserve_missingness` --- how closely to reproduce the original
#'       pattern of missing (`NA`) values (`"approx"`, `"exact"`, `"none"`).
#'   }
#'
#' @return An S3 object of class `dataganger_spec` (a named list).
#' @export
#'
#' @examples
#' synth_spec(purpose = "demo")
#' synth_spec(purpose = "development", n = 200, seed = 42)
#' synth_spec(purpose = "analytics", acknowledge_risk = TRUE)
synth_spec <- function(purpose,
                       level = NULL,
                       n = NULL,
                       roles = NULL,
                       privacy = NULL,
                       name_strategy = NULL,
                       seed = NULL,
                       engine = NULL,
                       acknowledge_risk = FALSE,
                       ...) {

  valid_purposes <- c("demo", "development", "analytics")

  if (!is.character(purpose) || length(purpose) != 1L ||
      is.na(purpose) || !nzchar(purpose)) {
    cli::cli_abort(c(
      "{.arg purpose} must be a single non-missing character string",
      "i" = "Valid purposes: {.val {valid_purposes}}"
    ))
  }

  if (!purpose %in% valid_purposes) {
    cli::cli_abort(c(
      "Invalid purpose: {.val {purpose}}",
      "i" = "Valid purposes: {.val {valid_purposes}}"
    ))
  }

  # --- Load preset ---
  preset <- preset_table(purpose)

  # --- User overrides ---
  if (!is.null(level))         preset$level         <- level
  if (!is.null(n))             preset$n             <- n
  if (!is.null(name_strategy)) preset$name_strategy <- name_strategy
  if (!is.null(seed))          preset$seed          <- seed

  if (!is.null(engine)) {
    valid_engines <- c("auto", "internal", "marginal", "synthpop")
    if (!engine %in% valid_engines) {
      cli::cli_abort(c(
        "Invalid engine: {.val {engine}}",
        "i" = "Valid engines: {.val {valid_engines}}"
      ))
    }
    preset$engine <- if (identical(engine, "auto")) NULL else engine
  }

  # --- Absorb ... into spec ---
  dots <- list(...)
  for (nm in names(dots)) {
    preset[[nm]] <- dots[[nm]]
  }

  preset$k_anon <- preset$k_anon %||% 5

  # --- Validation ---
  validate_spec(preset, purpose, acknowledge_risk, roles)

  # --- Privacy pre-flag hardening (C11) ---
  preset <- apply_privacy_hardening(preset, privacy, roles)


  # --- Set acknowledged_risk flag ---
  preset$acknowledged_risk <- acknowledge_risk
  preset$purpose <- purpose

  class(preset) <- "dataganger_spec"
  preset
}

# ===========================================================================
# Preset table - exact mapping from implementation plan section 4.1
# ===========================================================================

preset_table <- function(purpose) {
  switch(purpose,
    demo = list(
      level               = "marginal",
      n                   = NULL,
      preserve_correlations = "none",
      coarsen_dates       = TRUE,
      merge_rare          = FALSE,
      label_strategy      = "mask_rare",
      free_text_strategy  = "categorical",
      name_strategy       = "preserve",
      rare_level_min_n    = 5,
      k_anon              = 5,
      preserve_missingness = "approx",
      seed                = NULL
    ),
    development = list(
      level               = "marginal",
      n                   = NULL,
      preserve_correlations = "moderate",
      coarsen_dates       = FALSE,
      merge_rare          = FALSE,
      label_strategy      = "mask_rare",
      free_text_strategy  = "categorical",
      name_strategy       = "preserve",
      rare_level_min_n    = 5,
      k_anon              = 5,
      preserve_missingness = "approx",
      seed                = NULL
    ),
    analytics = list(
      level               = "hifi",
      n                   = NULL,
      preserve_correlations = "high",
      coarsen_dates       = FALSE,
      merge_rare          = FALSE,
      label_strategy      = "preserve",
      free_text_strategy  = "categorical",
      name_strategy       = "preserve",
      rare_level_min_n    = 5,
      k_anon              = 5,
      preserve_missingness = "approx",
      seed                = NULL
    ),
    cli::cli_abort("Unknown purpose: {.val {purpose}}")
  )
}

# ===========================================================================
# Validation
# ===========================================================================

validate_spec <- function(spec, purpose, acknowledge_risk, roles) {
  # n must be positive if set
  if (!is.null(spec$n) && spec$n <= 0) {
    cli::cli_abort("{.arg n} must be > 0; use {.code level = \"schema\"} for type-only output")
  }

  # rare_level_min_n must be > 1
  if (!is.null(spec$rare_level_min_n) && spec$rare_level_min_n <= 1) {
    cli::cli_abort("{.arg rare_level_min_n} must be > 1, got {spec$rare_level_min_n}")
  }

  # k_anon must be an integer-ish value >= 2
  if (!is.null(spec$k_anon) && (!is.numeric(spec$k_anon) || spec$k_anon < 2)) {
    cli::cli_abort("{.arg k_anon} must be a number >= 2, got {spec$k_anon}")
  }

  # analytics requires acknowledge_risk
  if (purpose == "analytics" && !isTRUE(acknowledge_risk)) {
    cli::cli_abort(c(
      "Purpose {.val analytics} requires {.arg acknowledge_risk = TRUE}",
      "i" = "High-fidelity synthesis may preserve sensitive patterns.",
      "i" = "Set {.code acknowledge_risk = TRUE} to proceed."
    ))
  }

  # development routes to synthpop when available
  if (purpose == "development") {
    cli::cli_inform(
      c("i" = "Development synthesis uses {.pkg synthpop} for correlation-aware output when installed; review privacy warnings before sharing.")
    )
  }

  # Validate roles if supplied
  if (!is.null(roles) && !inherits(roles, "dataganger_roles")) {
    cli::cli_abort("{.arg roles} must be a {.cls dataganger_roles} object or {.code NULL}")
  }

  # Validate level
  valid_levels <- c("schema", "marginal", "hifi")
  if (!is.null(spec$level) && !spec$level %in% valid_levels) {
    cli::cli_abort(c(
      "Invalid level: {.val {spec$level}}",
      "i" = "Valid levels: {.val {valid_levels}}"
    ))
  }

  # Validate name_strategy
  valid_name_strategies <- c("preserve", "generic", "dictionary_only")
  if (!is.null(spec$name_strategy) && !spec$name_strategy %in% valid_name_strategies) {
    cli::cli_abort(c(
      "Invalid name_strategy: {.val {spec$name_strategy}}",
      "i" = "Valid strategies: {.val {valid_name_strategies}}"
    ))
  }

  # Validate preserve_missingness
  valid_missingness <- c("none", "approx", "exact")
  if (!is.null(spec$preserve_missingness) && !spec$preserve_missingness %in% valid_missingness) {
    cli::cli_abort(c(
      "Invalid preserve_missingness: {.val {spec$preserve_missingness}}",
      "i" = "Valid values: {.val {valid_missingness}}"
    ))
  }

  # Validate free_text_strategy
  valid_free_text_strategies <- c("categorical", "drop", "redact")
  if (!is.null(spec$free_text_strategy) &&
      !spec$free_text_strategy %in% valid_free_text_strategies) {
    cli::cli_abort(c(
      "Invalid free_text_strategy: {.val {spec$free_text_strategy}}",
      "i" = "Valid values: {.val {valid_free_text_strategies}}"
    ))
  }

  # Validate label_strategy
  valid_label_strategies <- c("preserve", "mask_rare")
  if (!is.null(spec$label_strategy) &&
      !spec$label_strategy %in% valid_label_strategies) {
    cli::cli_abort(c(
      "Invalid label_strategy: {.val {spec$label_strategy}}",
      "i" = "Valid values: {.val {valid_label_strategies}}"
    ))
  }

  invisible(spec)
}

# ===========================================================================
# Privacy pre-flag hardening (C11)
# ===========================================================================

apply_privacy_hardening <- function(spec, privacy, roles) {
  if (is.null(privacy)) return(spec)
  if (is.null(attr(privacy, "stage")) || attr(privacy, "stage") != "pre") return(spec)

  flags <- if (inherits(privacy, "dataganger_privacy_check")) {
    privacy
  } else if (is.data.frame(privacy)) {
    privacy
  } else {
    return(spec)
  }

  # Extract flag variables
  flag_vars <- if ("flag" %in% names(flags)) flags$flag else character(0)

  if (length(flag_vars) == 0) return(spec)

  # If there are ID flags, reinforce that IDs should be dropped
  if (any(grepl("(?i)id", flag_vars))) {
    spec$remove_ids <- TRUE
  }

  # If free text flags exist, reinforce free_text_strategy
  if (any(grepl("(?i)free.?text", flag_vars))) {
    spec$free_text_strategy <- "drop"
  }

  # If date flags exist, reinforce coarsen_dates
  if (any(grepl("(?i)date|time", flag_vars))) {
    spec$coarsen_dates <- TRUE
  }

  spec
}

# ===========================================================================
# Engine determination
# ===========================================================================


engine_from_correlations <- function(spec) {
  pc <- spec$preserve_correlations %||% "none"
  if (pc %in% c("moderate", "high")) {
    return("synthpop")
  }
  "internal"
}

# ===========================================================================
# Print method
# ===========================================================================

#' @export
print.dataganger_spec <- function(x, ...) {
  cli::cli_h1("DataGangeR Synthesis Spec")

  cli::cli_h3("Purpose")
  cli::cli_text("{.val {x$purpose}}")

  cli::cli_h3("Level")
  cli::cli_text("{.val {x$level}}")

  if (!is.null(x$n)) {
    cli::cli_h3("Target rows")
    cli::cli_text("{x$n}")
  }

  cli::cli_h3("Key settings")
  cli::cli_li("Name strategy: {.val {x$name_strategy}}")
  cli::cli_li("Coarsen dates: {x$coarsen_dates}")
  cli::cli_li("Merge rare levels: {x$merge_rare} (min_n = {x$rare_level_min_n})")
  cli::cli_li("Rare label strategy: {.val {x$label_strategy}}")
  cli::cli_li("Minimum cell size (k-anonymity): {x$k_anon}")
  cli::cli_li("Free text strategy: {.val {x$free_text_strategy}}")
  cli::cli_li("Preserve correlations: {.val {x$preserve_correlations}}")
  cli::cli_li("Preserve missingness: {.val {x$preserve_missingness}}")
  engine <- x[["engine", exact = TRUE]] %||% "auto (derived from objective)"
  cli::cli_li("Engine: {.val {engine}}")

  if (!is.null(x$seed)) {
    cli::cli_h3("Seed")
    cli::cli_text("{x$seed}")
  }
  if (isTRUE(x$acknowledged_risk)) {
    cli::cli_alert_warning("Risk acknowledged by user")
  }

  if (x$purpose == "development") {
    cli::cli_alert_info("Relationship-aware synthesis uses synthpop when installed.")
  }

  invisible(x)
}
