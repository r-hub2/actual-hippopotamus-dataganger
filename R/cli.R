#' DataGangeR command-line interface
#'
#' Testable command-line entrypoint used by the installed `exec/dataganger` shim.
#'
#' @param args Character vector of trailing command-line arguments.
#' @param quit Logical. When `TRUE`, terminate the R process using the returned
#'   status code. Tests pass `FALSE` and assert the integer code.
#'
#' @return Integer status code: `0` success, `1` processing error, `2` syntax error.
#' @export
dataganger_cli <- function(args = commandArgs(trailingOnly = TRUE), quit = FALSE) {
  status <- cli_dispatch(args)
  if (isTRUE(quit)) {
    base::quit(save = "no", status = status, runLast = FALSE)
  }
  status
}

cli_status_ok <- function() 0L
cli_status_error <- function() 1L
cli_status_usage <- function() 2L

cli_dispatch <- function(args) {
  tryCatch(
    {
      if (length(args) == 0L || identical(args[[1]], "--help") || identical(args[[1]], "-h")) {
        cli_print_help()
        return(cli_status_ok())
      }

      command <- args[[1]]
      rest <- args[-1]

      switch(
        command,
        profile = cli_cmd_profile(rest),
        roles = cli_cmd_roles(rest),
        spec = cli_cmd_spec(rest),
        synthesize = cli_cmd_synthesize(rest),
        inspect = cli_cmd_inspect(rest),
        skill = cli_cmd_skill(rest),
        "make-agent-bundle" = cli_cmd_make_agent_bundle(rest),
        "export-diagnostic" = cli_cmd_export_diagnostic(rest),
        {
          cli::cli_alert_danger("Unknown command: {command}")
          cli_status_usage()
        }
      )
    },
    dataganger_cli_usage_error = function(e) {
      cli::cli_alert_danger("{conditionMessage(e)}")
      cli_status_usage()
    },
    error = function(e) {
      cli::cli_alert_danger("{conditionMessage(e)}")
      cli_status_error()
    }
  )
}

cli_print_help <- function() {
  cat(
    paste(
      "Usage: dataganger <command> [options]",
      "",
      "Commands:",
      "  profile <data-file> --out <profile.json>",
      "  roles <data-file> --out <roles.yaml>",
      "  spec --purpose <purpose> --out <spec.yaml> [--acknowledge-risk true|false]",
      "  synthesize <data-file> (--spec <spec.yaml> [--roles <roles.yaml>] | --recipe <recipe.yaml>) --out <synthetic_bundle.zip> [--engine <auto|internal|synthpop>]",
      "  inspect <synthetic_bundle.zip>",
      "  skill [--out <file>]",
      "  make-agent-bundle <data-file> --out <bundle.zip> [--purpose <purpose>] [--seed <n>]",
      "  export-diagnostic <data-file> --out <diagnostic_view.json>",
      sep = "\n"
    ),
    "\n",
    sep = ""
  )
}

cli_usage_error <- function(message) {
  structure(
    list(message = message, call = NULL),
    class = c("dataganger_cli_usage_error", "error", "condition")
  )
}


cli_parse_options <- function(args, allowed = character()) {
  positionals <- character()
  options <- list()
  i <- 1L

  while (i <= length(args)) {
    token <- args[[i]]
    if (startsWith(token, "--")) {
      key <- substring(token, 3L)
      if (!(key %in% allowed)) {
        stop(cli_usage_error(sprintf("Unknown option --%s", key)))
      }
      if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
        stop(cli_usage_error(sprintf("Missing value for option --%s", key)))
      }
      options[[key]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      positionals <- c(positionals, token)
      i <- i + 1L
    }
  }

  list(positionals = positionals, options = options)
}

cli_require_option <- function(parsed, name) {
  value <- parsed$options[[name]]
  if (is.null(value) || !nzchar(value)) {
    stop(cli_usage_error(sprintf("Missing required option --%s", name)))
  }
  value
}

cli_require_n_positionals <- function(parsed, n, command, label) {
  if (length(parsed$positionals) != n) {
    stop(cli_usage_error(sprintf("%s requires exactly %s %s", command, if (identical(n, 1L)) "one" else as.character(n), label)))
  }
  parsed$positionals
}

cli_parse_bool <- function(value, option_name) {
  if (is.logical(value) && length(value) == 1L) {
    return(isTRUE(value))
  }

  value_chr <- tolower(trimws(as.character(value %||% "")))
  if (value_chr %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (value_chr %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }

  stop(cli_usage_error(sprintf("Option --%s must be true or false", option_name)))
}

cli_assert_existing_file <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file does not exist: %s", path), call. = FALSE)
  }
  invisible(path)
}


cli_write_json <- function(x, path) {
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(path)
}

cli_profile_to_list <- function(profile) {
  list(
    n_rows = unclass(profile)$n_rows,
    n_cols = unclass(profile)$n_cols,
    generated_at = format(unclass(profile)$generated_at, usetz = TRUE),
    profile = as.data.frame(unclass(profile)$profile)
  )
}

cli_cmd_profile <- function(args) {
  parsed <- cli_parse_options(args, allowed = "out")
  input <- cli_require_n_positionals(parsed, 1L, "profile", "data file")[[1]]
  out <- cli_require_option(parsed, "out")
  cli_assert_existing_file(input)

  data <- read_input(input)
  profile <- profile_data(data)
  cli_write_json(cli_profile_to_list(profile), out)
  cli::cli_alert_success("Wrote profile JSON: {out}")
  cli_status_ok()
}


cli_write_yaml <- function(x, path) {
  yaml::write_yaml(x, path)
  invisible(path)
}

#' @keywords internal
#' @noRd
roles_to_yaml_list <- function(roles, name_map = NULL, include_original_names = TRUE) {
  keep <- intersect(
    c(
      "variable", "identifies", "sensitive", "simulation",
      "disclosure_role", "user_role", "postal_strategy", "postal_country",
      "label_strategy"
    ),
    names(roles)
  )

  lapply(seq_len(nrow(roles)), function(i) {
    row <- as.list(roles[i, keep, drop = FALSE])
    if (!isTRUE(include_original_names) && !is.null(name_map) && length(name_map) > 0L) {
      # Columns dropped before renaming (e.g. direct identifiers) never enter
      # the name_map; their original names must still be withheld.
      row$variable <- if (as.character(roles$variable[[i]]) %in% names(name_map)) {
        unname(name_map[[as.character(roles$variable[[i]])]])
      } else {
        sprintf("withheld_%d", i)
      }
      row$user_role <- NULL
    }
    row
  })
}

#' @keywords internal
#' @noRd
cli_read_roles_yaml <- function(path, data) {
  cli_assert_existing_file(path)
  raw <- yaml::read_yaml(path)
  entries <- raw$roles %||% raw
  base <- detect_roles(data)

  for (entry in entries) {
    idx <- which(base$variable == entry$variable)
    if (!length(idx)) {
      next
    }

    if (is.null(entry$identifies) && !is.null(entry$disclosure_role)) {
      axes <- dg_role_to_axes(entry$disclosure_role)
      base$identifies[idx] <- axes$identifies
      base$sensitive[idx] <- axes$sensitive
    }

    for (field in c("identifies", "simulation", "disclosure_role", "user_role",
                    "postal_strategy", "postal_country", "label_strategy")) {
      if (!is.null(entry[[field]])) {
        base[[field]][idx] <- entry[[field]]
      }
    }

    if (!is.null(entry$sensitive)) {
      base$sensitive[idx] <- isTRUE(entry$sensitive)
    }
  }

  dg_sync_roles_axes(base)
}

cli_cmd_roles <- function(args) {
  parsed <- cli_parse_options(args, allowed = "out")
  input <- cli_require_n_positionals(parsed, 1L, "roles", "data file")[[1]]
  out <- cli_require_option(parsed, "out")
  cli_assert_existing_file(input)

  data <- read_input(input)
  profile <- profile_data(data)
  roles <- detect_roles(data, profile = profile)
  cli_write_yaml(roles_to_yaml_list(roles), out)
  cli::cli_alert_success("Wrote roles YAML: {out}")
  cli_status_ok()
}


cli_spec_to_list <- function(spec) {
  x <- unclass(spec)
  x$generated_at <- NULL
  x
}

cli_cmd_spec <- function(args) {
  parsed <- cli_parse_options(args, allowed = c("purpose", "out", "acknowledge-risk"))
  cli_require_n_positionals(parsed, 0L, "spec", "data file")
  purpose <- cli_require_option(parsed, "purpose")
  out <- cli_require_option(parsed, "out")
  acknowledge_risk <- if (!is.null(parsed$options[["acknowledge-risk"]])) {
    cli_parse_bool(parsed$options[["acknowledge-risk"]], "acknowledge-risk")
  } else {
    FALSE
  }

  spec <- synth_spec(purpose = purpose, acknowledge_risk = acknowledge_risk)
  cli_write_yaml(cli_spec_to_list(spec), out)
  cli::cli_alert_success("Wrote spec YAML: {out}")
  cli_status_ok()
}


cli_read_spec_yaml <- function(path) {
  cli_assert_existing_file(path)
  raw <- yaml::read_yaml(path)
  if (is.null(raw$purpose) || !nzchar(raw$purpose)) {
    stop("Spec YAML must contain a non-empty purpose field", call. = FALSE)
  }

  allowed <- c(
    "level", "n", "name_strategy", "seed", "engine", "preserve_correlations",
    "coarsen_dates", "merge_rare", "free_text_strategy", "geography_strategy",
    "rare_level_min_n", "preserve_missingness", "label_strategy", "k_anon"
  )
  override <- raw[intersect(names(raw), allowed)]
  acknowledge_risk <- raw$acknowledge_risk %||% raw$acknowledged_risk %||% FALSE
  spec <- do.call(
    synth_spec,
    c(list(purpose = raw$purpose, acknowledge_risk = cli_parse_bool(acknowledge_risk, "acknowledge-risk")), override)
  )
  if (!is.null(raw$disclosure_roles)) {
    if (!is.list(raw$disclosure_roles)) {
      stop("Spec YAML 'disclosure_roles' must be a mapping of column -> role", call. = FALSE)
    }
    attr(spec, "disclosure_roles") <- raw$disclosure_roles
  }
  spec
}

cli_cmd_synthesize <- function(args) {
  parsed <- cli_parse_options(args, allowed = c("spec", "recipe", "out", "roles", "engine"))
  input <- cli_require_n_positionals(parsed, 1L, "synthesize", "data file")[[1]]
  out <- cli_require_option(parsed, "out")

  recipe_path <- parsed$options[["recipe"]]
  spec_path <- parsed$options[["spec"]]
  if (is.null(recipe_path) == is.null(spec_path)) {
    stop(cli_usage_error("Provide exactly one of --spec or --recipe"))
  }
  if (!is.null(recipe_path) && !is.null(parsed$options[["roles"]])) {
    stop(cli_usage_error("Option --roles cannot be combined with --recipe"))
  }

  cli_assert_existing_file(input)
  data <- read_input(input)
  profile <- profile_data(data)

  config_path <- recipe_path %||% spec_path
  spec <- cli_read_spec_yaml(config_path)
  roles <- if (!is.null(recipe_path)) {
    cli_read_roles_yaml(recipe_path, data)
  } else {
    roles_path <- parsed$options[["roles"]]
    if (!is.null(roles_path)) {
      cli_assert_existing_file(roles_path)
      cli_read_roles_yaml(roles_path, data)
    } else {
      detected_roles <- detect_roles(data, profile = profile)
      apply_disclosure_overrides(detected_roles, attr(spec, "disclosure_roles"))
    }
  }
  pre_privacy <- privacy_check(data, roles = roles, stage = "pre")
  hardened_spec <- synth_spec(
    purpose = spec$purpose,
    level = spec$level,
    n = spec$n,
    roles = roles,
    privacy = pre_privacy,
    name_strategy = spec$name_strategy,
    seed = spec$seed,
    engine = spec[["engine", exact = TRUE]],
    acknowledge_risk = isTRUE(spec$acknowledged_risk),
    preserve_correlations = spec$preserve_correlations,
    coarsen_dates = spec$coarsen_dates,
    merge_rare = spec$merge_rare,
    label_strategy = spec$label_strategy,
    free_text_strategy = spec$free_text_strategy,
    geography_strategy = spec$geography_strategy,
    rare_level_min_n = spec$rare_level_min_n,
    preserve_missingness = spec$preserve_missingness,
    k_anon = spec$k_anon
  )
  engine    <- parsed$options[["engine"]]
  synthetic <- synthesize_data(data, hardened_spec, roles = roles, engine = engine)
  comparison <- compare_synthetic(data, synthetic, roles = roles)
  post_privacy <- privacy_check(data, synthetic, roles = roles, stage = "post", spec = hardened_spec)

  export_synthetic(
    synthetic,
    original = data,
    comparison = comparison,
    privacy = post_privacy,
    path = out,
    format = "zip"
  )
  cli::cli_alert_success("Wrote synthetic bundle: {out}")
  cli_status_ok()
}


cli_unpack_bundle_file <- function(zip_path, member, tmp) {
  utils::unzip(zip_path, files = member, exdir = tmp, junkpaths = TRUE)
  file.path(tmp, basename(member))
}

cli_read_bundle_summary <- function(zip_path) {
  cli_assert_existing_file(zip_path)
  listing <- utils::unzip(zip_path, list = TRUE)
  required <- c("agent/manifest.json", "human/human.md")
  missing <- setdiff(required, listing$Name)
  if (length(missing) > 0L) {
    stop(sprintf("Bundle is missing required file: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }

  tmp <- tempfile("dataganger-inspect-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  manifest_path <- cli_unpack_bundle_file(zip_path, "agent/manifest.json", tmp)
  human_path <- cli_unpack_bundle_file(zip_path, "human/human.md", tmp)

  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  human <- readLines(human_path, warn = FALSE)

  list(
    manifest = manifest,
    human = human,
    files = listing$Name
  )
}

cli_print_bundle_summary <- function(summary) {
  manifest <- summary$manifest
  human <- summary$human

  cat("Synthetic bundle
")
  cat(sprintf("Purpose: %s
", manifest$purpose %||% "unknown"))
  cat(sprintf("Variables: %d
", manifest$synthetic_dims$ncol %||% NA_integer_))
  cat(sprintf("Files: %d
", length(summary$files)))
  cat(sprintf("K-anonymity: %s
", cli_kanon_summary_line(manifest$kanon)))

  privacy_lines <- grep("Exact row matches:|^\\[[^]]+\\]", human, value = TRUE)
  cat("Privacy exposure ratings:
")
  if (length(privacy_lines) == 0L) {
    cat("  - No privacy report lines found
")
  } else {
    for (line in utils::head(privacy_lines, 8L)) {
      cat(sprintf("  - %s
", line))
    }
  }
}

# Reads the manifest's structured kanon block (rather than re-parsing
# human.md text) so `dataganger inspect` surfaces the same suppression
# volume as the bundle report and the Shiny app's post-generation stats --
# whole-cell suppression can silently blank far more of a QI column than
# the number of cells that were actually below k (see enforce_kanon()'s
# docs), so this is worth a dedicated line rather than folding it into the
# generic privacy-flag list above.
cli_kanon_summary_line <- function(kanon) {
  if (is.null(kanon)) {
    return("not applicable")
  }
  if (isTRUE(kanon$infeasible)) {
    return(sprintf(
      "not applied (k=%s infeasible for the selected quasi-identifiers)",
      kanon$k %||% "unknown"
    ))
  }
  if (!isTRUE(kanon$applied)) {
    return("not applicable; no quasi-identifier columns")
  }
  line <- sprintf(
    "enforced, k=%s, smallest cell=%s",
    kanon$k %||% "unknown", kanon$smallest_cell %||% "unknown"
  )
  row_frac <- kanon$suppressed_row_frac %||% 0
  if (row_frac > 0) {
    line <- sprintf("%s, %s%% of QI values suppressed", line, round(100 * row_frac))
  }
  line
}

cli_cmd_inspect <- function(args) {
  parsed <- cli_parse_options(args, allowed = character())
  bundle <- cli_require_n_positionals(parsed, 1L, "inspect", "bundle file")[[1]]
  summary <- cli_read_bundle_summary(bundle)
  cli_print_bundle_summary(summary)
  cli_status_ok()
}

cli_skill_path <- function() {
  agent_skill_path()
}

cli_cmd_skill <- function(args) {
  parsed <- cli_parse_options(args, allowed = c("out"))
  cli_require_n_positionals(parsed, 0L, "skill", "data file")
  skill_path <- cli_skill_path()
  out <- parsed$options[["out"]]

  if (is.null(out)) {
    cat(paste(readLines(skill_path, warn = FALSE), collapse = "\n"), "\n", sep = "")
    return(cli_status_ok())
  }

  if (!isTRUE(file.copy(skill_path, out, overwrite = TRUE))) {
    stop(sprintf("Failed to write skill file: %s", out), call. = FALSE)
  }
  cli::cli_alert_success("Wrote skill file: {out}")
  cli_status_ok()
}

cli_cmd_make_agent_bundle <- function(args) {
  parsed  <- cli_parse_options(args, allowed = c("out", "purpose", "seed"))
  input   <- cli_require_n_positionals(parsed, 1L, "make-agent-bundle", "data file")[[1]]
  out     <- cli_require_option(parsed, "out")
  purpose <- parsed$options[["purpose"]] %||% "development"
  seed    <- if (!is.null(parsed$options[["seed"]])) {
    as.integer(parsed$options[["seed"]])
  } else {
    NULL
  }
  cli_assert_existing_file(input)

  make_agent_bundle(input, out = out, purpose = purpose, seed = seed)
  cli::cli_alert_success("Wrote agent bundle: {out}")
  cli_status_ok()
}

cli_cmd_export_diagnostic <- function(args) {
  parsed <- cli_parse_options(args, allowed = c("out"))
  input  <- cli_require_n_positionals(parsed, 1L, "export-diagnostic", "data file")[[1]]
  out    <- cli_require_option(parsed, "out")
  cli_assert_existing_file(input)

  data <- read_input(input)
  export_diagnostic_package(data, path = out)
  cli::cli_alert_success("Wrote diagnostic schema: {out}")
  cli_status_ok()
}
