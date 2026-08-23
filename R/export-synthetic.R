#' Export a synthetic data bundle
#'
#' Writes a reviewable export bundle containing the synthetic data, a human
#' guide, an optional comparison report, a combined agent recipe, the packaged
#' agent instructions, and a manifest. By default the bundle is written as a
#' zip archive.
#'
#' @param synthetic A synthetic data frame, typically from [synthesize_data()].
#' @param original Optional original data frame. When provided, used for the
#'   data dictionary, comparison fallback, privacy fallback, and exact-row guard.
#' @param comparison Optional `dataganger_comparison` object. If `NULL` and
#'   `original` is supplied, [compare_synthetic()] is run automatically.
#' @param privacy Optional `dataganger_privacy_check` object. If `NULL` and
#'   `original` is supplied, [privacy_check()] is run automatically at the post
#'   stage.
#' @param path Output path. Required. For `format = "zip"`, this is the archive
#'   path. For `format = "dir"`, this is the output directory.
#' @param format Character. One of `"zip"` or `"dir"`.
#' @param sanitize_for_spreadsheets Logical. When `TRUE` (the default),
#'   character-like cells beginning with `=`, `+`, `-`, or `@` after leading
#'   whitespace are prefixed with a single quote before CSV export.
#' @param purpose Optional purpose label for README text. Defaults to the
#'   purpose recorded in `attr(synthetic, "spec")` when available.
#' @param roles Optional recorded role decisions as a `dataganger_roles` data
#'   frame. When supplied, the export bundle includes the exact column
#'   decisions needed to reproduce the same synthetic output.
#' @param include_original_names Logical or `NULL`. Controls whether the human
#'   guide and manifest recipe preserve original variable names. When `NULL`,
#'   this defaults to `TRUE` unless `name_strategy = "dictionary_only"`, in
#'   which case it defaults to `FALSE`.
#' @param fail_on_exact_match Logical. When `TRUE`, abort export if exact-row
#'   matches are detected for `nrow(original) >= 20`. When `FALSE` (the
#'   default), exact-row matches are recorded in the privacy report and
#'   manifest, and a warning is emitted instead.
#' @param include_report Logical. When `TRUE` (the default), write
#'   `human/comparison_report.html`. If `rmarkdown`/`knitr` are unavailable,
#'   the report is skipped with a message instead of an error.
#' @param kanon_acknowledged Logical. Records whether a human explicitly
#'   acknowledged exporting a bundle whose k-anonymity backstop was infeasible.
#'   Interactive UI export uses this to clear the bundle blocker; non-interactive
#'   exports leave it `FALSE`.
#' @param exact_match_acknowledged Logical. Records whether a human explicitly
#'   acknowledged exporting a bundle in which synthetic rows reproduce real
#'   records verbatim, including values marked sensitive. Interactive UI export
#'   sets this only when the user ticks the override; non-interactive exports
#'   leave it `FALSE`.
#' @param include_dictionary Deprecated, ignored.
#' @param code_readiness Optional `dataganger_code_readiness` object from
#'   [check_code_readiness()]. When supplied, writes
#'   `agent/code_readiness_report.json` into the bundle.
#' @param compact Deprecated, ignored.
#' @param overwrite Logical. When `FALSE` (the default), existing output paths
#'   are refused.
#'
#' @return Invisibly, the written bundle path.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   age = 21:70,
#'   score = seq(10, 59),
#'   grp = rep(LETTERS[1:5], each = 10)
#' )
#' spec <- synth_spec(purpose = "demo", seed = 1, engine = "internal")
#' syn <- synthesize_data(dat, spec)
#' export_synthetic(syn, path = tempfile(fileext = ".zip"), include_report = FALSE)
export_synthetic <- function(synthetic,
                             original = NULL,
                             comparison = NULL,
                             privacy = NULL,
                             path,
                             format = c("zip", "dir"),
                             sanitize_for_spreadsheets = TRUE,
                             purpose = NULL,
                             roles = NULL,
                             include_original_names = NULL,
                             fail_on_exact_match = FALSE,
                             include_report = TRUE,
                             kanon_acknowledged = FALSE,
                             exact_match_acknowledged = FALSE,
                             include_dictionary = TRUE,
                             code_readiness = NULL,
                             compact = FALSE,
                             overwrite = FALSE) {
  format <- match.arg(format)

  if (missing(path) || !is.character(path) || length(path) != 1 || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty character string")
  }

  if (!is.data.frame(synthetic)) {
    cli::cli_abort("{.arg synthetic} must be a data frame")
  }

  if (!is.null(original) && !is.data.frame(original)) {
    cli::cli_abort("{.arg original} must be a data frame when supplied")
  }

  if (!is.null(roles) && !is.data.frame(roles)) {
    cli::cli_abort("{.arg roles} must be a data frame when supplied")
  }

  if (!is.logical(sanitize_for_spreadsheets) || length(sanitize_for_spreadsheets) != 1) {
    cli::cli_abort("{.arg sanitize_for_spreadsheets} must be TRUE or FALSE")
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    cli::cli_abort("{.arg overwrite} must be TRUE or FALSE")
  }

  if (!is.null(include_original_names) &&
      (!is.logical(include_original_names) || length(include_original_names) != 1)) {
    cli::cli_abort("{.arg include_original_names} must be TRUE, FALSE, or NULL")
  }

  if (!is.logical(fail_on_exact_match) || length(fail_on_exact_match) != 1) {
    cli::cli_abort("{.arg fail_on_exact_match} must be TRUE or FALSE")
  }

  if (!is.logical(include_report) || length(include_report) != 1) {
    cli::cli_abort("{.arg include_report} must be TRUE or FALSE")
  }

  if (!is.logical(kanon_acknowledged) || length(kanon_acknowledged) != 1) {
    cli::cli_abort("{.arg kanon_acknowledged} must be TRUE or FALSE")
  }

  if (!is.logical(exact_match_acknowledged) || length(exact_match_acknowledged) != 1) {
    cli::cli_abort("{.arg exact_match_acknowledged} must be TRUE or FALSE")
  }

  if (!is.logical(include_dictionary) || length(include_dictionary) != 1) {
    cli::cli_abort("{.arg include_dictionary} must be TRUE or FALSE")
  }

  if (!is.logical(compact) || length(compact) != 1) {
    cli::cli_abort("{.arg compact} must be TRUE or FALSE")
  }

  if (!is.null(code_readiness) &&
      !inherits(code_readiness, "dataganger_code_readiness")) {
    cli::cli_abort(
      "{.arg code_readiness} must be a dataganger_code_readiness object when supplied"
    )
  }

  spec <- attr(synthetic, "spec", exact = TRUE)
  purpose <- purpose %||% spec$purpose %||% "unspecified"
  include_original_names <- resolve_include_original_names(
    include_original_names = include_original_names,
    purpose = purpose,
    spec = spec
  )
  export_roles <- roles
  if (is.null(export_roles) && !is.null(original)) {
    export_roles <- detect_roles(original)
  }

  if (is.null(comparison) && !is.null(original)) {
    comparison <- compare_synthetic(original, synthetic, roles = export_roles)
  }

  if (is.null(privacy) && !is.null(original)) {
    privacy <- privacy_check(original, synthetic, roles = export_roles, stage = "post", spec = spec)
  }

  export_target <- prepare_export_target(path, format, overwrite)
  bundle_dir <- export_target$bundle_dir
  output_path <- export_target$output_path

  if (!identical(bundle_dir, output_path)) {
    on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
  }

  exact_row_matches <- attr(privacy, "exact_row_matches", exact = TRUE) %||% 0L
  if (!is.null(original)) {
    role_map <- NULL
    if (!is.null(export_roles) && "variable" %in% names(export_roles) &&
        "recommended_role" %in% names(export_roles)) {
      role_map <- stats::setNames(export_roles$recommended_role, export_roles$variable)
    }
    exact_row_matches <- exact_row_match_count(original, dg_original_names(synthetic), role_map)
    handle_exact_row_matches(exact_row_matches, fail_on_exact_match)
  }

  dictionary <- build_data_dictionary(original, synthetic, spec, roles = export_roles, include_original_names = include_original_names)

  csv_data <- synthetic
  if (isTRUE(sanitize_for_spreadsheets)) {
    csv_data <- sanitize_for_spreadsheet_export(csv_data)
  }

  synthetic_path <- file.path(bundle_dir, "synthetic_data.csv")
  readr::write_csv(csv_data, synthetic_path, na = "")

  human_dir <- file.path(bundle_dir, "human")
  agent_dir <- file.path(bundle_dir, "agent")
  dir.create(human_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(agent_dir, recursive = TRUE, showWarnings = FALSE)

  recipe <- recipe_to_yaml_list(spec, export_roles, include_original_names = include_original_names)
  cli_write_yaml(recipe, file.path(agent_dir, "recipe.yaml"))
  file.copy(agent_skill_path(), file.path(agent_dir, "AGENT.md"), overwrite = TRUE)

  if (!is.null(code_readiness)) {
    jsonlite::write_json(
      code_readiness_to_json(code_readiness),
      path = file.path(agent_dir, "code_readiness_report.json"),
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  }

  writeLines(
    render_human_markdown(
      synthetic,
      dictionary,
      purpose,
      include_report,
      spec,
      roles = export_roles,
      privacy = privacy,
      exact_row_matches = exact_row_matches,
      kanon = attr(synthetic, "kanon", exact = TRUE),
      kanon_acknowledged = kanon_acknowledged,
      has_code_readiness = !is.null(code_readiness)
    ),
    con = file.path(human_dir, "human.md"),
    useBytes = TRUE
  )

  if (isTRUE(include_report)) {
    if (can_render_comparison_report()) {
      render_comparison_report(
        comparison = comparison,
        privacy = privacy,
        synthetic = synthetic,
        purpose = purpose,
        output_file = file.path(human_dir, "comparison_report.html")
      )
    } else {
      message("rmarkdown/knitr not available - skipping comparison report. Install with install.packages(c('rmarkdown', 'knitr')).")
    }
  }

  write_manifest(
    bundle_dir             = bundle_dir,
    synthetic              = synthetic,
    spec                   = spec,
    purpose                = purpose,
    exact_row_matches      = exact_row_matches,
    include_original_names = include_original_names,
    original               = original,
    roles                  = export_roles,
    kanon_acknowledged     = kanon_acknowledged,
    exact_match_acknowledged = exact_match_acknowledged
  )

  if (identical(format, "zip")) {
    zip_bundle(bundle_dir, output_path)
  }

  invisible(output_path)
}

prepare_export_target <- function(path, format, overwrite) {
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    cli::cli_abort(c(
      "Parent directory does not exist: {.file {parent}}",
      "i" = "Export paths must stay inside an existing parent directory."
    ))
  }

  if (format == "dir") {
    if (file.exists(path)) {
      if (!isTRUE(overwrite)) {
        cli::cli_abort(
          "Output directory already exists at {.file {path}}; set {.arg overwrite = TRUE} to replace it"
        )
      }
      unlink(path, recursive = TRUE, force = TRUE)
    }
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(list(bundle_dir = path, output_path = path))
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "Output file already exists at {.file {path}}; set {.arg overwrite = TRUE} to replace it"
    )
  }

  if (file.exists(path) && isTRUE(overwrite)) {
    unlink(path, force = TRUE)
  }

  bundle_dir <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = parent
  )
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  list(bundle_dir = bundle_dir, output_path = path)
}

handle_exact_row_matches <- function(n_exact, fail_on_exact_match) {
  if (n_exact > 0) {
    msg <- c(
      "{n_exact} exact-row match{?es} detected between {.arg synthetic} and {.arg original}",
      "i" = "The exact-row match count is recorded in {.file agent/manifest.json} and the human Privacy section."
    )
    if (isTRUE(fail_on_exact_match)) {
      cli::cli_abort(msg)
    }
    cli::cli_warn(msg)
  }

  invisible(NULL)
}

row_key <- function(data) {
  apply(data, 1, function(row) {
    row[is.na(row)] <- "<NA>"
    paste(row, collapse = "\x01\x02\x03")
  })
}

sanitize_for_spreadsheet_export <- function(data) {
  out <- data

  for (nm in names(out)) {
    col <- out[[nm]]
    if (is.factor(col)) {
      out[[nm]] <- sanitize_text_values(as.character(col))
    } else if (is.character(col)) {
      out[[nm]] <- sanitize_text_values(col)
    }
  }

  tibble::as_tibble(out)
}

sanitize_text_values <- function(x) {
  if (length(x) == 0) {
    return(x)
  }

  is_danger <- !is.na(x) & grepl("^\\s*[=+\\-@]", x, perl = TRUE)
  x[is_danger] <- paste0("'", x[is_danger])
  x
}

build_data_dictionary <- function(original, synthetic, spec, roles = NULL,
                                  include_original_names = TRUE) {
  spec <- spec %||% list()
  name_map <- spec$name_map %||% stats::setNames(names(synthetic), names(synthetic))
  role_index <- if (!is.null(roles) && "variable" %in% names(roles)) {
    stats::setNames(seq_len(nrow(roles)), roles$variable)
  } else {
    NULL
  }

  original_names <- if (!is.null(original)) names(original) else names(name_map)
  if (length(original_names) == 0L) {
    original_names <- names(name_map)
  }

  rows <- lapply(original_names, function(orig_name) {
    syn_name <- if (orig_name %in% names(name_map)) {
      unname(name_map[[orig_name]])
    } else if (orig_name %in% names(synthetic)) {
      orig_name
    } else {
      NA_character_
    }

    syn_col <- if (!is.na(syn_name) && nzchar(syn_name) && syn_name %in% names(synthetic)) {
      synthetic[[syn_name]]
    } else {
      NULL
    }
    orig_col <- if (!is.null(original) && orig_name %in% names(original)) original[[orig_name]] else NULL
    role_row <- if (!is.null(role_index) && orig_name %in% names(role_index)) {
      roles[role_index[[orig_name]], , drop = FALSE]
    } else {
      NULL
    }

    label_meta <- extract_label_metadata(orig_col %||% syn_col)

    tibble::tibble(
      synthetic_variable = syn_name,
      original_variable = orig_name,
      original_class = class_string(orig_col),
      synthetic_class = class_string(syn_col),
      label_names = label_meta$label_names,
      label_values = label_meta$label_values,
      treatment = infer_treatment(orig_col, syn_col, syn_name, orig_name, spec, role_row = role_row)
    )
  })

  out <- dplyr::bind_rows(rows)
  if (!isTRUE(include_original_names)) {
    out$original_variable <- NULL
  }
  out
}

extract_label_metadata <- function(x) {
  lbl <- attr(x, "labels", exact = TRUE)
  if (is.null(lbl)) {
    return(list(label_names = NA_character_, label_values = NA_character_))
  }

  list(
    label_names = paste(names(lbl), collapse = "; "),
    label_values = paste(unname(lbl), collapse = "; ")
  )
}

class_string <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  paste(class(x), collapse = "/")
}

infer_treatment <- function(original_col, synthetic_col, synthetic_name, original_name, spec,
                            role_row = NULL) {
  if (identical(spec$level, "schema")) {
    return("schema_only")
  }

  role_treatment <- if (!is.null(role_row) && nrow(role_row) == 1L) {
    dg_role_treatment(role_row)[[1]]
  } else {
    NULL
  }

  if (identical(role_treatment, "drop") || is.na(synthetic_name) || !nzchar(synthetic_name)) {
    return("dropped")
  }

  if (identical(role_treatment, "pass_through")) {
    return("pass_through")
  }

  if (!identical(synthetic_name, original_name)) {
    return("renamed")
  }

  if (!is.null(original_col) && !is.null(synthetic_col) &&
      isTRUE(all(is.na(synthetic_col))) && !all(is.na(original_col))) {
    if (is.character(original_col) && any(nchar(stats::na.omit(original_col)) > 50)) {
      return("free_text_dropped")
    }
    return("masked_or_dropped")
  }

  if (inherits(original_col, "Date") && isTRUE(spec$coarsen_dates)) {
    return("coarsened_date")
  }

  "synthesized"
}

escape_r_string <- function(x) {
  gsub("\\\\", "\\\\\\\\", gsub('"', '\\"', x, fixed = TRUE))
}

format_named_character_vector <- function(x) {
  if (length(x) == 0L) {
    return("character(0)")
  }

  vals <- vapply(seq_along(x), function(i) {
    value <- x[[i]]
    value_text <- if (is.na(value)) {
      "NA_character_"
    } else {
      sprintf('"%s"', escape_r_string(as.character(value)))
    }
    sprintf('"%s" = %s', escape_r_string(names(x)[[i]]), value_text)
  }, character(1))

  paste0("c(", paste(vals, collapse = ", "), ")")
}

format_r_scalar <- function(x) {
  if (length(x) != 1L) {
    cli::cli_abort("Expected a scalar value while building the reproduction script.")
  }

  if (is.null(x)) {
    return(NULL)
  }

  if (is.na(x)) {
    return("NA")
  }

  if (is.character(x)) {
    return(sprintf('"%s"', escape_r_string(x)))
  }

  if (is.logical(x)) {
    return(if (isTRUE(x)) "TRUE" else "FALSE")
  }

  if (is.numeric(x)) {
    return(as.character(x))
  }

  sprintf('"%s"', escape_r_string(as.character(x)))
}

reproduction_spec_args <- function(spec, purpose) {
  spec <- spec %||% list()

  arg_names <- c(
    "purpose",
    "seed",
    "level",
    "engine",
    "n",
    "name_strategy",
    "k_anon",
    "rare_level_min_n",
    "preserve_missingness",
    "coarsen_dates",
    "merge_rare",
    "free_text_strategy",
    "preserve_correlations",
    "acknowledge_risk"
  )

  arg_values <- list(purpose = purpose)
  for (nm in setdiff(arg_names, "purpose")) {
    spec_name <- if (identical(nm, "acknowledge_risk")) "acknowledged_risk" else nm
    value <- spec[[spec_name]]
    if (identical(nm, "engine") && is.null(value)) {
      next
    }
    if (!is.null(value)) {
      arg_values[[nm]] <- value
    }
  }

  vapply(names(arg_values), function(nm) {
    sprintf("%s = %s", nm, format_r_scalar(arg_values[[nm]]))
  }, character(1))
}

build_reproduction_script <- function(spec, roles, purpose, include_original_names = TRUE) {
  purpose <- purpose %||% spec$purpose %||% "unspecified"

  script <- c(
    "library(dataganger)",
    "",
    "# 1. Load YOUR original data. It is NOT shipped in this bundle (privacy);",
    "#    point this at the file you synthesized from.",
    'original <- read_input("path/to/your/original-data.csv")',
    "",
    "# 2. Recreate the column decisions made in DataGangeR.",
    "roles <- detect_roles(original)"
  )

  if (!isTRUE(include_original_names)) {
    script <- c(
      script,
      "# Column names were withheld for name privacy, so per-column override",
      "# vectors are omitted. Recreate decisions in your own DataGangeR session."
    )
  } else if (is.null(roles)) {
    script <- c(
      script,
      "# No manual overrides were recorded, so detect_roles(original) is enough."
    )
  } else {
    user_role_values <- roles$user_role %||% rep(NA_character_, nrow(roles))
    names(user_role_values) <- roles$variable
    user_role_values <- user_role_values[!is.na(user_role_values) & nzchar(user_role_values)]
    if (length(user_role_values) > 0L) {
      script <- c(
        script,
        sprintf(
          "roles$user_role <- %s[roles$variable]",
          format_named_character_vector(user_role_values)
        )
      )
    } else {
      script <- c(script, "# No manual user_role overrides were recorded.")
    }

    disclosure_values <- roles$disclosure_role %||% rep(NA_character_, nrow(roles))
    names(disclosure_values) <- roles$variable
    disclosure_values <- disclosure_values[
      !is.na(disclosure_values) & nzchar(disclosure_values)
    ]
    if (length(disclosure_values) > 0L) {
      script <- c(
        script,
        sprintf(
          "roles$disclosure_role <- %s[roles$variable]",
          format_named_character_vector(disclosure_values)
        )
      )
    } else {
      script <- c(script, "# No disclosure_role overrides were recorded.")
    }

    action_values <- dg_role_treatment(roles)
    has_action_overrides <- length(action_values) > 0L &&
      !all(action_values %in% "synthesize")
    if (has_action_overrides) {
      script <- c(
        script,
        sprintf(
          "roles$simulation <- %s[roles$variable]",
          format_named_character_vector(action_values)
        )
      )
    } else {
      script <- c(script, "# All column actions used the default synthesize behavior.")
    }
  }

  script <- c(
    script,
    "",
    "# 3. Recreate the synthesis settings (same seed = same output).",
    sprintf(
      "spec <- synth_spec(\n  %s\n)",
      paste(reproduction_spec_args(spec, purpose), collapse = ",\n  ")
    ),
    "",
    "# 4. Generate the identical synthetic data.",
    "synthetic <- synthesize_data(original, spec, roles = roles)",
    "",
    "# 5. (optional) rebuild the full export bundle.",
    'export_synthetic(synthetic, original = original, path = "dataganger_bundle", format = "dir")',
    "",
    "# Exact reproduction requires the same original data and the same dataganger version."
  )

  paste(script, collapse = "\n")
}

build_dropped_variables_text <- function(dictionary) {
  dropped <- dictionary[dictionary$treatment %in% c("dropped", "free_text_dropped", "masked_or_dropped"), , drop = FALSE]
  if (nrow(dropped) == 0) {
    return("- None")
  }

  label <- if ("original_variable" %in% names(dropped)) {
    dropped$original_variable
  } else {
    dropped$synthetic_variable
  }
  label[is.na(label) | !nzchar(label)] <- dropped$synthetic_variable[is.na(label) | !nzchar(label)]
  label[is.na(label) | !nzchar(label)] <- "dropped column"

  paste(
    sprintf("- `%s`: %s", label, dropped$treatment),
    collapse = "\n"
  )
}

build_regeneration_command <- function(purpose, spec, roles = NULL, include_original_names = TRUE) {
  build_reproduction_script(
    spec = spec,
    roles = roles,
    purpose = purpose,
    include_original_names = include_original_names
  )
}

render_human_markdown <- function(synthetic, dictionary, purpose, include_report = TRUE,
                                  spec = NULL, roles = NULL, privacy = NULL,
                                  exact_row_matches = 0L,
                                  kanon = NULL,
                                  kanon_acknowledged = FALSE,
                                  has_code_readiness = FALSE) {
  file_lines <- c(
    "- `synthetic_data.csv` - the synthetic dataset. This is the main file.",
    "- `human/human.md` - this guide, including privacy notes and agent-facing guidance.",
    "- `agent/recipe.yaml` - the synthesis spec plus per-column role decisions for reproduction.",
    "- `agent/AGENT.md` - the packaged agent instructions for using this bundle safely.",
    paste0(
      "- `agent/manifest.json` - provenance: package version, synthesis engine, seed, the full ",
      "spec, disclosure metrics, and a checksum of every bundled file."
    ),
    if (isTRUE(has_code_readiness)) {
      "- `agent/code_readiness_report.json` - structural compatibility checks for whether code written on the synthetic data should run on the original."
    },
    if (isTRUE(include_report)) {
      "- `human/comparison_report.html` - a pre-rendered fidelity and privacy comparison; open it in a browser."
    }
  )

  paste(
    "# DataGangeR synthetic data bundle",
    "",
    paste0(
      "This bundle contains a synthetic copy of a dataset, generated for the ",
      sprintf("`%s` objective. ", purpose),
      "It is meant to share the structure and behaviour of the original data ",
      "without sharing the original records. Synthetic data can still preserve ",
      "sensitive patterns, so review the Privacy section below before sharing externally."
    ),
    "",
    sprintf("Rows: %s | Columns: %s", nrow(synthetic), ncol(synthetic)),
    "",
    "## Files in this bundle",
    "",
    paste(file_lines, collapse = "\n"),
    "",
    "## How each column was treated",
    "",
    paste(sprintf("- `%s`: %s", ifelse(is.na(dictionary$synthetic_variable) | !nzchar(dictionary$synthetic_variable), if ("original_variable" %in% names(dictionary)) dictionary$original_variable else "dropped column", dictionary$synthetic_variable), dictionary$treatment), collapse = "\n"),
    "",
    "## Privacy",
    "",
    "|                      | sensitive = No | sensitive = Yes |",
    "|---|---|---|",
    "| identifies = none | Synthesized from the observed distribution with noise; observed values can still recur. | Recreated from its distribution with noise; observed values can still recur - attribute-level protection is not yet applied. |",
    if (isTRUE(kanon$infeasible)) {
      "| identifies = combination | Grouping (k-anonymity) was attempted but could NOT be applied to this output - see the k-anonymity status line below. | Grouping (k-anonymity) was attempted but could NOT be applied to this output - see the k-anonymity status line below. |"
    } else {
      "| identifies = combination | Coarsened & grouped (k-anonymity), then synthesized. | Synthesized; grouped with k-anonymity so no rare combination survives. |"
    },
    "| identifies = direct | **Removed** from the output. | **Removed** from the output. |",
    "",
    render_kanon_line(kanon, kanon_acknowledged = kanon_acknowledged),
    "",
    paste(render_privacy_report(
      privacy,
      exact_row_matches,
      spec = spec %||% attr(synthetic, "spec", exact = TRUE),
      include_original_names = "original_variable" %in% names(dictionary)
    ), collapse = "\n"),
    "",
    "## Regenerate this data",
    "",
    "With the original data and the same seed, this reproduces the synthetic output:",
    "",
    "```r",
    build_regeneration_command(
      purpose,
      spec %||% attr(synthetic, "spec", exact = TRUE),
      roles = roles,
      include_original_names = "original_variable" %in% names(dictionary)
    ),
    "```",
    "",
    "## For AI assistants",
    "",
    paste0(
      "This is synthetic data designed to reduce direct disclosure risk for AI coding workflows. Use it to build ",
      "and test code, then run the same pipeline on the real data. To reproduce the ",
      "exact synthetic output, use `agent/recipe.yaml` with DataGangeR or run the command above."
    ),
    "",
    "Columns dropped from the synthetic output:",
    "",
    build_dropped_variables_text(dictionary),
    sep = "\n"
  )
}

render_kanon_line <- function(kanon, kanon_acknowledged = FALSE) {
  if (is.null(kanon)) {
    return("k-anonymity: not applicable")
  }
  if (isTRUE(kanon$infeasible)) {
    return(sprintf(
      "k-anonymity: not applied; k=%s, smallest cell=%s, acknowledged=%s",
      kanon$k %||% "unknown",
      kanon$smallest_cell %||% "unknown",
      if (isTRUE(kanon_acknowledged)) "yes" else "no"
    ))
  }
  if (identical(kanon$k_provenance %||% "default", "user_selected_after_infeasible")) {
    return(sprintf(
      "k-anonymity: k = %s, user-selected after k = %s was infeasible",
      kanon$k %||% "unknown",
      kanon$k_default %||% "unknown"
    ))
  }
  if (length(kanon$qi_cols %||% character(0)) == 0L) {
    return(sprintf("k-anonymity: not applicable; k=%s, no QI columns selected", kanon$k %||% "unknown"))
  }
  line <- sprintf(
    "k-anonymity: enforced, k=%s, smallest cell=%s",
    kanon$k %||% "unknown",
    kanon$smallest_cell %||% "unknown"
  )
  # Suppression works at whole-cell granularity, not row granularity, so
  # reaching k can suppress far more rows than the number that were
  # originally below k (see enforce_kanon()'s docs) -- call that out here
  # rather than leaving it implicit in smallest_cell alone.
  row_frac <- kanon$suppressed_row_frac %||% 0
  if (row_frac > 0) {
    line <- sprintf(
      "%s, %s%% of QI values suppressed",
      line, round(100 * row_frac)
    )
  }
  line
}

code_readiness_to_json <- function(code_readiness) {
  meta <- code_readiness$meta
  if (inherits(meta$generated_at, "POSIXt")) {
    meta$generated_at <- format(meta$generated_at, tz = "UTC", usetz = TRUE)
  }

  list(
    checks = unclass(code_readiness$checks),
    summary = code_readiness$summary,
    meta = meta
  )
}

render_privacy_report <- function(privacy, exact_row_matches = 0L,
                                  spec = NULL, include_original_names = TRUE) {
  privacy <- privacy_for_name_policy(privacy, spec, include_original_names)
  if (is.null(privacy) || nrow(privacy) == 0) {
    return(c(
      "DataGangeR privacy report",
      "",
      sprintf("Exact row matches: %s", exact_row_matches),
      "",
      "No privacy flags were supplied for this bundle."
    ))
  }

  c(
    "DataGangeR privacy report",
    "",
    sprintf("Stage: %s", attr(privacy, "stage") %||% "unknown"),
    sprintf("Exact row matches: %s", exact_row_matches),
    sprintf("Flags: %s", nrow(privacy)),
    "",
    apply(privacy, 1, function(row) {
      sprintf(
        "[%s] %s: %s | %s",
        row[["severity"]],
        row[["variable"]],
        row[["flag"]],
        row[["recommendation"]]
      )
    })
  )
}

privacy_for_name_policy <- function(privacy, spec, include_original_names = TRUE) {
  if (is.null(privacy) || isTRUE(include_original_names)) {
    return(privacy)
  }
  name_map <- spec$name_map %||% NULL
  if (is.null(name_map) || !"variable" %in% names(privacy)) {
    return(privacy)
  }
  out <- privacy
  mapped <- out$variable %in% names(name_map)
  out$variable[mapped] <- unname(name_map[out$variable[mapped]])
  out
}

render_comparison_report <- function(comparison, privacy, synthetic, purpose, output_file) {
  rlang::check_installed(c("rmarkdown", "knitr"), reason = "to render comparison reports")

  if (!isTRUE(rmarkdown::pandoc_available("1.12.3"))) {
    writeLines(
      render_comparison_html_fallback(comparison, privacy, synthetic, purpose),
      con = output_file,
      useBytes = TRUE
    )
    return(invisible(output_file))
  }

  template <- system.file("templates", "comparison-report.Rmd", package = "dataganger")
  params <- list(
    comparison = comparison,
    privacy = privacy,
    synthetic_summary = list(
      n_rows = nrow(synthetic),
      n_cols = ncol(synthetic),
      classes = vapply(synthetic, class_string, character(1))
    ),
    purpose = purpose,
    generated_at = as.character(Sys.time())
  )

  # Two separate hazards, both of which broke
  # `dataganger synthesize --out <relative-path>` -- the form a user actually
  # types. Every CLI test used tempfile(), which is absolute, so the suite
  # never covered either one.
  #
  # 1. rmarkdown resolves a relative output_file against the template's
  #    directory rather than the working directory, sending the report into
  #    inst/templates and aborting with "The directory ... does not exist".
  # 2. rmarkdown::render() does not restore the working directory on exit. It
  #    leaves it at intermediates_dir or at the template's directory, so every
  #    later relative path in the export silently resolves against the wrong
  #    root and the next write fails with "No such file or directory".
  output_file <- file.path(
    normalizePath(dirname(output_file), winslash = "/", mustWork = TRUE),
    basename(output_file)
  )
  withr::local_dir(getwd())

  rmarkdown::render(
    input = template,
    output_file = output_file,
    params = params,
    quiet = TRUE,
    envir = new.env(parent = baseenv()),
    intermediates_dir = tempdir()
  )

  invisible(output_file)
}

can_render_comparison_report <- function() {
  override <- getOption("dataganger.can_render_comparison_report", NULL)
  if (is.function(override)) {
    return(isTRUE(override()))
  }
  if (is.logical(override) && length(override) == 1L && !is.na(override)) {
    return(override)
  }
  rlang::is_installed("rmarkdown") && rlang::is_installed("knitr")
}

render_comparison_html_fallback <- function(comparison, privacy, synthetic, purpose) {
  summary_tbl <- data.frame(
    variable = names(synthetic),
    class = vapply(synthetic, class_string, character(1)),
    stringsAsFactors = FALSE
  )
  comparison_rows <- function(section) {
    if (is.null(comparison)) {
      return(NULL)
    }
    comparison[[section]]
  }
  empty_rows <- function(x) {
    is.null(x) || !is.data.frame(x) || nrow(x) == 0L
  }
  numeric_rows <- comparison_rows("numeric")
  categorical_rows <- comparison_rows("categorical")
  relationship_rows <- comparison_rows("relationship")

  paste(
    "<html><head><meta charset=\"utf-8\"><title>DataGangeR Comparison Report</title></head><body>",
    "<h1>DataGangeR Comparison Report</h1>",
    sprintf("<p><strong>Purpose:</strong> %s</p>", html_escape(purpose)),
    sprintf("<p><strong>Rows:</strong> %s<br><strong>Columns:</strong> %s</p>", nrow(synthetic), ncol(synthetic)),
    "<h2>Synthetic Column Classes</h2>",
    data_frame_to_html(summary_tbl),
    "<h2>Dataset Metrics</h2>",
    if (is.null(comparison)) "<p>No comparison object was supplied.</p>" else data_frame_to_html(comparison$dataset),
    "<h2>Numeric Comparison</h2>",
    if (empty_rows(numeric_rows)) "<p>No numeric comparison rows available.</p>" else data_frame_to_html(numeric_rows),
    "<h2>Categorical Comparison</h2>",
    if (empty_rows(categorical_rows)) "<p>No categorical comparison rows available.</p>" else data_frame_to_html(categorical_rows),
    "<h2>Relationship Comparison</h2>",
    if (empty_rows(relationship_rows)) "<p>No relationship comparison rows available.</p>" else data_frame_to_html(relationship_rows),
    "<h2>Privacy Flags</h2>",
    if (is.null(privacy) || nrow(privacy) == 0) "<p>No privacy flags were supplied for this bundle.</p>" else data_frame_to_html(privacy),
    "</body></html>",
    sep = "\n"
  )
}

data_frame_to_html <- function(x) {
  if (nrow(x) == 0) {
    return("<p>No rows available.</p>")
  }

  header <- paste(sprintf("<th>%s</th>", html_escape(names(x))), collapse = "")
  rows <- apply(as.data.frame(x, stringsAsFactors = FALSE), 1, function(row) {
    cells <- paste(sprintf("<td>%s</td>", html_escape(as.character(row))), collapse = "")
    sprintf("<tr>%s</tr>", cells)
  })

  paste0(
    "<table border=\"1\"><thead><tr>", header, "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table>"
  )
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

write_manifest <- function(bundle_dir, synthetic, spec, purpose, exact_row_matches = 0L,
                           include_original_names = TRUE, original = NULL, roles = NULL,
                           kanon_acknowledged = FALSE,
                           exact_match_acknowledged = FALSE) {
  files <- list.files(bundle_dir, full.names = TRUE, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  rel_files <- sub(paste0("^", normalizePath(bundle_dir, winslash = "/", mustWork = TRUE), "/?"), "", normalizePath(files, winslash = "/", mustWork = TRUE))
  keep <- rel_files != "agent/manifest.json"
  files <- files[keep]
  rel_files <- rel_files[keep]
  spec_for_manifest <- unclass(spec %||% list())
  if (!isTRUE(include_original_names)) {
    spec_for_manifest$name_map <- NULL
  }

  spec_json <- jsonlite::toJSON(spec_for_manifest, auto_unbox = TRUE, null = "null", pretty = TRUE)
  spec_hash <- hash_text(spec_json)
  file_hashes <- as.list(hash_files(files))
  names(file_hashes) <- rel_files

  role_treatment <- if (!is.null(roles) && nrow(roles)) dg_role_treatment(roles) else character(0)
  pass_through_cols <- names(role_treatment)[role_treatment %in% "pass_through"]
  pass_through_rows <- if (length(pass_through_cols) > 0L && !is.null(roles) && "variable" %in% names(roles)) {
    roles[roles$variable %in% pass_through_cols, , drop = FALSE]
  } else {
    NULL
  }
  raw_rows_included <- length(pass_through_cols) > 0L
  ids_included <- !is.null(pass_through_rows) && any(
    pass_through_rows$identifies %in% "direct" |
      pass_through_rows$recommended_role %in% "alphanumeric ID",
    na.rm = TRUE
  )
  free_text_included <- !is.null(pass_through_rows) && any(
    pass_through_rows$recommended_role %in% "free text",
    na.rm = TRUE
  )
  # The comparison report is the only bundle artifact that embeds plots
  # (distribution charts). Reflect its actual presence rather than hard-coding.
  plots_included <- any(rel_files == "human/comparison_report.html")

  kanon <- attr(synthetic, "kanon", exact = TRUE)
  manifest <- list(
    dataganger_version = as.character(utils::packageVersion("dataganger")),
    generated_at = as.character(Sys.time()),
    purpose = purpose,
    engine = attr(synthetic, "engine", exact = TRUE) %||% "unknown",
    synthesis_citation = if (identical(attr(synthetic, "engine", exact = TRUE), "synthpop")) {
      synthpop_citation()
    } else {
      NULL
    },
    seed = spec$seed %||% NULL,
    spec = spec_for_manifest,
    spec_hash = spec_hash,
    exact_row_matches = exact_row_matches,
    # Records that a human explicitly accepted an export in which synthetic
    # rows reproduce real records including sensitive values. FALSE whenever
    # there was nothing to accept.
    exact_match_acknowledged = isTRUE(exact_match_acknowledged),
    kanon = {
      if (is.null(kanon)) {
        list(
          applied = FALSE,
          k = NULL,
          smallest_cell = NULL,
          suppressed_cells = NULL,
          suppressed_rows = NULL,
          suppressed_row_frac = NULL,
          infeasible = NULL,
          acknowledged = isTRUE(kanon_acknowledged),
          k_default = 5L,
          k_provenance = "default"
        )
      } else {
        list(
          applied = !isTRUE(kanon$infeasible) && length(kanon$qi_cols %||% character(0)) > 0L,
          k = kanon$k %||% NULL,
          smallest_cell = kanon$smallest_cell %||% NULL,
          suppressed_cells = kanon$suppressed_cells %||% 0L,
          # Row-level suppression volume, distinct from suppressed_cells (a
          # count of distinct QI combinations folded into suppression) --
          # see enforce_kanon()'s docs. Absent (rather than 0) when
          # infeasible, since no suppression was actually applied then.
          suppressed_rows = if (isTRUE(kanon$infeasible)) NULL else kanon$suppressed_rows %||% 0L,
          suppressed_row_frac = if (isTRUE(kanon$infeasible)) NULL else kanon$suppressed_row_frac %||% 0,
          infeasible = isTRUE(kanon$infeasible),
          acknowledged = isTRUE(kanon_acknowledged),
          k_default = kanon$k_default %||% 5L,
          k_provenance = kanon$k_provenance %||% "default"
        )
      }
    },
    blockers = build_manifest_blockers(
      kanon,
      spec = spec,
      include_original_names = include_original_names,
      kanon_acknowledged = kanon_acknowledged
    ),
    synthetic_dims = list(nrow = nrow(synthetic), ncol = ncol(synthetic)),
    file_sha256 = file_hashes,
    source                  = "dataganger",
    original_rows_bucket    = if (!is.null(original)) bucket_nrows(nrow(original)) else NULL,
    original_columns_count  = if (!is.null(original)) ncol(original) else NULL,
    raw_rows_included       = raw_rows_included,
    free_text_included      = free_text_included,
    ids_included            = ids_included,
    plots_included          = plots_included,
    original_names_included = isTRUE(include_original_names),
    factor_levels_included  = isTRUE(spec$level %in% c("marginal", "hifi")),
    numeric_ranges_included = FALSE,
    policy_file             = NULL
  )

  jsonlite::write_json(
    manifest,
    path = file.path(bundle_dir, "agent", "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

build_manifest_blockers <- function(kanon, spec = NULL, include_original_names = TRUE,
                                    kanon_acknowledged = FALSE) {
  if (is.null(kanon) || !isTRUE(kanon$infeasible) || isTRUE(kanon_acknowledged)) {
    return(I(list()))
  }

  qi_cols <- kanon$qi_cols %||% character(0)
  if (!isTRUE(include_original_names) && !is.null(spec$name_map) && length(qi_cols) > 0L) {
    mapped <- qi_cols %in% names(spec$name_map)
    qi_cols[mapped] <- unname(spec$name_map[qi_cols[mapped]])
  }

  detail <- sprintf(
    paste(
      "k-anonymity was not applied at k = %s for quasi-identifier columns: %s.",
      "No k-anonymity protection was applied to this output."
    ),
    kanon$k %||% "unknown",
    paste(qi_cols, collapse = ", ")
  )

  I(list(list(type = "kanon_not_applied", detail = detail)))
}

recipe_to_yaml_list <- function(spec, roles, include_original_names = TRUE) {
  recipe <- cli_spec_to_list(spec)
  if (!isTRUE(include_original_names)) {
    recipe$name_map <- NULL
  }
  recipe$roles <- if (is.null(roles)) {
    list()
  } else {
    roles_to_yaml_list(
      roles,
      name_map = spec$name_map %||% NULL,
      include_original_names = include_original_names
    )
  }
  if (!isTRUE(include_original_names) && !is.null(spec$name_map)) {
    recipe$note <- "column names withheld (name privacy); reproduce from your own DataGangeR session"
  }
  recipe
}

agent_skill_path <- function() {
  path <- system.file("agent-skill", "SKILL.md", package = "dataganger")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Packaged skill file not found")
  }
  path
}

resolve_include_original_names <- function(include_original_names, purpose, spec) {
  if (!is.null(include_original_names)) {
    return(include_original_names)
  }

  if (identical(spec$name_strategy, "dictionary_only")) {
    return(FALSE)
  }

  TRUE
}

hash_text <- function(text) {
  digest::digest(text, algo = "sha256", serialize = FALSE)
}

hash_files <- function(files) {
  vapply(
    files,
    digest::digest,
    algo = "sha256",
    file = TRUE,
    serialize = FALSE,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

zip_bundle <- function(bundle_dir, output_path) {
  files <- list.files(bundle_dir, recursive = TRUE, all.files = FALSE, no.. = TRUE, full.names = TRUE)
  files <- files[!file.info(files)$isdir]

  # Resolve every path BEFORE calling zip::zip(). zip() switches the working
  # directory to `root` and only then forces its lazily-evaluated `files`
  # argument, so computing normalizePath(mustWork = TRUE) inline resolves the
  # relative staging paths against the wrong directory and aborts with
  # "path[1]=...: No such file or directory". An absolute bundle_dir happens to
  # be immune, which is why every tempfile()-based test passed while
  # `dataganger synthesize --out <relative-path>` failed.
  root <- normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  rel_files <- sub(
    paste0("^", root, "/?"), "",
    normalizePath(files, winslash = "/", mustWork = TRUE)
  )

  # Same reason: a relative zipfile would be created inside `root` rather than
  # where the caller asked for it.
  output_path <- file.path(
    normalizePath(dirname(output_path), winslash = "/", mustWork = TRUE),
    basename(output_path)
  )

  zip::zip(zipfile = output_path, files = rel_files, root = root)

  invisible(output_path)
}
