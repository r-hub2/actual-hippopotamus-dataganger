#' Disclosure-role taxonomy and mappings (internal)
#'
#' The Configure page asks two intrinsic questions per column -- does it point to
#' a person (`identifies`: none/combination/direct) and is it sensitive
#' (`sensitive`: logical). The legacy single `disclosure_role` value
#' (direct/quasi/sensitive/none) is kept as a derived projection of the axes so
#' the synthesis engine, exporters, and CLI YAML contract are unchanged.
#'
#' @keywords internal
#' @noRd
dg_identifies_option_meta <- function() {
  list(
    list(
      value = "none",
      label = "No \u2014 not a person-level identifier",
      examples = "blood pressure, lab value, score, price, row number, record index"
    ),
    list(
      value = "combination",
      label = "Only in combination with other columns",
      examples = "age, sex, ZIP/postcode, birth date, job title"
    ),
    list(
      value = "direct",
      label = "Yes \u2014 it identifies a person on its own",
      examples = "name, email, phone, address, SSN, record/account number"
    )
  )
}

#' @keywords internal
#' @noRd
dg_privacy_glossary <- function() {
  list(
    qi = paste(
      "A column that does not name a person on its own, but can identify",
      "someone when combined with others, like age plus sex plus education."
    ),
    k_anonymity = paste(
      "A rule that makes every quasi-identifier combination appear in at least",
      "k rows."
    ),
    k = "The minimum number of rows required for each quasi-identifier combination.",
    suppression = paste(
      "Blanking the quasi-identifier values in rows that still fall below k."
    ),
    cell = "A group of rows that share the same quasi-identifier combination."
  )
}

#' @keywords internal
#' @noRd
dg_privacy_term <- function(label, key) {
  title <- dg_privacy_glossary()[[key]]
  if (is.null(title)) {
    return(label)
  }

  shiny::tags$abbr(
    title = title,
    style = "text-decoration:underline dotted; cursor:help;",
    label
  )
}

#' @keywords internal
#' @noRd
dg_axes_to_role <- function(identifies, sensitive) {
  if (length(identifies) != 1) {
    identifies <- identifies[[1]]
  }
  if (length(identifies) == 0 || is.na(identifies) || !nzchar(identifies)) {
    return(NA_character_)
  }
  if (identical(identifies, "direct")) {
    return("direct")
  }
  if (identical(identifies, "combination")) {
    return("quasi")
  }
  if (isTRUE(sensitive)) "sensitive" else "none"
}

#' @keywords internal
#' @noRd
dg_role_to_axes <- function(disclosure_role) {
  if (length(disclosure_role) != 1) {
    disclosure_role <- disclosure_role[[1]]
  }
  if (is.na(disclosure_role) || !nzchar(disclosure_role)) {
    return(list(identifies = NA_character_, sensitive = FALSE))
  }
  switch(
    disclosure_role,
    direct = list(identifies = "direct", sensitive = FALSE),
    quasi = list(identifies = "combination", sensitive = FALSE),
    sensitive = list(identifies = "none", sensitive = TRUE),
    none = list(identifies = "none", sensitive = FALSE),
    list(identifies = NA_character_, sensitive = FALSE)
  )
}

#' @keywords internal
#' @noRd
dg_id_name_pattern <- function() {
  "(?i)(^id$|_id$|^subject|^patient|^record|^case(_no)?$|uuid|guid|(^|_)(key|code|num|no)$)"
}

#' Maximum number of distinct levels worth showing on the Compare page
#'
#' Scales with sample size rather than using a fixed cutoff: every displayed
#' level should have, on average, at least `rare_level_min_n` observations --
#' the same threshold synthesis already uses to decide a category is reliable
#' enough to keep instead of collapsing into `.other` (see
#' `synth_categorical()`). A column with more distinct values than this is
#' excluded from Compare's charts with a warning, since a bar chart with more
#' groups than that has, on average, too few observations per group to
#' compare meaningfully -- and free-text-as-categorical columns in particular
#' will often have close to one distinct value per row.
#'
#' @param n Number of rows in the dataset (typically `nrow(original)`).
#' @param rare_level_min_n Minimum observations per level to be considered
#'   reliable; default 5, matching `synth_categorical()`'s default.
#' @param floor_levels Always allow at least this many levels, even for tiny
#'   datasets (default 5).
#' @param cap_levels Never allow more than this many levels regardless of `n`,
#'   so the chart stays legible even for very large datasets (default 30).
#' @return Integer: the maximum number of distinct levels allowed.
#' @keywords internal
#' @noRd
dg_max_comparable_levels <- function(n, rare_level_min_n = 5,
                                      floor_levels = 5L, cap_levels = 30L) {
  if (is.null(n) || length(n) != 1L || is.na(n) || n <= 0) {
    return(floor_levels)
  }
  suggested <- floor(n / rare_level_min_n)
  as.integer(max(floor_levels, min(cap_levels, suggested)))
}

#' @keywords internal
#' @noRd
dg_named_lookup <- function(x, name) {
  if (is.null(x) || is.na(name) || !nzchar(name) || !(name %in% names(x))) {
    return(NA_character_)
  }
  x[[name]]
}

#' @keywords internal
#' @noRd
eff_role <- function(user_role, recommended_role, class_col = NA_character_) {
  if (!is.na(user_role) && nzchar(user_role)) return(user_role)
  from_rec <- dg_rec_to_role(recommended_role)
  if (!is.na(from_rec)) return(from_rec)
  dg_class_to_role(class_col)
}

#' @keywords internal
#' @noRd
dg_role_treatment <- function(roles) {
  if (is.null(roles)) {
    return(character(0))
  }
  treatment_col <- if ("simulation" %in% names(roles)) {
    "simulation"
  } else if ("treatment" %in% names(roles)) {
    "treatment"
  } else {
    NULL
  }
  if (is.null(treatment_col)) {
    vals <- rep("synthesize", nrow(roles))
  } else {
    vals <- roles[[treatment_col]]
    vals[is.na(vals) | !nzchar(vals)] <- "synthesize"
  }
  stats::setNames(vals, roles$variable)
}

#' @keywords internal
#' @noRd
dg_derived_action_axes <- function(identifies, sensitive) {
  if (length(identifies) != 1) {
    identifies <- identifies[[1]]
  }
  if (length(identifies) == 1 && !is.na(identifies) && identical(identifies, "direct")) {
    "drop"
  } else {
    "synthesize"
  }
}

#' @keywords internal
#' @noRd
dg_treatment_text_axes <- function(identifies, sensitive) {
  if (length(identifies) != 1) {
    identifies <- identifies[[1]]
  }
  if (is.na(identifies) || !nzchar(identifies)) {
    return("\u26a0 needs an answer before you can generate")
  }
  if (identical(identifies, "direct")) {
    return("Removed \u2014 not included in the synthetic data.")
  }
  if (identical(identifies, "combination")) {
    return(if (isTRUE(sensitive)) {
      "Synthesized; grouped with k-anonymity so no rare combination survives."
    } else {
      "Coarsened and grouped so no one is unique (k-anonymity), then recreated."
    })
  }
  if (isTRUE(sensitive)) {
    return("Recreated from its distribution with noise; observed values can still recur - attribute-level protection is not yet applied.")
  }
  "Recreated synthetically; values are drawn from the observed distribution with noise; observed values can still recur."
}

#' @keywords internal
#' @noRd
dg_kanon_columns <- function(roles) {
  if (is.null(roles) || !"variable" %in% names(roles)) {
    return(character(0))
  }

  discrete_classes <- c("categorical candidate", "date", "alphanumeric ID", "label_check", "postal code")
  recommended <- if ("recommended_role" %in% names(roles)) {
    roles$recommended_role
  } else {
    rep(NA_character_, nrow(roles))
  }

  if (all(c("identifies", "sensitive") %in% names(roles))) {
    combo <- roles$variable[roles$identifies %in% "combination"]
    sens <- roles$variable[isTRUE_vec(roles$sensitive) & recommended %in% discrete_classes]
    return(unique(c(combo, sens)))
  }

  if (!"disclosure_role" %in% names(roles)) {
    return(character(0))
  }
  quasi <- roles$variable[roles$disclosure_role %in% "quasi"]
  sensitive_identifying <- roles$variable[
    roles$disclosure_role %in% "sensitive" & recommended %in% discrete_classes
  ]
  unique(c(quasi, sensitive_identifying))
}

#' @keywords internal
#' @noRd
dg_sync_roles_axes <- function(roles) {
  if (is.null(roles)) {
    return(roles)
  }
  if (!"identifies" %in% names(roles)) {
    roles$identifies <- NA_character_
  }
  if (!"sensitive" %in% names(roles)) {
    roles$sensitive <- NA
  }
  non_na_s <- !is.na(roles$sensitive)
  roles$sensitive[non_na_s] <- isTRUE_vec(roles$sensitive[non_na_s])
  roles$disclosure_role <- vapply(
    seq_len(nrow(roles)),
    function(i) dg_axes_to_role(roles$identifies[i], roles$sensitive[i]),
    character(1)
  )
  if (!"simulation" %in% names(roles)) {
    roles$simulation <- NA_character_
  }
  blank <- is.na(roles$simulation) | !nzchar(roles$simulation)
  roles$simulation[blank] <- vapply(
    roles$identifies[blank],
    function(id) dg_derived_action_axes(id, FALSE),
    character(1)
  )
  roles
}

#' @keywords internal
#' @noRd
dg_suggest_disclosure <- function(class) {
  if (is.null(class) || is.na(class) || !nzchar(class)) {
    return(NA_character_)
  }

  switch(
    class,
    "alphanumeric ID" = "direct",
    "free text" = "direct",
    "date" = "quasi",
    "Date" = "quasi",
    "POSIXct" = "quasi",
    "postal code" = "quasi",
    "numeric" = "none",
    "logical" = "none",
    NA_character_
  )
}

#' @keywords internal
#' @noRd
dg_seed_disclosure <- function(roles) {
  if (is.null(roles) || !"class" %in% names(roles)) {
    return(roles)
  }
  if (!"identifies" %in% names(roles)) {
    roles$identifies <- NA_character_
  }
  if (!"sensitive" %in% names(roles)) {
    roles$sensitive <- NA
  }
  if (!"disclosure_role" %in% names(roles)) {
    roles$disclosure_role <- ""
  }
  # user_identifies/user_sensitive track explicit user selections in the
  # Configure UI.  Auto-detected values land in identifies/sensitive so the
  # synthesis engine can use them, but the dropdowns only show a pre-selected
  # value when the user has explicitly confirmed it.
  if (!"user_identifies" %in% names(roles)) {
    roles$user_identifies <- NA_character_
  }
  if (!"user_sensitive" %in% names(roles)) {
    roles$user_sensitive <- NA
  }

  blank <- is.na(roles$identifies) | !nzchar(roles$identifies)
  if (!any(blank)) {
    return(dg_sync_roles_axes(roles))
  }

  roles$identifies[blank] <- vapply(
    roles$class[blank],
    function(class_value) {
      suggestion <- dg_suggest_disclosure(class_value)
      axes <- dg_role_to_axes(suggestion)
      if (is.na(axes$identifies)) "" else axes$identifies
    },
    character(1)
  )
  dg_sync_roles_axes(roles)
}

#' @keywords internal
#' @noRd
dg_ensure_ui_roles <- function(roles) {
  if (is.null(roles)) {
    return(roles)
  }
  roles <- dg_seed_disclosure(roles)
  if ("user_identifies" %in% names(roles)) {
    blank_ui <- is.na(roles$user_identifies)
    if (any(blank_ui)) {
      roles$user_identifies[blank_ui] <- ""
    }
  }
  if ("user_sensitive" %in% names(roles)) {
    roles$user_sensitive[is.na(roles$user_sensitive)] <- NA
  }
  dg_sync_roles_axes(roles)
}

#' @keywords internal
#' @noRd
roles_generation_eligible <- function(roles) {
  if (is.null(roles) || !nrow(roles)) {
    return(logical(0))
  }

  eligible <- rep(TRUE, nrow(roles))
  if ("simulation" %in% names(roles)) {
    eligible <- !(roles$simulation %in% c("drop", "pass_through"))
    eligible[is.na(eligible)] <- TRUE
  }
  eligible
}

#' @keywords internal
#' @noRd
roles_generation_pending <- function(roles) {
  if (is.null(roles) || !nrow(roles)) {
    return(integer(0))
  }

  roles <- dg_seed_disclosure(roles)
  if (!"identifies" %in% names(roles)) {
    return(seq_len(nrow(roles)))
  }

  eligible <- roles_generation_eligible(roles)

  # When user_identifies column contains empty strings (set by ensure_simulation_column
  # in the Shiny Configure module), use the user-confirmed fields so that
  # auto-detected values don't count as answers.  CLI paths leave user_identifies
  # as all-NA and take the old branch.
  if ("user_identifies" %in% names(roles) && "user_sensitive" %in% names(roles) &&
      any(!is.na(roles$user_identifies))) {
    pending <- (!nzchar(roles$user_identifies %||% "")) | is.na(roles$user_sensitive)
    pending <- pending & eligible
    return(which(pending))
  }

  pending <- ((is.na(roles$identifies) | !nzchar(roles$identifies)) | is.na(roles$sensitive)) & eligible
  which(pending)
}

#' @keywords internal
#' @noRd
roles_ready_for_generation <- function(roles) {
  if (is.null(roles) || !nrow(roles)) {
    return(FALSE)
  }
  length(roles_generation_pending(roles)) == 0L
}

#' @keywords internal
#' @noRd
app_fail_safe_empty <- function() {
  data.frame(
    variable = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
app_fail_safe_token <- function(state) {
  roles <- state$roles
  data <- state$raw_data
  if (is.null(roles) || is.null(data)) {
    return(NULL)
  }
  paste(
    nrow(data),
    ncol(data),
    paste(roles$variable, collapse = "|"),
    sep = "::"
  )
}

#' @keywords internal
#' @noRd
.app_refuse <- function(...) shiny::stopApp(...)

#' @keywords internal
#' @noRd
app_attestation_modal <- function(ns = shiny::NS(NULL)) {
  box_style <- paste(
    "margin-top:12px; padding:12px 14px; border-radius:6px;",
    "background:var(--risk-50, #FEF3C7);",
    "border:1px solid var(--risk-500, #F59E0B);",
    "border-left:4px solid var(--risk-500, #F59E0B);"
  )
  callout <- function(icon, text, style = box_style, text_color = "var(--risk-700, #B45309)") {
    shiny::tags$div(
      style = style,
      shiny::tags$div(
        style = "display:flex; gap:10px; align-items:flex-start;",
        shiny::tags$span(style = "font-size:18px; line-height:1.3;", icon),
        shiny::tags$span(
          style = sprintf("color:%s; font-weight:600;", text_color),
          text
        )
      )
    )
  }
  info_box_style <- paste(
    "margin-top:12px; padding:12px 14px; border-radius:6px;",
    "background:var(--info-50, #EFF6FF);",
    "border:1px solid var(--info-500, #3B82F6);",
    "border-left:4px solid var(--info-500, #3B82F6);"
  )
  synthpop_notice <- if (!synthpop_available()) {
    callout(
      "\U0001F4E6",
      shiny::tagList(
        "For correlation-aware synthesis that preserves relationships between variables, we recommend installing the ",
        shiny::tags$strong("synthpop"),
        " package. Without it, columns are synthesized independently. Install it with ",
        shiny::tags$code("install.packages(\"synthpop\")"),
        " and restart the app."
      ),
      style = info_box_style,
      text_color = "var(--info-700, #1D4ED8)"
    )
  }
  shiny::modalDialog(
    title = shiny::tags$span(
      style = "color:var(--risk-700, #B45309); font-weight:700;",
      "Read before you continue"
    ),
    callout(
      "\u2139\ufe0f",
      "Your data is processed locally on your machine, in memory only. It is never uploaded, never sent anywhere, and never written to disk by this app. Nothing is retained after you close it. Feel free to disable your internet connection while using this package."
    ),
    callout(
      "\u26a0\ufe0f",
      "By using this app I confirm there are no direct identifiers \u2014 including institutional identifiers \u2014 in this dataset (for example: name, email, healthcare/medical record number, national ID, phone, address)."
    ),
    synthpop_notice,
    footer = shiny::tagList(
      shiny::actionButton(ns("refuse"), "I do not agree", class = "btn btn-secondary"),
      shiny::actionButton(ns("agree"), "I agree", class = "btn btn-primary")
    ),
    easyClose = FALSE
  )
}

#' @keywords internal
#' @noRd
app_fail_safe_modal <- function(flagged, ns = shiny::NS(NULL)) {
  lines <- apply(flagged, 1L, function(row) {
    shiny::tags$li(
      shiny::tags$code(row[["variable"]]),
      shiny::tags$span(
        style = "margin-left:6px; font-size:11px; padding:1px 6px; border-radius:999px; background:var(--risk-50); color:var(--risk-700); border:1px solid var(--risk-200);",
        "potential identifier"
      )
    )
  })
  shiny::modalDialog(
    title = "Possible direct identifiers flagged",
    shiny::tags$p(
      "We detected columns that might point to a person. You are still responsible for confirming."
    ),
    shiny::tags$ul(lines),
    footer = shiny::tagList(
      shiny::actionButton(ns("abort_flagged"), "Abort", class = "btn btn-danger"),
      shiny::actionButton(ns("drop_flagged"), "Drop these columns", class = "btn btn-warning"),
      shiny::actionButton(ns("confirm_keep_flagged"), "Confirm and keep", class = "btn btn-success")
    ),
    easyClose = FALSE
  )
}

#' Replace disclosure jargon with plain language for user-facing strings.
#'
#' The disclosure-risk modal must not show terms users do not understand
#' ("k-anonymity", "k-anon", "quasi-identifier"). Flag text built elsewhere
#' (e.g. `privacy_check_post()`) can contain them, so it is passed through here
#' at display time. This is a presentation-layer transform only: the source
#' strings in other files are left untouched.
#'
#' @keywords internal
#' @noRd
dg_plain_language <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  # Replace the longer forms first so their substrings are not mangled:
  # "k-anon" is a prefix of "k-anonymity", "quasi-identifier" of the plural.
  x <- gsub("quasi-identifiers", "grouping columns", x, ignore.case = TRUE)
  x <- gsub("quasi-identifier", "grouping column", x, ignore.case = TRUE)
  x <- gsub("k-anonymity", "group-size protection", x, ignore.case = TRUE)
  x <- gsub("k-anon", "group-size", x, ignore.case = TRUE)

  # The HIGH-severity flag privacy_check_post() raises when the target is
  # missed reads "Synthetic output has a QI cell of size 3 (< k=5)", with the
  # recommendation "review enforce_kanon settings". None of the forms above
  # touch it: it says "QI", not "quasi-identifier", and "k=5", not
  # "k-anonymity". That is the flag a user most needs to understand, so the
  # abbreviation, the bare k notation, and the internal function name are
  # translated too.
  x <- gsub("QI cell", "group", x)
  x <- gsub("\\bQIs\\b", "grouping columns", x)
  x <- gsub("\\bQI\\b", "grouping column", x)
  x <- gsub("review enforce_kanon settings",
            "review the minimum group size setting", x)
  x <- gsub("\\benforce_kanon\\b", "the minimum group size setting", x)
  x <- gsub("\\(< *k *= *([0-9]+)\\)", "(fewer than the \\1 required)", x)
  x <- gsub("\\bk *= *([0-9]+)", "a minimum group size of \\1", x)
  x
}

#' Informational disclosure-risk modal shown on arrival at the Export step.
#'
#' Pure presentation: it briefs the user on the protections applied and the
#' risks that remain, and grants nothing. The inline acknowledgment gates and
#' the `build_export()` stops are untouched. User-facing copy describes what
#' each protection DOES and avoids jargon; flag text from `privacy_check_post()`
#' is passed through `dg_plain_language()` for the same reason.
#'
#' @param kanon The stored `kanon` attribute (or NULL). Fields used: `k`,
#'   `infeasible`, `qi_cols`, `suppressed_rows`, `suppressed_row_frac`.
#' @param n_exact Number of synthetic rows reproducing a source row exactly.
#' @param n_sensitive Of those, the number exposing a populated sensitive value.
#' @param privacy_flags A flags frame from `privacy_check_post()` (or NULL).
#' @keywords internal
#' @noRd
disclosure_risk_modal <- function(kanon = NULL,
                                  n_exact = 0L,
                                  n_sensitive = 0L,
                                  privacy_flags = NULL) {
  section_style <- "margin-top:16px;"
  label_style <- paste(
    "font-weight:700; font-size:12px; text-transform:uppercase;",
    "letter-spacing:.05em; color:var(--fg-muted, #6b7280); margin-bottom:6px;"
  )

  col_chips <- function(cols) {
    if (length(cols) == 0L) {
      return(shiny::tags$p(class = "help", "No columns were grouped."))
    }
    shiny::tagList(
      shiny::tags$div(style = "margin-top:6px;", "Columns this applies to:"),
      shiny::tags$div(
        style = "display:flex; flex-wrap:wrap; gap:6px; margin-top:4px;",
        lapply(cols, function(col) shiny::tags$code(col))
      )
    )
  }

  k_section <- if (is.null(kanon)) {
    shiny::tags$div(
      style = section_style,
      shiny::tags$div(style = label_style, "Group size protection"),
      shiny::tags$p("No group-size protection was measured for this output.")
    )
  } else if (isTRUE(kanon$infeasible)) {
    shiny::tags$div(
      style = section_style,
      shiny::tags$div(style = label_style, "Group size protection"),
      shiny::tags$p(
        "This protection could not be applied to this output. Some rows may ",
        "still stand out because of the columns below."
      ),
      col_chips(kanon$qi_cols)
    )
  } else {
    k <- kanon$k
    sentence <- if (is.null(k) || length(k) != 1L || is.na(k)) {
      paste(
        "Every combination of the columns below appears in enough rows that",
        "no individual row stands out."
      )
    } else {
      sprintf(
        paste(
          "Every combination of the columns below appears at least %s times in",
          "the synthetic data, so no individual row stands out."
        ),
        format(k, trim = TRUE)
      )
    }
    suppressed_rows <- kanon$suppressed_rows %||% 0L
    frac <- kanon$suppressed_row_frac %||% 0
    blanked <- if (suppressed_rows > 0L) {
      sprintf(
        paste(
          "To reach this, %d row(s) (%.1f%% of the data) had their values in",
          "these columns blanked."
        ),
        suppressed_rows, 100 * frac
      )
    } else {
      "No rows needed to be blanked to reach this."
    }
    shiny::tags$div(
      style = section_style,
      shiny::tags$div(style = label_style, "Group size protection"),
      shiny::tags$p(sentence),
      col_chips(kanon$qi_cols),
      shiny::tags$p(class = "help", style = "margin-top:6px;", blanked)
    )
  }

  exact_section <- shiny::tags$div(
    style = section_style,
    shiny::tags$div(style = label_style, "Exact copies of real records"),
    shiny::tags$p(
      sprintf("%d synthetic row(s) reproduce a real record exactly.", n_exact)
    ),
    shiny::tags$p(
      if (n_sensitive > 0L) {
        sprintf("Of those, %d expose a value you marked sensitive.", n_sensitive)
      } else {
        "None of those expose a value you marked sensitive."
      }
    )
  )

  severity_badge <- function(severity) {
    palette <- list(
      HIGH   = c(bg = "#FEE2E2", fg = "#B91C1C", border = "#FCA5A5"),
      MEDIUM = c(bg = "#FEF3C7", fg = "#B45309", border = "#FCD34D"),
      LOW    = c(bg = "#DBEAFE", fg = "#1D4ED8", border = "#93C5FD")
    )
    col <- palette[[severity]] %||% palette[["LOW"]]
    shiny::tags$span(
      style = sprintf(
        paste(
          "display:inline-block; font-size:10px; font-weight:700; padding:1px 6px;",
          "border-radius:999px; background:%s; color:%s; border:1px solid %s;",
          "margin-right:6px;"
        ),
        col[["bg"]], col[["fg"]], col[["border"]]
      ),
      severity
    )
  }

  flags_section <- if (is.null(privacy_flags) || nrow(privacy_flags) == 0L) {
    shiny::tags$div(
      style = section_style,
      shiny::tags$div(style = label_style, "Other checks"),
      shiny::tags$p("No other disclosure risks were flagged.")
    )
  } else {
    items <- lapply(seq_len(nrow(privacy_flags)), function(i) {
      variable <- dg_plain_language(as.character(privacy_flags$variable[i]))
      flag <- dg_plain_language(as.character(privacy_flags$flag[i]))
      recommendation <- dg_plain_language(as.character(privacy_flags$recommendation[i]))
      severity <- as.character(privacy_flags$severity[i])
      shiny::tags$li(
        style = "margin-bottom:8px;",
        severity_badge(severity),
        shiny::tags$strong(variable),
        ": ",
        flag,
        shiny::tags$div(
          class = "help",
          style = "margin-top:2px;",
          "Recommendation: ", recommendation
        )
      )
    })
    shiny::tags$div(
      style = section_style,
      shiny::tags$div(style = label_style, "Other checks"),
      shiny::tags$ul(
        style = "list-style:none; padding-left:0; margin:6px 0 0;",
        items
      )
    )
  }

  shiny::modalDialog(
    title = shiny::tags$span(
      style = "font-weight:700;",
      "Disclosure risk of this bundle"
    ),
    shiny::tags$p(
      "A quick brief on the protections applied and the risks that remain in ",
      "the bundle you are about to download. This does not change any of the ",
      "checks on the page."
    ),
    k_section,
    exact_section,
    flags_section,
    footer = shiny::modalButton("Continue"),
    easyClose = TRUE,
    size = "l"
  )
}

#' @keywords internal
#' @noRd
app_guardrail_server <- function(id, state, app_refuse = .app_refuse) {
  shiny::moduleServer(id, function(input, output, session) {
    show_fail_safe <- function(flagged) {
      shiny::showModal(app_fail_safe_modal(flagged, ns = session$ns))
    }

    resolve_flagged_roles <- function(mode = c("confirm_keep", "drop")) {
      mode <- match.arg(mode)
      flagged <- state$fail_safe_flagged
      roles <- state$roles
      if (is.null(roles) || !is.data.frame(flagged) || !nrow(flagged)) {
        return(invisible(NULL))
      }

      idx <- roles$variable %in% flagged$variable
      if (!any(idx)) {
        return(invisible(NULL))
      }

      roles$identifies[idx] <- ""
      roles$disclosure_role[idx] <- NA_character_
      roles <- dg_sync_roles_axes(roles)
      if (identical(mode, "drop")) {
        roles$simulation[idx] <- "drop"
      } else {
        # Alpha-numeric ID's default treatment is to scramble, not synthesize.
        recommended <- if ("recommended_role" %in% names(roles)) {
          roles$recommended_role[idx]
        } else {
          rep(NA_character_, sum(idx))
        }
        roles$simulation[idx] <- ifelse(
          recommended %in% "alphanumeric ID", "scramble", "synthesize"
        )
      }
      state$roles <- roles
      state$fail_safe_status <- "ready"
      state$fail_safe_upload_token <- app_fail_safe_token(state)
      shiny::removeModal()
      invisible(NULL)
    }

    shiny::observe({
      if (!isTRUE(state$attested_no_direct)) {
        shiny::showModal(app_attestation_modal(ns = session$ns))
      }
    })

    shiny::observeEvent(input$agree, ignoreNULL = TRUE, {
      state$attested_no_direct <- TRUE
      shiny::removeModal()
    })

    shiny::observeEvent(input$refuse, ignoreNULL = TRUE, {
      app_refuse()
    })

    shiny::observe({
      if (!isTRUE(state$attested_no_direct)) {
        return()
      }
      if (is.null(state$raw_data) || is.null(state$roles)) {
        return()
      }

      token <- app_fail_safe_token(state)
      if (isTRUE(state$fail_safe_status == "pending") ||
          identical(state$fail_safe_upload_token, token)) {
        return()
      }

      flagged <- suspected_direct_identifiers(state$roles)
      state$fail_safe_flagged <- flagged

      if (nrow(flagged) > 0L) {
        state$fail_safe_status <- "pending"
        show_fail_safe(flagged)
      } else {
        state$fail_safe_status <- "ready"
        state$fail_safe_upload_token <- token
      }
    })

    shiny::observeEvent(input$confirm_keep_flagged, ignoreNULL = TRUE, {
      resolve_flagged_roles("confirm_keep")
    })

    shiny::observeEvent(input$drop_flagged, ignoreNULL = TRUE, {
      resolve_flagged_roles("drop")
    })

    shiny::observeEvent(input$abort_flagged, ignoreNULL = TRUE, {
      shiny::removeModal()
      state$upload_source <- NULL
      state$raw_data <- NULL
      state$profile <- NULL
      state$roles <- NULL
      state$column_filter <- NULL
      state$filename <- NULL
      state$fail_safe_status <- "idle"
      state$fail_safe_flagged <- app_fail_safe_empty()
      state$fail_safe_upload_token <- NULL
      state$nav_request <- "upload"
    })

    invisible(NULL)
  })
}

#' Columns that look like direct identifiers, with a human reason.
#'
#' Assistive only -- heuristic, not a guarantee.
#' @keywords internal
#' @noRd
suspected_direct_identifiers <- function(roles) {
  if (is.null(roles) || !nrow(roles)) {
    return(data.frame(
      variable = character(0),
      reason = character(0),
      stringsAsFactors = FALSE
    ))
  }

  recommended_role <- if ("recommended_role" %in% names(roles)) {
    as.character(roles$recommended_role)
  } else {
    rep(NA_character_, nrow(roles))
  }
  identifies <- if ("identifies" %in% names(roles)) {
    as.character(roles$identifies)
  } else {
    rep(NA_character_, nrow(roles))
  }
  variables <- if ("variable" %in% names(roles)) {
    as.character(roles$variable)
  } else {
    rep("", nrow(roles))
  }

  reason <- rep(NA_character_, nrow(roles))
  reason[identifies %in% "direct"] <- "marked as a direct identifier"
  reason[is.na(reason) & recommended_role %in% "alphanumeric ID"] <-
    "looks like an ID (high-cardinality / ID-shaped)"
  reason[is.na(reason) & recommended_role %in% "free text"] <-
    "free text may contain names or identifying details"

  keep <- !is.na(reason)
  data.frame(
    variable = variables[keep],
    reason = reason[keep],
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
#' @noRd
isTRUE_vec <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  tolower(as.character(x)) %in% c("true", "yes", "1")
}

#' @keywords internal
#' @noRd
dg_decision_recap_table <- function(roles) {
  if (is.null(roles) || !nrow(roles)) {
    return(data.frame(
      variable = character(0),
      points_to_person = character(0),
      sensitive = character(0),
      action = character(0),
      what_we_do = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    ))
  }

  pick_col <- function(name, default) {
    if (name %in% names(roles)) roles[[name]] else rep(default, nrow(roles))
  }

  variable <- pick_col("variable", "")
  identifies <- pick_col("identifies", NA_character_)
  sensitive_raw <- pick_col("sensitive", FALSE)
  user_role <- pick_col("user_role", NA_character_)
  recommended_role <- pick_col("recommended_role", NA_character_)
  class_col <- pick_col("class", NA_character_)

  sensitive <- isTRUE_vec(sensitive_raw)
  treatment <- unname(dg_role_treatment(roles))
  treatment[is.na(treatment) | !nzchar(treatment)] <- "synthesize"

  identifies_meta <- dg_identifies_option_meta()
  identifies_labels <- stats::setNames(
    vapply(identifies_meta, `[[`, character(1), "label"),
    vapply(identifies_meta, `[[`, character(1), "value")
  )

  action_label <- function(x) {
    switch(
      x,
      synthesize = "Synthesize",
      pass_through = "Pass through",
      drop = "Drop",
      x
    )
  }

  points_to_person <- unname(identifies_labels[identifies])
  points_to_person[is.na(points_to_person) | is.na(identifies) | !nzchar(identifies)] <- "\u2014"

  data.frame(
    variable = as.character(variable),
    points_to_person = points_to_person,
    sensitive = ifelse(sensitive, "Yes", "No"),
    action = vapply(treatment, action_label, character(1)),
    what_we_do = vapply(
      seq_len(nrow(roles)),
      function(i) dg_treatment_text_axes(identifies[[i]], sensitive[[i]]),
      character(1)
    ),
    type = vapply(
      seq_len(nrow(roles)),
      function(i) eff_role(user_role[[i]], recommended_role[[i]], class_col[[i]]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}
