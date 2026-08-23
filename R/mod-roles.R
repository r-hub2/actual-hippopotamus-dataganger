#' Internal Shiny Roles Module
#'
#' @keywords internal
#' @noRd
NULL

#' @keywords internal
#' @noRd
dg_rec_to_role <- function(rec) {
  if (is.na(rec) || !nzchar(rec)) return(NA_character_)
  lc <- tolower(rec)
  # There is no separate "pseudo identifier" type any more -- anything that
  # looks like an identifier, structured or not, is an alphanumeric ID.
  if (grepl("postal", lc)) return("postal_code")
  if (grepl("alphanumeric", lc)) return("alphanumeric_id")
  if (grepl("id\\b|identifier", lc)) return("alphanumeric_id")
  if (grepl("categor", lc)) return("categorical")
  if (grepl("free.text|free_text", lc)) return("free_text")
  if (grepl("\\bdate\\b", lc)) return("date")
  # Logical/boolean is not a distinct role -- it is treated as categorical.
  if (grepl("logic|boolean", lc)) return("categorical")
  if (grepl("numeric", lc)) return("numeric")
  NA_character_
}

#' @keywords internal
#' @noRd
dg_class_to_role <- function(cls) {
  if (is.na(cls) || !nzchar(cls)) return("numeric")
  lc <- tolower(cls)
  if (grepl("date|posix", lc)) return("date")
  # Logical/boolean is not a distinct role -- it is treated as categorical.
  if (grepl("logical", lc)) return("categorical")
  if (grepl("char|factor", lc)) return("categorical")
  "numeric"
}


#' Which actions a data type may be given, in display order
#'
#' The Action dropdown is type-aware: a free-text column has no meaningful
#' "simulate from a distribution" behaviour, and an alphanumeric ID has no
#' meaningful resample, so offering every action on every column invited
#' choices that silently did something other than their label.
#'
#' These are *UI tokens*, not storage values. The stored `simulation` column
#' keeps its four-value vocabulary (synthesize / pass_through / scramble /
#' drop) because it is the agent-bundle and CLI recipe contract; the extra
#' tokens here (`postal_resample` and `mask_rare`) are translated on write,
#' never persisted in `simulation`.
#' The first entry is the default for that type.
#'
#' @param role Effective data type, e.g. `"free_text"`.
#' @return Character vector of action tokens.
#' @keywords internal
#' @noRd
dg_action_options <- function(role) {
  switch(
    role %||% "",
    alphanumeric_id = c("scramble", "pass_through", "drop"),
    free_text       = c("synthesize", "mask_rare", "scramble", "pass_through", "drop"),
    postal_code     = c("synthesize", "postal_resample", "pass_through", "drop"),
    categorical     = c("synthesize", "mask_rare", "pass_through", "drop"),
    numeric         = c("synthesize", "pass_through", "drop"),
    date            = c("synthesize", "pass_through", "drop"),
    c("synthesize", "pass_through", "drop")
  )
}

#' Display label for an action, which depends on the type it applies to
#'
#' One stored action can be honestly described several ways: `synthesize`
#' resamples observed values for categorical and free-text columns, draws from
#' a fitted distribution for numeric and date columns, and builds new
#' format-valid values for postal codes. The old single label "Synthesise"
#' covered all three and told the user nothing about which they were getting.
#'
#' @param action Action token from `dg_action_options()`.
#' @param role Effective data type.
#' @return A single display string.
#' @keywords internal
#' @noRd
dg_action_label <- function(action, role) {
  # Postal resample is the same action as categorical/free-text resample --
  # draw from the values observed in the data -- so it carries the same label.
  # Within a postal column the contrast is "Generate new" vs "Resample", which
  # needs no further qualifier.
  if (identical(action, "postal_resample")) {
    return("Resample")
  }
  if (identical(action, "mask_rare")) {
    return("Resample (rare levels masked)")
  }
  if (identical(action, "synthesize")) {
    return(switch(
      role %||% "",
      categorical = "Resample",
      free_text   = "Resample",
      postal_code = "Generate new",
      "Simulate"
    ))
  }
  switch(
    action %||% "",
    scramble     = "Scramble",
    pass_through = "Pass through",
    drop         = "Drop",
    action
  )
}

#' Stored action plus strategy -> the token the dropdown shows
#'
#' Inverse of the translation `dg_apply_action_token()` performs on write.
#' Postal codes and masked categorical labels store `simulation ==
#' "synthesize"` for both strategies, so the separate strategy is what
#' distinguishes them.
#'
#' @param simulation Stored `simulation` value.
#' @param role Effective data type.
#' @param postal_strategy Stored `postal_strategy`, or `NA`.
#' @param label_strategy Stored `label_strategy`, or `NA`.
#' @return A single action token.
#' @keywords internal
#' @noRd
action_token <- function(simulation, role, postal_strategy = NA_character_,
                         label_strategy = NA_character_) {
  sim <- simulation %||% "synthesize"
  if (is.na(sim) || !nzchar(sim)) sim <- "synthesize"
  if (identical(role, "postal_code") && identical(sim, "synthesize") &&
      !is.na(postal_strategy) && identical(postal_strategy, "resample")) {
    return("postal_resample")
  }
  if (role %in% c("categorical", "free_text") && identical(sim, "synthesize") &&
      identical(label_strategy, "mask_rare")) {
    return("mask_rare")
  }
  sim
}

#' Write an action token onto a roles row
#'
#' Translates a UI token into the stored columns: `simulation` keeps its
#' four-value contract vocabulary, and per-column strategies are written
#' separately.
#' Rejects any token the type does not offer, so a hand-crafted browser
#' message cannot set an action the dropdown would never have shown -- e.g.
#' `scramble` on a numeric column, which would run the identifier scrambler
#' over numbers.
#'
#' @param roles Roles data frame.
#' @param orig_row Row index into `roles`.
#' @param token Action token from `dg_action_options()`.
#' @param role Effective data type for that row.
#' @return The modified `roles`, or unchanged when `token` is not allowed.
#' @keywords internal
#' @noRd
dg_apply_action_token <- function(roles, orig_row, token, role) {
  if (!token %in% dg_action_options(role)) {
    return(roles)
  }
  if (identical(token, "postal_resample")) {
    roles$simulation[[orig_row]] <- "synthesize"
    if (!"postal_strategy" %in% names(roles)) roles$postal_strategy <- NA_character_
    roles$postal_strategy[[orig_row]] <- "resample"
  } else if (identical(token, "mask_rare")) {
    roles$simulation[[orig_row]] <- "synthesize"
    if (!"label_strategy" %in% names(roles)) roles$label_strategy <- NA_character_
    roles$label_strategy[[orig_row]] <- "mask_rare"
  } else {
    roles$simulation[[orig_row]] <- token
    if (identical(role, "postal_code") && identical(token, "synthesize")) {
      if (!"postal_strategy" %in% names(roles)) roles$postal_strategy <- NA_character_
      roles$postal_strategy[[orig_row]] <- "generate"
    }
    if (role %in% c("categorical", "free_text") && identical(token, "synthesize")) {
      if (!"label_strategy" %in% names(roles)) roles$label_strategy <- NA_character_
      roles$label_strategy[[orig_row]] <- "preserve"
    }
  }
  # Record the action as a user override, alongside user_role /
  # user_identifies / user_sensitive. Without this the choice lives only in
  # the derived `simulation` column and any later re-derivation silently
  # discards it.
  if (!"user_simulation" %in% names(roles)) {
    roles$user_simulation <- NA_character_
  }
  roles$user_simulation[[orig_row]] <- roles$simulation[[orig_row]]
  roles
}

#' Question-1 (identifies axis) choices.
#'
#' All three choices are always offered, including after the
#' no-direct-identifier attestation. Hiding `direct` when attested made a
#' column's own state unrepresentable in its control: columns seeded as direct
#' rendered with no option selected and no "needs an answer" styling, so the
#' user could neither see nor change the classification that decides whether
#' the column is dropped. An attestation that contradicts a column-level answer
#' is surfaced as a warning instead of by removing the answer.
#'
#' @param attested Retained for call-site compatibility; no longer filters.
#' @keywords internal
#' @noRd
q1_identifies_choices <- function(attested = FALSE) {
  c("none", "combination", "direct")
}


#' @keywords internal
#' @noRd
mod_roles_ui <- function(id, embedded = FALSE) {
  rlang::check_installed(
    c("shiny", "DT"),
    reason = "to use the DataGangeR Shiny modules"
  )

  ns <- shiny::NS(id)

  header <- if (!isTRUE(embedded)) {
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 03 \u00b7 Column Roles"),
        shiny::tags$h1("Review column roles"),
        shiny::tags$p(
          class = "subtitle",
          "DataGangeR auto-detected each column's role. ",
          shiny::tags$strong("Adjust any that look wrong"),
          " before generating \u2014 roles control whether columns are coarsened, redacted, regenerated, or dropped. Alphanumeric IDs and free text are always handled with extra care."
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::actionButton(
          ns("confirm"),
          "Confirm and Continue \u2192",
          class = "btn btn-primary"
        )
      )
    )
  } else {
    NULL
  }

  shiny::tagList(
    header,
    shiny::tags$div(
      class = "banner info",
      shiny::tags$span(class = "icon", "i"),
      shiny::uiOutput(ns("roles_banner_text"))
    ),
    shiny::uiOutput(ns("agg_warning")),
    shiny::tags$div(
      class = "card",
      shiny::tags$div(
        class = "card-header",
        shiny::uiOutput(ns("roles_card_header"))
      ),
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:12px; flex-wrap:wrap;",
        shiny::uiOutput(ns("filter_chips")),
        shiny::tags$input(
          type        = "text",
          class       = "input",
          id          = ns("col_search"),
          placeholder = "filter columns\u2026",
          oninput     = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority:'event'})",
            ns("col_search_val")
          ),
          style = "margin-left:auto; width:200px; padding:4px 8px; font-size:12px;"
        )
      ),
      type_action_legend_ui(),
      shiny::uiOutput(ns("disclosure_help")),
      shiny::uiOutput(ns("bulk_toolbar")),
      shiny::uiOutput(ns("roles_table")),
      shiny::uiOutput(ns("kanon_readout")),
      shiny::uiOutput(ns("disclosure_gate"))
    ),
    if (!isTRUE(embedded)) {
      shiny::tags$div(
        class = "main-header-action",
        style = "display:flex; justify-content:flex-end; margin-top:16px;",
        shiny::actionButton(
          ns("confirm_bottom"),
          "Confirm and Continue \u2192",
          class = "btn btn-primary"
        )
      )
    }
  )
}

#' Inline two-question classifier explainer
#'
#' Renders the two intrinsic questions (does it point to a person? is it
#' sensitive?) with one example line each, shown above the per-column table so
#' users see how to answer without leaving the page.
#'
#' @keywords internal
#' @noRd
disclosure_help_ui <- function(attested = FALSE) {
  identifies_meta <- dg_identifies_option_meta()
  identifies_meta <- identifies_meta[vapply(
    identifies_meta,
    function(meta) meta$value %in% q1_identifies_choices(attested),
    logical(1)
  )]
  q1_options <- lapply(identifies_meta, function(meta) {
    shiny::tags$div(
      class = "dq-opt",
      shiny::tags$span(class = "dq-opt-label", meta$label),
      shiny::tags$span(class = "dq-opt-ex", paste0(" \u2014 ", meta$examples))
    )
  })

  shiny::tags$div(
    class = "disclosure-help",
    shiny::tags$div(
      class = "dq-lead",
      if (isTRUE(attested)) {
        "You've confirmed there are no direct identifiers. Two risks remain for each column:"
      } else {
        "Classify every column by answering two questions."
      }
    ),
    shiny::tags$div(
      class = "dq",
      shiny::tags$div(
        class = "dq-eyebrow",
        "Question 1 \u00b7 the \u201cPoints to a person?\u201d column"
      ),
      shiny::tags$p(
        class = "dq-q",
        if (isTRUE(attested)) {
          "Could this column, combined with others, help single out a person?"
        } else {
          "Could a value point to a specific person \u2014 on its own, or combined with other columns?"
        }
      ),
      shiny::tags$div(class = "dq-opts", q1_options)
    ),
    shiny::tags$div(
      class = "dq",
      shiny::tags$div(
        class = "dq-eyebrow",
        "Question 2 \u00b7 the \u201cSensitive?\u201d column"
      ),
      shiny::tags$p(
        class = "dq-q",
        if (isTRUE(attested)) {
          "Is this column sensitive \u2014 would it be considered private or intrusive if linked to a person?"
        } else {
          "Would it be considered private or intrusive if this value were linked back to someone?"
        }
      ),
      shiny::tags$p(
        class = "dq-ex",
        "Examples: diagnosis, income, religion, mental health, immigration."
      )
    )
  )
}

#' Reference card: what each action does, and which types offer it
#'
#' Keyed by *action* rather than by type. A user scanning the Action dropdown
#' is asking "what does this choice do to my data", so the action is the
#' entry point and the types it applies to are the qualifier -- the reverse
#' of the type-first legend this replaces, which forced the reader to find
#' their type before learning the consequence.
#'
#' @keywords internal
#' @noRd
type_action_legend_ui <- function() {
  row <- function(action, types, detail, danger = FALSE) {
    shiny::tags$tr(
      shiny::tags$td(
        # No white-space:nowrap here: the longest action label carries a
        # parenthetical qualifier that is broken onto its own line below, and
        # nowrap would force the column wide enough to defeat that.
        style = paste0(
          "font-family:var(--font-sans); font-weight:600; font-size:12px; padding:4px 8px;",
          if (danger) " color:#b7791f;" else ""
        ),
        action
      ),
      shiny::tags$td(style = "font-family:var(--font-mono); font-size:12px; padding:4px 8px;", types),
      shiny::tags$td(style = "font-family:var(--font-sans); font-size:12px; color:var(--fg-muted); padding:4px 8px;", detail)
    )
  }
  shiny::tags$div(
    class = "card",
    style = "margin-bottom:12px;",
    shiny::tags$div(
      class = "card-header",
      shiny::tags$span(class = "title", "What each action does"),
      shiny::tags$span(class = "sub", "the Action dropdown offers only the actions that fit a column's type")
    ),
    shiny::tags$table(
      style = "width:100%; border-collapse:collapse;",
      row("Resample", "categorical, free text, postal code",
          shiny::tagList(
            "Draws from the values observed in your data; rare ones are grouped first. ",
            shiny::tags$span(
              style = "color:#b7791f;",
              "Some levels may not survive -- grouped away, or crowded out."
            ),
            " Postal codes are reused as-is."
          )),
      # The qualifier is broken onto its own line so the action column stays
      # narrow. The Action dropdown keeps the single-line label: an <option>
      # cannot contain a line break, so the two cannot be shared.
      row(shiny::tagList("Resample", shiny::tags$br(), "(rare levels masked)"),
          "categorical, free text",
          "Every level survives at its observed frequency; only rare labels are swapped for a neutral placeholder. Nothing is grouped, nothing invented."),
      row("Simulate", "numeric, date",
          "Recreated within the observed distribution and range, with noise or coarsening."),
      row("Scramble", "alpha-numeric ID, free text",
          "Characters are reordered within each value, keeping length and punctuation in place. Nothing is carried across rows."),
      row("Generate new", "postal code",
          "Fresh format-valid values in the detected country format; no source value is reused."),
      row("Pass through", "any type",
          "The real values are copied across unchanged. Verify before sharing.", danger = TRUE),
      row("Drop", "any type",
          "The column is removed from the synthetic data entirely.")
    )
  )
}

#' @keywords internal
#' @noRd
mod_roles_server <- function(id, state) {
  rlang::check_installed(
    c("shiny", "DT"),
    reason = "to use the DataGangeR Shiny modules"
  )

  shiny::moduleServer(id, function(input, output, session) {
    roles_local <- shiny::reactiveVal(NULL)
    role_filter <- shiny::reactiveVal("all")
    row_map     <- shiny::reactiveVal(integer(0))
    # Bulk-configure selection, keyed by variable name (not row index) so it
    # survives filtering/search -- selecting a few columns, filtering to a
    # different subset, and selecting more before applying one bulk edit is
    # a deliberate workflow this supports.
    selected_vars <- shiny::reactiveVal(character(0))

    ensure_simulation_column <- function(roles) {
      dg_ensure_ui_roles(roles)
    }

    normalize_edit_info <- function(info) {
      if (is.null(info)) {
        return(NULL)
      }
      if (is.data.frame(info)) {
        info <- info[1, , drop = FALSE]
      }
      list(
        row   = as.integer(info$row[[1]]),
        col   = as.integer(info$col[[1]]),
        value = info$value[[1]]
      )
    }

    shiny::observe({
      shiny::req(state$roles)
      roles_local(ensure_simulation_column(state$roles))
    })

    output$disclosure_help <- shiny::renderUI({
      disclosure_help_ui(isTRUE(state$attested_no_direct))
    })

    visible_roles <- shiny::reactive({
      roles <- roles_local()
      shiny::req(roles)

      rf <- role_filter()
      nf <- tolower(trimws(input$col_search_val %||% ""))

      idx <- seq_len(nrow(roles))
      if (!identical(rf, "all")) {
        idx <- idx[vapply(idx, function(i) {
          identical(
            eff_role(roles$user_role[[i]], roles$recommended_role[[i]], roles$class[[i]]),
            rf
          )
        }, logical(1))]
      }
      if (nchar(nf) > 0) {
        idx <- idx[grepl(nf, tolower(roles$variable[idx]), fixed = TRUE)]
      }
      list(data = roles[idx, , drop = FALSE], map = idx)
    })

    output$roles_banner_text <- shiny::renderUI({
      roles <- roles_local()
      if (is.null(roles)) {
        return(shiny::tags$div(shiny::tags$b("Auto-detected."), " Edit anything that's wrong."))
      }
      changed <- sum(!is.na(roles$user_role) & nzchar(roles$user_role))
      shiny::tags$div(
        shiny::tags$b("Auto-detected. Edit anything that's wrong."),
        if (changed > 0L) sprintf(" \u00b7 %d manually adjusted.", changed)
      )
    })

    output$agg_warning <- shiny::renderUI({
      data <- state$raw_data
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
        return(NULL)
      }
      agg <- looks_aggregated(data)
      if (!isTRUE(agg$aggregated)) {
        return(NULL)
      }
      shiny::tags$div(
        class = "banner risk",
        shiny::tags$span(class = "icon", "!"),
        shiny::tags$div(
          shiny::tags$b("This looks like aggregated data, not individual records."),
          shiny::tags$span(
            sprintf(" (%s)", agg$reason)
          ),
          shiny::tags$div(
            style = "font-size:12px; margin-top:4px;",
            "Disclosure control assumes individual-level microdata. On a counts table, ",
            "the k-anonymity guarantee below applies to the dimension columns, not to the ",
            "counts; review small cells directly before sharing."
          )
        )
      )
    })

    output$roles_card_header <- shiny::renderUI({
      roles   <- roles_local()
      vr      <- visible_roles()
      total   <- if (!is.null(roles)) nrow(roles) else 0L
      visible <- if (!is.null(vr)) nrow(vr$data) else 0L
      sub_lbl <- if (visible < total) paste0(visible, " shown") else "all shown"
      shiny::tagList(
        shiny::tags$span(class = "title", paste0("Column roles \u00b7 ", total)),
        shiny::tags$span(class = "sub",   sub_lbl)
      )
    })

    output$filter_chips <- shiny::renderUI({
      roles <- roles_local()
      shiny::req(roles)

      all_roles <- c("alphanumeric_id", "numeric", "categorical",
                     "date", "postal_code", "free_text")
      eff_roles <- vapply(seq_len(nrow(roles)), function(i) {
        eff_role(roles$user_role[[i]], roles$recommended_role[[i]], roles$class[[i]])
      }, character(1))
      counts  <- table(eff_roles)
      present <- all_roles[all_roles %in% names(counts)]
      current <- role_filter()

      make_chip <- function(label, value, count) {
        is_active <- identical(current, value)
        shiny::tags$button(
          class   = paste0("btn btn-sm", if (is_active) " btn-primary" else " btn-secondary"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            session$ns("role_filter_val"),
            value
          ),
          label,
          shiny::tags$span(
            style = "font-family:var(--font-mono); font-size:11px; opacity:0.7; margin-left:4px;",
            as.character(count)
          )
        )
      }

      chips <- list(make_chip("all", "all", nrow(roles)))
      for (r in present) {
        chips <- c(chips, list(make_chip(ROLE_LABELS[[r]] %||% r, r, as.integer(counts[r]))))
      }
      shiny::tagList(chips)
    })

    shiny::observeEvent(input$role_filter_val, ignoreNULL = TRUE, {
      role_filter(input$role_filter_val)
    })

    # There is no separate "pseudo identifier" type any more -- any column
    # that looks like an identifier, structured or not, is an alphanumeric
    # ID, whose default action is scramble rather than drop. "free_text" is
    # kept unchanged so every existing comparison/dispatch keyed on it keeps
    # working; only its displayed label differs. Logical is no longer a
    # distinct role -- it is folded into categorical (see
    # dg_rec_to_role/dg_class_to_role). "drop" is a data *treatment*, not a
    # data type -- it lives only in SIMULATION_OPTIONS (Action override) now,
    # not in the type dropdown.
    ROLE_OPTIONS <- c("alphanumeric_id", "numeric", "categorical",
                      "date", "postal_code", "free_text")
    ROLE_LABELS <- c(
      alphanumeric_id = "alpha-numeric ID",
      numeric         = "numeric",
      categorical     = "categorical",
      date            = "date",
      postal_code     = "postal code",
      free_text       = "free text"
    )
    SIMULATION_OPTIONS <- c("synthesize", "pass_through", "scramble", "drop")

    # Role-mapping helpers (rec_to_role/class_to_role/eff_role) are defined at
    # file scope so the Generate page's read-only decision table can reuse them.
    rec_to_role   <- dg_rec_to_role
    class_to_role <- dg_class_to_role

    # ---- Per-row mutation logic, shared by the single-column dropdowns and
    # the bulk-configure toolbar below, so both apply exactly the same rules
    # (Q1 reset on a type change away from an identifying type, alphanumeric
    # ID defaulting to scramble, etc.) instead of two copies drifting apart.
    # Each takes/returns the whole `roles` object rather than mutating in
    # place, so a bulk apply can fold a loop of these over multiple rows
    # before writing back to state once.
    apply_type_change <- function(roles, orig_row, val) {
      if (!val %in% ROLE_OPTIONS) return(roles)
      roles$user_role[[orig_row]] <- val
      if (val %in% c("free_text", "alphanumeric_id")) {
        roles$identifies[[orig_row]] <- "direct"
      } else if (identical(roles$identifies[[orig_row]], "direct")) {
        roles$identifies[[orig_row]]      <- NA_character_
        roles$user_identifies[[orig_row]] <- NA_character_
      }
      roles <- dg_sync_roles_axes(roles)
      roles$simulation[[orig_row]] <- if (identical(val, "alphanumeric_id")) {
        "scramble"
      } else {
        dg_derived_action_axes(roles$identifies[[orig_row]], roles$sensitive[[orig_row]])
      }
      # A sticky action from the previous type may be meaningless for the new
      # one -- scramble chosen on free text, then retyped to numeric. Leaving
      # it set would let a later Q1 answer restore an action this type never
      # offers, so drop the override and fall back to the type's default.
      if (!roles$simulation[[orig_row]] %in% dg_action_options(val)) {
        roles$simulation[[orig_row]] <- dg_action_options(val)[[1]]
      }
      if ("user_simulation" %in% names(roles)) {
        stale <- roles$user_simulation[[orig_row]]
        if (!is.na(stale) && !stale %in% dg_action_options(val)) {
          roles$user_simulation[[orig_row]] <- NA_character_
        }
      }
      if (identical(val, "postal_code")) {
        if (!"postal_strategy" %in% names(roles)) roles$postal_strategy <- NA_character_
        if (!"postal_country" %in% names(roles)) roles$postal_country <- NA_character_
        roles$postal_strategy[[orig_row]] <- "generate"
        roles$postal_country[[orig_row]] <- NA_character_
      } else {
        if ("postal_strategy" %in% names(roles)) roles$postal_strategy[[orig_row]] <- NA_character_
        if ("postal_country" %in% names(roles)) roles$postal_country[[orig_row]] <- NA_character_
      }
      if (val %in% c("categorical", "free_text")) {
        if (!"label_strategy" %in% names(roles)) roles$label_strategy <- NA_character_
        if (is.na(roles$label_strategy[[orig_row]]) || !nzchar(roles$label_strategy[[orig_row]])) {
          roles$label_strategy[[orig_row]] <- "preserve"
        }
      } else if ("label_strategy" %in% names(roles)) {
        roles$label_strategy[[orig_row]] <- NA_character_
      }
      roles
    }

    apply_identifies_change <- function(roles, orig_row, val, attested) {
      if (!val %in% q1_identifies_choices(attested)) return(roles)
      roles$user_identifies[[orig_row]] <- val
      roles$identifies[[orig_row]]      <- val
      roles <- dg_sync_roles_axes(roles)
      # Answering Q1 re-derives the action, because Q1 *is* the drop decision:
      # choosing "direct" means drop. But an explicit action the user picked
      # themselves wins -- re-deriving over it is what silently discarded
      # keep-decisions before.
      chosen <- if ("user_simulation" %in% names(roles)) {
        roles$user_simulation[[orig_row]]
      } else {
        NA_character_
      }
      if (is.na(chosen) || !nzchar(chosen)) {
        roles$simulation[[orig_row]] <- dg_derived_action_axes(
          roles$identifies[[orig_row]], roles$sensitive[[orig_row]]
        )
      }
      roles
    }

    apply_sensitive_change <- function(roles, orig_row, val) {
      if (!val %in% c("yes", "no")) return(roles)
      val_bool <- identical(val, "yes")
      roles$user_sensitive[[orig_row]] <- val_bool
      roles$sensitive[[orig_row]]      <- val_bool
      # Sensitivity does not decide whether a column is kept or dropped, so it
      # must not touch `simulation`. It used to re-derive the action here, which
      # silently converted an explicit keep (pass_through/scramble) into "drop"
      # for any column seeded as a direct identifier -- and answering Q2 is
      # mandatory, so that fired for effectively every such column.
      dg_sync_roles_axes(roles)
    }

    apply_simulation_change <- function(roles, orig_row, val) {
      role <- eff_role(
        roles$user_role[[orig_row]],
        roles$recommended_role[[orig_row]],
        roles$class[[orig_row]]
      )
      dg_apply_action_token(roles, orig_row, val, role)
    }

    is_whole_number_column <- function(x) {
      if (is.integer(x)) {
        return(TRUE)
      }
      if (!is.numeric(x)) {
        return(FALSE)
      }
      x_finite <- x[!is.na(x) & is.finite(x)]
      if (!length(x_finite)) {
        return(FALSE)
      }
      all(x_finite == round(x_finite))
    }

    storage_signal_for <- function(variable, class_col) {
      data <- state$raw_data
      if (!is.null(data) && variable %in% names(data)) {
        x <- data[[variable]]
        if (is_whole_number_column(x)) {
          return("stored as integer")
        }
        if (is.numeric(x)) {
          return("stored as decimal/numeric")
        }
      }

      lc <- tolower(class_col %||% "")
      if (grepl("integer", lc)) {
        return("stored as integer")
      }
      if (grepl("numeric|double", lc)) {
        return("stored as decimal/numeric")
      }
      if (nzchar(class_col %||% "")) {
        return(sprintf("stored as %s", class_col))
      }
      "stored in an unknown class"
    }

    output$roles_table <- shiny::renderUI({
      vr <- visible_roles()
      shiny::req(vr)
      roles <- vr$data
      map   <- vr$map
      row_map(map)

      if (nrow(roles) == 0L) {
        return(shiny::tags$p(
          style = "text-align:center; color:var(--fg-subtle); padding:20px 0; font-family:var(--font-sans);",
          "No matches."
        ))
      }

      make_select <- function(orig_row, user_role, recommended_role, class_col, disabled = FALSE) {
        effective  <- eff_role(user_role, recommended_role, class_col)
        overridden <- !is.na(user_role) && nzchar(user_role)
        recommended_option <- rec_to_role(recommended_role)
        needs_review <- !overridden &&
          !is.na(recommended_role) && nzchar(recommended_role) &&
          !identical(tolower(effective %||% ""), tolower(class_to_role(class_col) %||% ""))
        opts <- lapply(ROLE_OPTIONS, function(opt) {
          opt_label <- ROLE_LABELS[[opt]] %||% opt
          shiny::tags$option(
            value    = opt,
            selected = if (identical(opt, effective)) "selected" else NULL,
            if (!is.na(recommended_option) && identical(opt, recommended_option)) {
              paste0(opt_label, " (recommended)")
            } else {
              opt_label
            }
          )
        })
        sel <- shiny::tags$select(
          class    = paste("input", if (disabled) "dg-disabled-select"),
          style    = sprintf(
            "width:100%%; padding:2px 6px; font-size:11px; font-family:var(--font-mono); border-radius:2px; %s%s",
            if (overridden) "background:var(--synth-50); border-color:var(--synth-300);" else "",
            if (disabled) " opacity:0.5; cursor:not-allowed;" else ""
          ),
          disabled = if (disabled) "disabled" else NULL,
          onchange = sprintf(
            "Shiny.setInputValue('%s', {row: %d, value: this.value}, {priority:'event'})",
            session$ns("role_change"),
            orig_row
          ),
          opts
        )
        shiny::tags$div(
          class = paste(
            "role-select-wrap",
            if (overridden) "is-overridden",
            if (needs_review) "needs-review"
          ),
          sel
        )
      }

      make_simulation_select <- function(orig_row, simulation, role,
                                         postal_strategy = NA_character_,
                                         label_strategy = NA_character_) {
        allowed <- dg_action_options(role)
        current <- action_token(simulation, role, postal_strategy, label_strategy)
        if (!current %in% allowed) {
          current <- allowed[[1]]
        }
        opts <- lapply(allowed, function(opt) {
          shiny::tags$option(
            value    = opt,
            selected = if (identical(opt, current)) "selected" else NULL,
            dg_action_label(opt, role)
          )
        })
        shiny::tags$select(
          class = "input",
          style = "width:100%; padding:2px 6px; font-size:11px; font-family:var(--font-mono); border-radius:2px;",
          onchange = sprintf(
            "Shiny.setInputValue('%s', {row: %d, value: this.value}, {priority:'event'})",
            session$ns("simulation_change"),
            orig_row
          ),
          opts
        )
      }

      make_identifies_select <- function(orig_row, current, ns) {
        is_unset <- is.na(current) || !nzchar(current)
        allowed_values <- q1_identifies_choices(isTRUE(state$attested_no_direct))
        placeholder <- shiny::tags$option(
          value = "", disabled = "disabled",
          selected = if (is_unset) "selected" else NULL,
          "Select answer..."
        )
        opts <- lapply(Filter(
          function(meta) meta$value %in% allowed_values,
          dg_identifies_option_meta()
        ), function(meta) {
          shiny::tags$option(
            value = meta$value,
            selected = if (!is_unset && meta$value == current) "selected" else NULL,
            meta$label
          )
        })
        shiny::tags$select(
          onchange = sprintf(
            "Shiny.setInputValue('%s', {row: %d, value: this.value}, {priority:'event'})",
            ns("identifies_change"),
            orig_row
          ),
          style = sprintf(
            "font-family:var(--font-mono); font-size:11px; padding:3px 6px; width:100%%; %s",
            if (is_unset) "border-color:#e53e3e; background:#fff5f5; color:#c53030;" else ""
          ),
          c(list(placeholder), opts)
        )
      }

      make_sensitive_select <- function(orig_row, current, ns) {
        is_unset    <- is.na(current)
        current_yes <- !is_unset && isTRUE(isTRUE_vec(current))
        shiny::tags$select(
          onchange = sprintf(
            "Shiny.setInputValue('%s', {row: %d, value: this.value}, {priority:'event'})",
            ns("sensitive_change"),
            orig_row
          ),
          style = sprintf(
            "font-family:var(--font-mono); font-size:11px; padding:3px 6px; width:100%%; %s",
            if (is_unset) "border-color:#e53e3e; background:#fff5f5; color:#c53030;" else ""
          ),
          shiny::tags$option(
            value = "", disabled = "disabled",
            selected = if (is_unset) "selected" else NULL,
            "Select answer..."
          ),
          shiny::tags$option(value = "no",  selected = if (!is_unset && !current_yes) "selected" else NULL, "No"),
          shiny::tags$option(value = "yes", selected = if (!is_unset &&  current_yes) "selected" else NULL, "Yes")
        )
      }

      make_postal_country_select <- function(orig_row, current) {
        countries <- c(NA, "CA", "US", "UK", "AU", "DE", "FR", "JP", "IN", "BR", "NL")
        country_labels <- c(
          "Auto-detect", "Canada", "United States", "United Kingdom",
          "Australia", "Germany", "France", "Japan", "India", "Brazil",
          "Netherlands"
        )
        if (is.na(current)) current <- ""
        shiny::tags$select(
          class = "input",
          style = "width:100%; padding:2px 6px; font-size:11px; font-family:var(--font-mono); border-radius:2px;",
          onchange = sprintf(
            "Shiny.setInputValue('%s', {row: %d, value: this.value}, {priority:'event'})",
            session$ns("postal_country_change"),
            orig_row
          ),
          lapply(seq_along(countries), function(idx) {
            val <- if (is.na(countries[idx])) "" else countries[idx]
            shiny::tags$option(
              value = val,
              selected = if (identical(val, current)) "selected" else NULL,
              country_labels[idx]
            )
          })
        )
      }

      # Explains what a type override actually does once it takes effect --
      # shown only when moving *away* from an identifying recommended role
      # (alphanumeric ID / free text) to a plain type, since that is the
      # override whose consequence (Q1 reset, inclusion in synthesis and
      # Compare) is easy to miss. If the underlying column still has more
      # distinct values than the Compare page's dynamic cap, that is called
      # out too, since the override alone will not make it comparable.
      override_consequence_caption <- function(recommended_role, user_role, col_data, n_rows) {
        rec <- tolower(recommended_role %||% "")
        usr <- tolower(user_role %||% "")
        was_identifying <- grepl("alphanumeric", rec) || grepl("free.text|free_text", rec)
        now_plain <- usr %in% c("categorical", "numeric", "date")
        if (!(was_identifying && now_plain)) {
          return(NULL)
        }
        n_rows <- n_rows %||% length(col_data)
        n_distinct <- if (!is.null(col_data)) length(unique(col_data[!is.na(col_data)])) else NA_integer_
        cap <- dg_max_comparable_levels(n_rows)
        over_cap <- !is.na(n_distinct) && n_distinct > cap
        text <- if (over_cap) {
          sprintf(
            paste(
              "Now treated as ordinary data, but %s distinct values is above the",
              "%d-value Compare limit for %s rows -- it will still be hidden there",
              "and may synthesize as many rare categories. Q1 was reset; confirm it",
              "again before generating."
            ),
            format(n_distinct, big.mark = ","), cap, format(n_rows, big.mark = ",")
          )
        } else {
          "Now treated as ordinary data: synthesized and shown on Compare. Q1 was reset -- confirm it again before generating."
        }
        list(text = text, warn = over_cap)
      }

      make_action_override_controls <- function(orig_row, row_data, col_data, n_rows) {
        simulation_value <- as.character(row_data$simulation[[1]] %||% "synthesize")
        effective_role <- eff_role(
          row_data$user_role[[1]], row_data$recommended_role[[1]], row_data$class[[1]]
        )
        caption <- override_consequence_caption(
          row_data$recommended_role[[1]], row_data$user_role[[1]], col_data, n_rows
        )
        shiny::tags$div(
          style = "display:grid; gap:6px;",
          shiny::tags$div(
            shiny::tags$div(
              style = "font-size:11px; color:var(--fg-muted);",
              "Action override"
            ),
            make_simulation_select(
              orig_row, simulation_value, effective_role,
              if ("postal_strategy" %in% names(row_data)) row_data$postal_strategy[[1]] else NA_character_,
              if ("label_strategy" %in% names(row_data)) row_data$label_strategy[[1]] else NA_character_
            )
          ),
          shiny::tags$div(
            shiny::tags$div(
              style = "font-size:11px; color:var(--fg-muted);",
              "Data type override"
            ),
            make_select(
              orig_row,
              row_data$user_role[[1]],
              row_data$recommended_role[[1]],
              row_data$class[[1]]
            )
          ),
          # Postal strategy is no longer its own dropdown -- "Generate new" and
          # "Resample observed" are actions, so they belong in the Action
          # select with every other action. Country format stays separate
          # because it is a parameter of the action, not an alternative to it.
          if (identical(effective_role, "postal_code")) {
            shiny::tags$div(
              shiny::tags$div(
                style = "font-size:11px; color:var(--fg-muted);",
                "Country format"
              ),
              make_postal_country_select(
                orig_row,
                if ("postal_country" %in% names(row_data)) row_data$postal_country[[1]] else NA_character_
              )
            )
          },
          if (!is.null(caption)) {
            shiny::tags$div(
              style = if (isTRUE(caption$warn)) {
                "font-size:11px; color:#b7791f; background:#fffbea; border:1px solid #f6e05e; border-radius:3px; padding:4px 6px;"
              } else {
                "font-size:11px; color:var(--fg-muted); background:var(--bg-subtle); border-radius:3px; padding:4px 6px;"
              },
              caption$text
            )
          }
        )
      }

      raw_data <- state$raw_data
      sel <- selected_vars()
      rows <- lapply(seq_len(nrow(roles)), function(i) {
        orig_row <- map[[i]]
        r <- roles[i, , drop = FALSE]
        tooltip <- paste(
          r$reason[[1]],
          storage_signal_for(r$variable[[1]], r$class[[1]]),
          sep = "\n"
        )
        col_data <- if (!is.null(raw_data) && r$variable[[1]] %in% names(raw_data)) {
          raw_data[[r$variable[[1]]]]
        } else {
          NULL
        }
        shiny::tags$tr(
          shiny::tags$td(
            style = "padding:6px 4px; text-align:center;",
            shiny::tags$input(
              type = "checkbox",
              checked = if (r$variable[[1]] %in% sel) "checked" else NULL,
              onclick = sprintf(
                "Shiny.setInputValue('%s', {variable: '%s', checked: this.checked}, {priority:'event'})",
                session$ns("row_select"),
                gsub("'", "\\\\'", r$variable[[1]])
              )
            )
          ),
          shiny::tags$td(
            style = "font-family:var(--font-mono); font-size:12px; padding:6px 8px;",
            shiny::tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              shiny::tags$span(r$variable[[1]]),
              shiny::tags$span(
                class = "role-info",
                title = tooltip,
                "(i)"
              )
            )
          ),
          # Q1 and Q2 share one cell, stacked vertically, so the disclosure
          # questions cost one column of horizontal space instead of two.
          shiny::tags$td(
            style = "min-width:260px; padding:4px 8px;",
            shiny::tags$div(
              style = "display:flex; flex-direction:column; gap:4px;",
              make_identifies_select(orig_row, r$user_identifies[[1]], session$ns),
              make_sensitive_select(orig_row, r$user_sensitive[[1]], session$ns)
            )
          ),
          shiny::tags$td(
            style = "min-width:320px; padding:6px 8px;",
            make_action_override_controls(orig_row, r, col_data, nrow(raw_data))
          )
        )
      })

      all_visible_selected <- nrow(roles) > 0L && all(roles$variable %in% sel)

      shiny::tags$table(
        class = "data compact",
        style = "width:100%; border-collapse:collapse;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th(
              style = "width:28px; padding:6px 4px; text-align:center;",
              shiny::tags$input(
                type = "checkbox",
                title = "Select all shown",
                checked = if (all_visible_selected) "checked" else NULL,
                onclick = sprintf(
                  "Shiny.setInputValue('%s', this.checked, {priority:'event'})",
                  session$ns("select_all_visible")
                )
              )
            ),
            shiny::tags$th(style = "width:24%; padding:6px 8px;", "Column"),
            shiny::tags$th(
              style = "width:32%; padding:6px 8px;",
              "Points to a person? (Q1)",
              shiny::tags$br(),
              shiny::tags$span(
                style = "font-weight:normal; color:var(--fg-muted);",
                "Sensitive? (Q2)"
              )
            ),
            shiny::tags$th(style = "width:44%; padding:6px 8px;", "Action override")
          )
        ),
        shiny::tags$tbody(rows)
      )
    })

    output$kanon_readout <- shiny::renderUI({
      roles <- roles_local()
      data  <- state$raw_data
      if (is.null(roles) || is.null(data) || !"disclosure_role" %in% names(roles)) {
        return(NULL)
      }
      # k is owned by the Generate advanced slider now; the spec is the more
      # authoritative of the two once one has been confirmed.
      k <- state$spec$k_anon %||% state$k_anon %||% 5
      qi <- intersect(dg_kanon_columns(roles), names(data))
      direct <- intersect(roles$variable[roles$disclosure_role %in% "direct"], names(data))

      if (length(qi) == 0L) {
        return(shiny::tags$div(
          class = "card",
          style = "margin-top:12px;",
          shiny::tags$strong("No quasi-identifiers selected."),
          " Mark the columns that could identify someone in combination."
        ))
      }
      res <- assess_kanonymity(data, qi, k = k)
      safe <- is.na(res$smallest_cell) || res$n_below == 0L

      worst_lines <- if (nrow(res$worst_cells) > 0L) {
        apply(utils::head(res$worst_cells, 3L), 1L, function(row) {
          vals <- paste(row[qi], collapse = " \u00b7 ")
          sprintf("%s \u2192 %s record(s)", vals, row[["n"]])
        })
      } else character(0)

      shiny::tags$div(
        class = "card",
        style = "margin-top:12px;",
        shiny::tags$div(
          style = "font-family:var(--font-mono); font-size:12px; color:var(--fg-muted);",
          shiny::tagList(
            dg_privacy_term("Quasi-identifier (QI)", "qi"),
            " columns: ",
            paste(qi, collapse = " \u00b7 "),
            "   ",
            dg_privacy_term("k", "k"),
            " = ",
            k
          )
        ),
        if (safe) {
          shiny::tags$div(
            style = "color:var(--real-700);",
            "\u2713 No record sits in an unsafe combination at this k."
          )
        } else {
          shiny::tagList(
            shiny::tags$div(
              style = "color:var(--synth-700); font-weight:600;",
              shiny::tagList(
                "\u26a0 Smallest ",
                dg_privacy_term("cell", "cell"),
                ": ",
                sprintf(
                  "%d record(s). %d of %d records (%.1f%%) fall below ",
                  res$smallest_cell, res$n_below, nrow(data), res$pct_below
                ),
                dg_privacy_term("k", "k"),
                "."
              )
            ),
            shiny::tags$ul(lapply(worst_lines, shiny::tags$li))
          )
        },
        # Report what is *actually* dropped, from the effective action -- not
        # from disclosure_role. A direct identifier the user chose to keep
        # (pass_through/scramble) stays in the output, and saying otherwise
        # here would misreport the disclosure.
        local({
          treatment <- dg_role_treatment(roles)
          dropped <- intersect(names(treatment)[treatment %in% "drop"], names(data))
          if (!length(dropped)) return(NULL)
          shiny::tags$div(
            class = "banner risk",
            style = "margin-top:6px; font-size:12px;",
            shiny::tags$strong(sprintf(
              "\u26a0 %d column(s) will be removed from the output: ",
              length(dropped)
            )),
            paste(dropped, collapse = ", "),
            shiny::tags$div(
              style = "font-weight:normal; margin-top:2px;",
              "Set Action override to keep a column instead."
            )
          )
        })
      )
    })

    output$disclosure_gate <- shiny::renderUI({
      roles <- roles_local()
      if (is.null(roles) || !"identifies" %in% names(roles)) return(NULL)
      unset <- length(roles_generation_pending(roles))
      if (unset == 0L) {
        shiny::tags$div(
          class = "banner", style = "margin-top:12px; color:var(--real-700);",
          "\u2713 All columns have answers. Ready to continue."
        )
      } else {
          shiny::tags$div(
            class = "banner risk", style = "margin-top:12px;",
            shiny::tags$span(class = "icon", "!"),
            sprintf(
            "%d column%s still need an answer before you can generate.",
            unset, if (unset == 1L) "" else "s"
          )
        )
      }
    })

    # The type dropdown doubles as a privacy signal for the options that
    # always mean "this points to a person": choosing "free_text" or
    # "alphanumeric_id" only ever strengthens protection, so it is safe to
    # apply immediately (see apply_type_change() for what moving *away* from
    # an identifying type does to a previously-confirmed Q1 answer).
    shiny::observeEvent(input$role_change, ignoreNULL = TRUE, {
      change <- input$role_change
      roles  <- roles_local()
      if (is.null(change) || is.null(roles)) return(invisible(NULL))
      orig_row <- as.integer(change$row)
      if (is.na(orig_row) || orig_row < 1L || orig_row > nrow(roles)) {
        return(invisible(NULL))
      }
      roles <- apply_type_change(roles, orig_row, as.character(change$value))
      roles_local(roles)
      state$roles <- roles
      invisible(NULL)
    })

    shiny::observeEvent(input$identifies_change, ignoreNULL = TRUE, {
      change <- input$identifies_change
      roles  <- roles_local()
      if (is.null(change) || is.null(roles)) return(invisible(NULL))
      orig_row <- as.integer(change$row)
      if (is.na(orig_row) || orig_row < 1L || orig_row > nrow(roles)) return(invisible(NULL))
      roles <- apply_identifies_change(
        roles, orig_row, as.character(change$value), isTRUE(state$attested_no_direct)
      )
      roles_local(roles)
      state$roles <- roles
      invisible(NULL)
    })

    shiny::observeEvent(input$sensitive_change, ignoreNULL = TRUE, {
      change <- input$sensitive_change
      roles  <- roles_local()
      if (is.null(change) || is.null(roles)) return(invisible(NULL))
      orig_row <- as.integer(change$row)
      if (is.na(orig_row) || orig_row < 1L || orig_row > nrow(roles)) return(invisible(NULL))
      roles <- apply_sensitive_change(roles, orig_row, as.character(change$value))
      roles_local(roles)
      state$roles <- roles
      invisible(NULL)
    })

    shiny::observeEvent(input$simulation_change, ignoreNULL = TRUE, {
      change <- input$simulation_change
      roles  <- roles_local()
      if (is.null(change) || is.null(roles)) return(invisible(NULL))
      orig_row <- as.integer(change$row)
      if (is.na(orig_row) || orig_row < 1L || orig_row > nrow(roles)) return(invisible(NULL))
      roles <- ensure_simulation_column(roles)
      roles <- apply_simulation_change(roles, orig_row, as.character(change$value))
      roles_local(roles)
      state$roles <- roles
      invisible(NULL)
    })

    shiny::observeEvent(input$postal_country_change, ignoreNULL = TRUE, {
      change <- input$postal_country_change
      roles  <- roles_local()
      if (is.null(change) || is.null(roles)) return(invisible(NULL))
      orig_row <- as.integer(change$row)
      if (is.na(orig_row) || orig_row < 1L || orig_row > nrow(roles)) return(invisible(NULL))
      val <- as.character(change$value)
      if (!"postal_country" %in% names(roles)) roles$postal_country <- NA_character_
      roles$postal_country[[orig_row]] <- if (nzchar(val)) val else NA_character_
      roles_local(roles)
      state$roles <- roles
      invisible(NULL)
    })

    # ---- Bulk configure ----
    # Row-selection state and the toolbar that applies one of the four edits
    # above to every selected row at once, for the common "10 columns, most
    # of them the same answer" case.

    output$bulk_toolbar <- shiny::renderUI({
      roles <- roles_local()
      shiny::req(roles)
      n_sel <- length(intersect(selected_vars(), roles$variable))

      if (n_sel == 0L) {
        return(shiny::tags$div(
          class = "card",
          style = "margin-bottom:10px; padding:8px 12px; background:var(--bg-subtle);",
          shiny::tags$span(
            style = "font-family:var(--font-sans); font-size:12px; color:var(--fg-muted);",
            "Check columns below to bulk-edit several at once."
          )
        ))
      }

      attested <- isTRUE(state$attested_no_direct)
      q1_opts  <- q1_identifies_choices(attested)
      q1_labels <- stats::setNames(
        vapply(dg_identifies_option_meta(), function(m) m$label, character(1)),
        vapply(dg_identifies_option_meta(), function(m) m$value, character(1))
      )

      bulk_row <- function(label, select_id, apply_id, options, option_labels = NULL) {
        shiny::tags$div(
          style = "display:flex; align-items:center; gap:8px; margin-top:6px;",
          shiny::tags$span(
            style = "font-family:var(--font-mono); font-size:11px; color:var(--fg-muted); width:150px; flex:none;",
            label
          ),
          shiny::tags$select(
            id = session$ns(select_id),
            class = "input",
            style = "width:200px; padding:2px 6px; font-size:11px; font-family:var(--font-mono);",
            lapply(options, function(opt) {
              shiny::tags$option(value = opt, option_labels[[opt]] %||% opt)
            })
          ),
          shiny::actionButton(
            session$ns(apply_id),
            sprintf("Apply to %d selected", n_sel),
            class = "btn btn-sm btn-secondary"
          )
        )
      }

      shiny::tags$div(
        class = "card",
        style = "margin-bottom:10px; padding:10px 12px; background:var(--bg-subtle);",
        shiny::tags$div(
          style = "display:flex; align-items:center; justify-content:space-between;",
          shiny::tags$strong(
            style = "font-family:var(--font-sans); font-size:12px;",
            sprintf("%d column%s selected", n_sel, if (n_sel == 1L) "" else "s")
          ),
          shiny::actionButton(
            session$ns("bulk_clear"), "Clear selection",
            class = "btn btn-sm btn-secondary"
          )
        ),
        bulk_row("Type", "bulk_type_value", "bulk_apply_type", ROLE_OPTIONS, ROLE_LABELS),
        bulk_row("Points to a person? (Q1)", "bulk_identifies_value", "bulk_apply_identifies",
                 q1_opts, q1_labels),
        bulk_row("Sensitive? (Q2)", "bulk_sensitive_value", "bulk_apply_sensitive",
                 c("no", "yes"), c(no = "No", yes = "Yes")),
        # A bulk selection can span types, so this offers the stored actions
        # under type-neutral labels rather than one type's vocabulary. Rows
        # whose type does not offer the chosen action are skipped, and the
        # notification reports how many actually changed.
        bulk_row("Action override", "bulk_simulation_value", "bulk_apply_simulation",
                 SIMULATION_OPTIONS, c(
                   synthesize = "Resample / simulate", pass_through = "Pass through",
                   scramble = "Scramble", drop = "Drop"
                 ))
      )
    })

    shiny::observeEvent(input$row_select, ignoreNULL = TRUE, {
      sel <- input$row_select
      variable <- as.character(sel$variable)
      if (is.null(variable) || !nzchar(variable)) return(invisible(NULL))
      current <- selected_vars()
      selected_vars(if (isTRUE(sel$checked)) {
        union(current, variable)
      } else {
        setdiff(current, variable)
      })
      invisible(NULL)
    })

    shiny::observeEvent(input$select_all_visible, ignoreNULL = TRUE, {
      vr <- visible_roles()
      shiny::req(vr)
      visible_vars <- vr$data$variable
      selected_vars(if (isTRUE(input$select_all_visible)) {
        union(selected_vars(), visible_vars)
      } else {
        setdiff(selected_vars(), visible_vars)
      })
      invisible(NULL)
    })

    shiny::observeEvent(input$bulk_clear, ignoreNULL = TRUE, {
      selected_vars(character(0))
      invisible(NULL)
    })

    # Applies one of the four per-row mutators to every currently-selected
    # row, writing the result back once (not once per row) so downstream
    # observers of state$roles only see a single update.
    bulk_apply <- function(mutate_row) {
      roles <- roles_local()
      if (is.null(roles)) return(invisible(NULL))
      rows <- match(intersect(selected_vars(), roles$variable), roles$variable)
      if (!length(rows)) return(invisible(NULL))
      changed <- 0L
      for (orig_row in rows) {
        before <- roles[orig_row, , drop = FALSE]
        roles  <- mutate_row(roles, orig_row)
        if (!identical(before, roles[orig_row, , drop = FALSE])) changed <- changed + 1L
      }
      roles_local(roles)
      state$roles <- roles
      skipped <- length(rows) - changed
      shiny::showNotification(
        sprintf(
          "Updated %d column%s.%s",
          changed, if (changed == 1L) "" else "s",
          if (skipped > 0L) {
            sprintf(" %d skipped -- that action does not apply to their type.", skipped)
          } else {
            ""
          }
        ),
        type = "message", duration = 2.5
      )
      invisible(NULL)
    }

    shiny::observeEvent(input$bulk_apply_type, ignoreNULL = TRUE, {
      val <- as.character(input$bulk_type_value)
      bulk_apply(function(roles, orig_row) apply_type_change(roles, orig_row, val))
    })

    shiny::observeEvent(input$bulk_apply_identifies, ignoreNULL = TRUE, {
      val <- as.character(input$bulk_identifies_value)
      attested <- isTRUE(state$attested_no_direct)
      bulk_apply(function(roles, orig_row) apply_identifies_change(roles, orig_row, val, attested))
    })

    shiny::observeEvent(input$bulk_apply_sensitive, ignoreNULL = TRUE, {
      val <- as.character(input$bulk_sensitive_value)
      bulk_apply(function(roles, orig_row) apply_sensitive_change(roles, orig_row, val))
    })

    shiny::observeEvent(input$bulk_apply_simulation, ignoreNULL = TRUE, {
      val <- as.character(input$bulk_simulation_value)
      bulk_apply(function(roles, orig_row) {
        roles <- ensure_simulation_column(roles)
        apply_simulation_change(roles, orig_row, val)
      })
    })

    do_confirm <- function() {
      roles <- roles_local()
      shiny::req(roles)
      roles <- ensure_simulation_column(roles)
      if (!roles_ready_for_generation(roles)) {
        shiny::showNotification(
          "Answer the privacy questions for every generated column before continuing.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      state$roles <- roles
      state$roles_confirmed <- (state$roles_confirmed %||% 0L) + 1L
      invisible(NULL)
    }

    shiny::observeEvent(input$confirm, ignoreNULL = TRUE, do_confirm())
    shiny::observeEvent(input$confirm_bottom, ignoreNULL = TRUE, do_confirm())

    invisible(NULL)
  })
}
