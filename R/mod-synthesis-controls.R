#' Internal Shiny Synthesis Controls Module
#'
#' @keywords internal
#' @noRd
mod_synthesis_controls_ui <- function(id) {
  mod_synthesis_controls_spec_ui(id)
}

#' @keywords internal
#' @noRd
mod_synthesis_controls_objective_ui <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 02 \u00b7 Objective"),
        shiny::tags$h1("Objective"),
        shiny::tags$p(
          class = "subtitle",
          shiny::tags$strong("Tell us what you'll use the synthetic data for"),
          " \u2014 that one choice presets sensible defaults for privacy hardening, ",
          "coarsening, and fidelity across the rest of the workflow."
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::conditionalPanel(
          condition = "input.purpose_group === 'analytics' && !input.acknowledge_risk",
          ns = ns,
          shiny::tags$button(
            type = "button",
            class = "btn btn-secondary action-button",
            disabled = "disabled",
            "Continue to Configure \u2192"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.purpose_group !== 'analytics' || input.acknowledge_risk",
          ns = ns,
          shiny::actionButton(ns("confirm_objective"), "Continue to Configure \u2192", class = "btn-primary")
        )
      )
    ),
    shiny::tags$div(
      class = "card",
      shiny::tags$div(
        class = "card-header",
        shiny::tags$span(class = "title", "Purpose"),
        shiny::tags$span(class = "sub", "presets the synthesis defaults")
      ),
      shiny::tags$p(class = "spec-question", "What are you creating synthetic data for?"),
      objective_cards(ns),
      shiny::tags$div(
        id = ns("purpose_detail_host"),
        shiny::uiOutput(ns("purpose_detail"))
      ),
      shiny::tags$div(
        style = "display:none",
        shiny::radioButtons(
          inputId = ns("purpose_group"),
          label = NULL,
          choiceValues = c("demo", "development", "analytics"),
          choiceNames = c("demo", "development", "analytics"),
          selected = "development"
        )
      )
    )
  )
}

#' @keywords internal
#' @noRd
dg_purpose_card <- function(ns, key, group, title, line, protection, risk = FALSE, selected = FALSE) {
  meter <- function(label, n, color) {
    shiny::tags$div(
      class = "pc-meter",
      shiny::tags$span(class = "pc-meter-lbl", label),
      shiny::tags$span(
        class = "pc-bars",
        lapply(seq_len(5L), function(i) {
          shiny::tags$span(
            class = "blk",
            style = sprintf(
              "font-size:11px;color:%s",
              if (i <= n) color else "var(--paper-300)"
            ),
            "\u25b0"
          )
        })
      )
    )
  }

  shiny::tags$div(
    class = paste("purpose-card", if (risk) "risk", if (selected) "selected"),
    `data-group` = group,
    `data-key` = key,
    onclick = sprintf(
      "DGsetPurpose(this,'%s','%s',%s)",
      group,
      key,
      if (identical(group, "prototype")) "true" else "false"
    ),
    shiny::tags$span(class = "pc-radio"),
    shiny::tags$div(
      class = "pc-body",
      shiny::tags$div(class = "pc-title", title),
      shiny::tags$div(class = "pc-line", line)
    ),
    shiny::tags$div(
      class = "pc-meters",
      meter("Protection", protection, if (risk) "var(--risk-500)" else "var(--real-700)")
    ),
    shiny::tags$div(class = "pc-detail-slot", `data-detail-slot` = key)
  )
}

#' @keywords internal
#' @noRd
objective_cards <- function(ns) {
  shiny::tagList(
    shiny::tags$div(
      class = "meter-legend",
      shiny::tags$div(
        shiny::tags$strong("Protection"),
        shiny::tags$span(
          "how strongly the data is shielded \u2014 combining coarsening, ",
          "disclosure protection, and k-anonymity. More bars = safer to ",
          "share, at the cost of less original detail preserved. (See the ",
          "details under each objective for the specifics.)"
        )
      )
    ),
    dg_purpose_card(
      ns, "demo", "demo", "Demo / Teaching",
      "Share externally, teach with, or use in presentations.", 5
    ),
    dg_purpose_card(
      ns, "development", "development", "Development and prototyping",
      "Build apps, AI tooling, or model pipelines.", 3, selected = TRUE
    ),
    dg_purpose_card(
      ns, "analytics", "analytics", "Internal Analytics",
      "Maximum structural detail \u2014 internal use only.", 1, risk = TRUE
    ),
    shiny::conditionalPanel(
      condition = "input.purpose_group === 'analytics'",
      ns = ns,
      shiny::tags$label(
        class = "pc-ack",
        shiny::tags$input(
          type = "checkbox",
          onclick = sprintf("Shiny.setInputValue('%s', this.checked, {priority: 'event'})", ns("acknowledge_risk"))
        ),
        shiny::tags$span("I understand this mode may preserve sensitive patterns and is for internal use only.")
      )
    )
  )
}

#' @keywords internal
#' @noRd
mod_synthesis_controls_spec_ui <- function(id, embedded = FALSE) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  header <- if (!isTRUE(embedded)) {
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 03 \u00b7 Synthesis Spec"),
        shiny::tags$h1("Configure synthesis"),
        shiny::tags$p(
          class = "subtitle",
          "Your objective presets the spec below. ",
          shiny::tags$strong("Review what DataGangeR will run"),
          " \u2014 and open ", shiny::tags$strong("Advanced Settings"),
          " only if you need to override individual knobs."
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::actionButton(ns("confirm"), "Confirm and Continue \u2192", class = "btn-primary")
      )
    )
  } else {
    NULL
  }

  shiny::tagList(
    header,
    # The objective recap card that stood here was removed: it restated a
    # choice already made in Step 02 and its "Change objective" link only
    # duplicated the sidebar stepper, which stays clickable for any unlocked
    # step (inst/app/app.R step_item()).
    shiny::tags$div(
      class = "card",
      shiny::tags$p(
        style = "margin:0 0 12px; color:var(--fg-muted); font-family:var(--font-sans); font-size:13px;",
        "Defaults are safe \u2014 leave unchanged unless you have a reason."
      ),
      shiny::tags$details(
        shiny::tags$summary("Advanced settings"),
        shiny::uiOutput(ns("advanced_settings"))
      )
    ),
    shiny::tags$div(
      class = "card",
      shiny::tags$details(
        shiny::tags$summary("Spec (for reproducibility)"),
        shiny::tags$div(
          class = "console",
          shiny::verbatimTextOutput(ns("spec_preview"))
        )
      )
    )
  )
}

#' Wrap a sliderInput so it is fully gated, without shinyjs
#'
#' Three layers, each doing something the others cannot:
#'   * `opacity` + `pointer-events:none` -- signals the state and blocks the
#'     mouse. Same visual idiom as the disabled selects in mod-roles.R.
#'   * `dg_disable_slider_tag()` -- blocks the keyboard, by telling
#'     ionRangeSlider to disable itself before it binds `keydown`.
#'   * `inert` -- ionRangeSlider generates its own focusable
#'     `<span class="irs-line" tabindex="0">` at init time, so without this the
#'     gated slider is still a tab stop: keyboard users land on a greyed
#'     control that does nothing and announces nothing. `inert` removes the
#'     whole subtree from the tab order and the accessibility tree, which no
#'     attribute on the input can do, because that span does not exist until
#'     the client renders it. Purely additive: where `inert` is unsupported the
#'     value is still protected by the layer above.
#'
#' @keywords internal
#' @noRd
dg_gate_slider_tag <- function(slider) {
  shiny::tags$div(
    style = "opacity:0.5; pointer-events:none;",
    inert = NA,
    dg_disable_slider_tag(slider)
  )
}

#' Mark a sliderInput tag as disabled, without shinyjs
#'
#' A wrapper styled `pointer-events:none` stops the mouse but NOT the keyboard.
#' ionRangeSlider builds its own widget at init time and binds the arrow keys to
#' a generated `<span class="irs-line" tabindex="0">`, not to the input shiny
#' renders, so CSS on the wrapper leaves the value fully editable. That would
#' make the gate look enforced while leaving it open, which is the worst of
#' both.
#'
#' `data-disable="true"` is what actually closes it. ionRangeSlider reads its
#' options from the input's data attributes and applies them LAST
#' (`$.extend(config, config_from_data)`), so they beat anything shiny passes --
#' shiny's binding only supplies `prettify`. With `disable` set, ionRangeSlider
#' sets `input.disabled`, appends its own `irs-disable-mask`, and skips
#' `bindEvents()` altogether, which is where the `keydown` handler would be
#' registered. No handler, no arrow keys. Verified in Chrome: six ArrowRight
#' presses moved an ungated control from 5 to 11 and left the gated one at 5.
#'
#' `tabindex="-1"` and `aria-disabled` go on the input for assistive tech.
#' They are not what blocks the keyboard -- ionRangeSlider sets `tabindex=-1`
#' on that input itself regardless -- so they are correctness, not enforcement.
#'
#' The input is located by its `js-range-slider` class rather than by position,
#' so a change in how shiny nests the tag cannot silently break this. If no
#' such node is found the tag is returned untouched: the visual gate still
#' applies and nothing errors.
#'
#' @keywords internal
#' @noRd
dg_disable_slider_tag <- function(tag) {
  has_class <- function(node) {
    cls <- node$attribs[["class"]]
    !is.null(cls) && any(grepl("js-range-slider", cls, fixed = TRUE))
  }

  mark <- function(node) {
    if (!inherits(node, "shiny.tag")) {
      return(node)
    }
    if (has_class(node)) {
      node$attribs[["data-disable"]] <- "true"
      node$attribs[["tabindex"]] <- "-1"
      node$attribs[["aria-disabled"]] <- "true"
      return(node)
    }
    if (length(node$children)) {
      node$children <- lapply(node$children, mark)
    }
    node
  }

  mark(tag)
}

#' Are both Configure questions answered for every column?
#'
#' Gate predicate for the k-anonymity slider. This is deliberately the SAME
#' predicate the Confirm button on the same step uses, so the two cannot
#' disagree: the slider unlocks exactly when Confirm unlocks. It is a thin
#' wrapper rather than a second implementation, because a parallel "all
#' answered" check would be free to drift away from the real one.
#'
#' roles_ready_for_generation() reads `user_identifies`/`user_sensitive`, not
#' `identifies`/`sensitive`. That matters: the latter pair are SEEDED by
#' detect_roles(), so a freshly uploaded file already carries
#' `identifies = "none"/"combination"/"direct"` and `sensitive = FALSE` for
#' every column before the user has answered anything. A predicate reading them
#' would treat a machine guess as a human answer and unlock the slider at upload
#' on any dataset whose columns all classify. Delegating also inherits the
#' exemption for dropped and passed-through columns, and the CLI shape where the
#' user_* columns are all NA.
#'
#' The only thing added here is a type guard: roles_ready_for_generation()
#' calls nrow() unguarded, so a non-data-frame reaches `if (!nrow(x))` with a
#' zero-length condition and errors. Inside a renderUI that would surface as a
#' broken panel, so anything that is not a roles table is simply "unanswered".
#' NULL and an empty table are unanswered for the same reason -- there is
#' nothing to configure yet, so the slider stays gated.
#'
#' @keywords internal
#' @noRd
dg_roles_all_answered <- function(roles) {
  if (!is.data.frame(roles)) {
    return(FALSE)
  }
  roles_ready_for_generation(roles)
}

#' @keywords internal
#' @noRd
mod_synthesis_controls_server <- function(id, state) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  # Each objective is described along the SAME dimensions, in the same order and
  # terminology, framed around the disclosure roles from the Configure page
  # (direct identifiers, quasi-identifiers, sensitive values). The exact-values
  # line is identical for all three on purpose: no original record is ever
  # reproduced; only distributions and relationships may carry over.
  exact_values_line <- paste(
    "Never reproduced. Every value is synthetic and no original record appears",
    "in the output."
  )
  purpose_copy <- list(
    demo = list(
      use_when = "Sharing externally, teaching, demos, or public examples, where safety matters more than fidelity.",
      exact_values = exact_values_line,
      distributions = "Approximated and simplified: rare categories are merged and dates coarsened, so each column's distribution is roughly right, not exact.",
      relationships = "Not preserved. Columns are generated independently, so relationships among quasi-identifiers and other variables are broken.",
      identifiers = "Direct identifiers are removed. Quasi-identifiers are coarsened and k-anonymity is enforced.",
      sensitive = "Sensitive and rare values are merged or dropped.",
      privacy_caution = "Not a formal privacy guarantee. Review all privacy warnings before sharing externally."
    ),
    development = list(
      use_when = "Building code, apps, AI tooling, or model pipelines that need realistic structure without exposing real records.",
      exact_values = exact_values_line,
      distributions = "Preserved per column: each column's distribution of values matches the original.",
      relationships = "Preserved between variables, including among quasi-identifiers, when synthpop is installed (otherwise columns are independent).",
      identifiers = "Direct identifiers are removed. Quasi-identifiers keep their distributions with light coarsening, and k-anonymity is enforced.",
      sensitive = "Sensitive value distributions are kept; very rare categories are merged.",
      privacy_caution = "Relationship-preserving synthesis may retain sensitive patterns. Not for external release."
    ),
    analytics = list(
      use_when = "Internal statistical work, validation studies, or auditing, where fidelity matters most and output stays internal.",
      exact_values = exact_values_line,
      distributions = "Preserved in full detail, including rare categories and precise dates.",
      relationships = "Strongly preserved between variables and among quasi-identifiers (high correlation fidelity).",
      identifiers = "Direct identifiers are removed, but quasi-identifiers receive minimal coarsening, so re-identification risk is higher.",
      sensitive = "Sensitive patterns may be retained. Internal use only.",
      privacy_caution = "May preserve sensitive patterns. Not for external sharing. Requires explicit risk acknowledgement."
    )
  )

  shiny::moduleServer(id, function(input, output, session) {
    purpose_default <- function() {
      input$purpose_group
    }

    current_purpose <- shiny::reactive({
      purpose_default()
    })

    shiny::observeEvent(input$confirm_objective, ignoreNULL = TRUE, {
      if (identical(current_purpose(), "analytics")) {
        shiny::req(isTRUE(input$acknowledge_risk))
      }

      state$objective_confirmed <- (state$objective_confirmed %||% 0L) + 1L
      invisible(NULL)
    })

    current_preset <- shiny::reactive({
      purpose <- current_purpose()
      shiny::req(purpose)
      preset_table(purpose)
    })

    # Coverage-based row-count suggestion (Feature 8). Pre-fills the row slider
    # with the minimum number of rows that still covers every observed category
    # combination, rather than blindly matching a large original row count.
    # Passes raw data + current roles so the suggestion re-fires when the user
    # changes a role on the Configure page (P3 UX polish).
    suggested_rows <- shiny::reactive({
      if (is.null(state$profile)) {
        return(NULL)
      }
      suggest_min_rows(state$profile, state$roles, data = state$raw_data)
    })

    default_n <- shiny::reactive({
      s <- suggested_rows()
      if (!is.null(s) && !is.na(s$n)) {
        s$n
      } else if (!is.null(state$raw_data)) {
        nrow(state$raw_data)
      } else {
        100L
      }
    })

    output$purpose_detail <- shiny::renderUI({
      if (!isTRUE(input$purpose_chosen)) {
        return(NULL)
      }
      purpose <- current_purpose()
      shiny::req(purpose)

      copy <- purpose_copy[[purpose]]
      shiny::div(
        class = "purpose-detail-panel",
        shiny::p(shiny::tags$strong("Use when:"), paste(copy$use_when)),
        shiny::p(shiny::tags$strong("Exact values:"), paste(copy$exact_values)),
        shiny::p(shiny::tags$strong("Distributions:"), paste(copy$distributions)),
        shiny::p(shiny::tags$strong("Relationships:"), paste(copy$relationships)),
        shiny::p(shiny::tags$strong("Identifiers:"), paste(copy$identifiers)),
        shiny::p(shiny::tags$strong("Sensitive & rare values:"), paste(copy$sensitive)),
        shiny::tags$div(
          class = "banner risk",
          shiny::tags$span(class = "icon", "!"),
          shiny::tags$div(
            shiny::tags$b("Privacy caution"),
            paste(copy$privacy_caution)
          )
        )
      )
    })

    # output$purpose_recap and its change_objective observer were removed with
    # the objective recap card above: that card held the only uiOutput binding
    # and the only source of input$change_objective, so both had become
    # unreachable. The sidebar stepper still navigates back to Step 02.

    # One-line explanation rendered directly under a control.
    setting_hint <- function(txt) {
      shiny::tags$p(
        class = "text-muted",
        style = "margin-top:-8px;margin-bottom:12px;font-size:12px;",
        txt
      )
    }

    output$advanced_settings <- shiny::renderUI({
      purpose <- current_purpose()
      shiny::req(purpose)
      preset <- current_preset()
      # isolate()d on purpose: default_n() reaches state$roles through
      # suggested_rows(), so reading it reactively rebuilt this whole panel
      # every time the user answered a column question -- silently resetting
      # rows_n, engine, seed, name_strategy, the rare threshold and the
      # checkboxes to preset defaults mid-workflow. This value is only a
      # starting point for the row slider; the live suggestion still tracks the
      # roles through output$rows_hint below, which re-renders on its own.
      # The panel now rebuilds only when the objective changes, which is the
      # one time its presets genuinely change.
      current_n <- shiny::isolate(default_n())

      shiny::tagList(
        # Five controls that protect the people in the data stay open; the
        # three that only shape the output collapse away below them.
        shiny::tags$div(
          class = "card-header",
          shiny::tags$span(class = "title", "Privacy controls"),
          shiny::tags$span(class = "sub", "protect the people in this data")
        ),
        shiny::selectInput(
          inputId = session$ns("name_strategy"),
          label = "Column name handling",
          choices = c(
            "Keep original column names" = "preserve",
            "Replace with generic names (var1, var2, ...)" = "generic",
            "Anonymize names, keep mapping in the data dictionary" = "dictionary_only"
          ),
          selected = preset$name_strategy
        ),
        setting_hint("Whether the synthetic data keeps your original column names or hides them."),
        shiny::sliderInput(
          inputId = session$ns("rare_level_min_n"),
          label = "Rare category threshold",
          min = 2,
          max = 30,
          value = preset$rare_level_min_n,
          step = 1
        ),
        setting_hint("Counts how often a single value appears in a single column. A value seen fewer times than this counts as rare, because the value itself can name someone."),
        shiny::uiOutput(session$ns("rare_hint")),
        # The k_anon slider, its hint and its readout live in their own
        # renderUI (kanon_control below) because the gate has to track the
        # column answers, and this panel deliberately no longer does.
        shiny::uiOutput(session$ns("kanon_control")),
        shiny::checkboxInput(
          inputId = session$ns("coarsen_dates"),
          label = "Coarsen dates",
          value = isTRUE(preset$coarsen_dates)
        ),
        setting_hint("Rounds dates (e.g. to month or year) so an exact event date cannot single out an individual."),
        shiny::checkboxInput(
          inputId = session$ns("merge_rare"),
          label = "Merge rare categories",
          value = isTRUE(preset$merge_rare)
        ),
        setting_hint("Combines infrequent category values into an 'other' group to reduce re-identification risk."),
        shiny::tags$details(
          shiny::tags$summary("Output settings"),
          shiny::numericInput(
            inputId = session$ns("rows_n"),
            label = "Row count (n)",
            value = current_n,
            min = 1
          ),
          setting_hint("How many synthetic rows to generate."),
          shiny::uiOutput(session$ns("rows_hint")),
          shiny::selectInput(
            inputId = session$ns("engine"),
            label = "Engine",
            choices = c(
              "auto (derived from objective)" = "auto",
              "internal (marginal, no dependencies)" = "internal",
              "synthpop (relationship-aware)" = "synthpop"
            ),
            selected = "auto"
          ),
          shiny::tags$p(
            class = "text-muted",
            style = "margin-top:-8px;margin-bottom:12px;font-size:12px;",
            if (rlang::is_installed("synthpop")) {
              "\u2713 synthpop is installed"
            } else {
              "\u26a0 synthpop not installed \u2014 selecting it will fall back to internal"
            }
          ),
          shiny::tags$div(
            class = "engine-help",
            shiny::tags$p(
              shiny::tags$strong("Auto"),
              " \u2014 picks the engine from your objective. Recommended unless you have a reason to override."
            ),
            shiny::tags$p(
              shiny::tags$strong("Internal"),
              " \u2014 synthesises each column from its own distribution (marginals only). Fast, dependency-free, ignores relationships between columns."
            ),
            shiny::tags$p(
              shiny::tags$strong("synthpop"),
              " \u2014 models columns conditionally on one another, so correlations and joint structure are preserved. Higher fidelity; requires the synthpop package."
            )
          ),
          shiny::numericInput(
            inputId = session$ns("seed"),
            label = "Seed",
            value = preset$seed %||% NA
          ),
          setting_hint("Fixes the random draw so the same settings reproduce the exact same synthetic data."),
          shiny::selectInput(
            inputId = session$ns("preserve_missingness"),
            label = "Preserve missing values",
            choices = c(
              "Approximate the original missing-value rate" = "approx",
              "Match the original missing-value pattern exactly" = "exact",
              "Do not reproduce missing values" = "none"
            ),
            selected = preset$preserve_missingness %||% "approx"
          ),
          setting_hint("How closely to reproduce the pattern of missing (NA) values from the original data.")
        )
      )
    })
    shiny::outputOptions(output, "advanced_settings", suspendWhenHidden = FALSE)

    # The k_anon control in its own renderUI so that ITS dependency on
    # state$roles (the answer gate) never re-renders the parent panel. Only
    # this control re-renders when a column question is answered, and the
    # isolate()d read below keeps whatever the user already chose.
    output$kanon_control <- shiny::renderUI({
      roles <- state$roles
      answered <- dg_roles_all_answered(roles)
      preset <- current_preset()
      # isolate() is mandatory here: reading input$k_anon reactively inside
      # the very renderUI that creates the k_anon input would re-trigger this
      # output forever. The isolated read only preserves the user's current
      # value across re-renders; the first render falls back to the preset.
      current_k <- shiny::isolate(input$k_anon) %||% (preset$k_anon %||% 5)

      slider <- shiny::sliderInput(
        inputId = session$ns("k_anon"),
        label = "Minimum group size",
        min = 2,
        max = 30,
        value = current_k,
        step = 1
      )

      shiny::tagList(
        if (answered) {
          slider
        } else {
          dg_gate_slider_tag(slider)
        },
        # Deliberately contrasted with the rare-category threshold above: that
        # one counts one value in one column, this one counts rows sharing a
        # combination across the columns marked as identifying in combination.
        setting_hint("Counts how many rows share the same combination of the columns you marked as identifying in combination. Combinations held by fewer rows than this are coarsened, then blanked."),
        if (!answered) {
          setting_hint("Answer both questions for every column in the table above to enable this.")
        },
        # The readout is only rendered once the gate is open: while the
        # slider is disabled its "nothing to act on" message would just
        # repeat the reason line above it.
        if (answered) {
          shiny::uiOutput(session$ns("kanon_hint"))
        }
      )
    })
    shiny::outputOptions(output, "kanon_control", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$rows_n, ignoreNULL = TRUE, {
      if (!is.null(input$rows_n) && isTRUE(input$rows_n > 500000)) {
        shiny::showNotification(
          "Large row counts may be slow to synthesize.",
          type = "warning"
        )
      }
    })

    # Both sliders get a live readout of what the current value does to THIS
    # dataset. The two settings are easy to confuse -- they share a default of
    # 5 but one is univariate and the other is not -- so each readout names the
    # thing it counts (values in a column vs combinations across columns).
    hint_box <- function(...) {
      shiny::tags$p(
        class = "text-muted",
        style = paste(
          "margin-top:-6px;margin-bottom:14px;font-size:12px;",
          "padding:6px 10px;border-left:2px solid var(--border, #d4d4d4);"
        ),
        ...
      )
    }

    output$rare_hint <- shiny::renderUI({
      data <- state$raw_data
      # length(thr) must be tested before is.na(): the sliders are created
      # inside renderUI, so on the first pass the input is still NULL,
      # as.integer(NULL) is integer(0), and `|| is.na(integer(0))` aborts the
      # render with "missing value where TRUE/FALSE needed".
      thr  <- suppressWarnings(as.integer(input$rare_level_min_n))
      if (is.null(data) || !nrow(data) || !length(thr) || is.na(thr)) {
        return(NULL)
      }

      is_cat <- vapply(data, function(col) {
        is.character(col) || is.factor(col)
      }, logical(1))
      cat_cols <- names(data)[is_cat]
      if (length(cat_cols) == 0L) {
        return(hint_box(
          "No text or category columns in this dataset, so this setting has ",
          "nothing to act on."
        ))
      }

      counts <- vapply(cat_cols, function(nm) {
        tbl <- table(as.character(data[[nm]]), useNA = "no")
        c(rare = sum(tbl < thr), total = length(tbl))
      }, numeric(2))
      n_rare  <- sum(counts["rare", ])
      n_total <- sum(counts["total", ])
      n_cols  <- sum(counts["rare", ] > 0)

      roles <- state$roles
      n_masked <- if (!is.null(roles) && "label_strategy" %in% names(roles)) {
        sum(roles$label_strategy %in% "mask_rare", na.rm = TRUE)
      } else {
        0L
      }

      hint_box(
        shiny::tags$strong(sprintf("At %d: ", thr)),
        sprintf(
          "%s of %s distinct values are rare, spread over %s of %s text or category %s. ",
          format(n_rare, big.mark = ","), format(n_total, big.mark = ","),
          n_cols, length(cat_cols),
          if (length(cat_cols) == 1L) "column" else "columns"
        ),
        if (n_masked > 0L) {
          sprintf(
            "%s %s set to mask rare values, so each rare value there is replaced by a neutral placeholder. Any column not set to mask keeps its values unchanged.",
            n_masked,
            if (n_masked == 1L) "column is" else "columns are"
          )
        } else {
          "No column is set to mask rare values, so nothing is replaced -- this threshold only feeds the privacy report. Set a column's action to 'Resample (rare levels masked)' on the Configure step to act on it."
        }
      )
    })

    output$kanon_hint <- shiny::renderUI({
      data  <- state$raw_data
      roles <- state$roles
      # See the rare_level_min_n guard above: length() first, or the initial
      # render throws before the slider input exists.
      k     <- suppressWarnings(as.integer(input$k_anon))
      if (is.null(data) || !nrow(data) || is.null(roles) ||
          !length(k) || is.na(k)) {
        return(NULL)
      }

      qi <- intersect(dg_kanon_columns(roles), names(data))
      if (length(qi) == 0L) {
        return(hint_box(
          "No columns are marked as identifying in combination, so this ",
          "setting has nothing to act on. Mark them in the column table above."
        ))
      }

      res <- assess_kanonymity(data, qi, k = k)
      # Combination count is derived here rather than read off
      # res$worst_cells, which disclosure-risk.R caps at 10 rows for display.
      # kanon_key() is the same keying the enforcement path uses, so this
      # readout cannot describe a different set of combinations than the one
      # enforce_kanon() will act on.
      key <- kanon_key(data, qi)
      combo_counts <- table(key)
      n_combos <- length(combo_counts)
      n_small  <- sum(combo_counts < k)

      hint_box(
        shiny::tags$strong(sprintf("At %d: ", k)),
        sprintf(
          "your %s combination %s (%s) %s %s distinct %s in this data. ",
          length(qi),
          if (length(qi) == 1L) "column" else "columns",
          paste(utils::head(qi, 4L), collapse = ", "),
          if (length(qi) > 4L) sprintf("and %d more form", length(qi) - 4L) else "form",
          format(n_combos, big.mark = ","),
          if (n_combos == 1L) "combination" else "combinations"
        ),
        if (n_small == 0L) {
          "None are held by too few rows, so nothing will be coarsened or blanked."
        } else {
          sprintf(
            "%s of them %s held by fewer than %d rows, covering %s of %s rows (%.0f%%). Those cells are coarsened first, and blanked if coarsening is not enough.",
            format(n_small, big.mark = ","),
            if (n_small == 1L) "is" else "are",
            k,
            format(res$n_below, big.mark = ","),
            format(nrow(data), big.mark = ","),
            res$pct_below
          )
        }
      )
    })

    output$rows_hint <- shiny::renderUI({
      s <- suggested_rows()
      if (is.null(s)) {
        return(NULL)
      }
      hint <- if (!is.na(s$combination_count)) {
        sprintf(
          "Suggested: %s rows (covers all %s observed category combinations). Original: %s rows.",
          format(s$n, big.mark = ","),
          format(s$combination_count, big.mark = ","),
          format(s$original_n, big.mark = ",")
        )
      } else {
        sprintf(
          "Suggested: %s rows. Original: %s rows.",
          format(s$n, big.mark = ","), format(s$original_n, big.mark = ",")
        )
      }
      below <- !is.null(input$rows_n) && !is.na(input$rows_n) &&
        !is.na(s$n) && input$rows_n < s$n
      shiny::tagList(
        shiny::tags$p(
          class = "text-muted",
          style = "margin-top:-8px; margin-bottom:12px; font-size:12px;",
          hint
        ),
        if (below) {
          shiny::tags$div(
            class = "banner risk",
            style = "margin-bottom:12px;",
            shiny::tags$span(class = "icon", "!"),
            shiny::tags$div(
              shiny::tags$b("Below coverage floor."),
              " Some category combinations may not appear in the synthetic data."
            )
          )
        }
      )
    })

    current_spec <- shiny::reactive({
      purpose <- current_purpose()
      shiny::req(purpose)
      preset <- current_preset()
      current_rows_n <- input$rows_n
      current_seed <- input$seed
      current_name_strategy <- if (!is.null(input$name_strategy)) {
        input$name_strategy
      } else {
        preset$name_strategy
      }
      # Always honour the row-count input. It pre-fills to the coverage-based
      # suggestion (Feature 8), which is often below the original row count, so
      # we cannot treat "equals the default" as "leave n unset" - that would
      # silently fall back to the full original size.
      n_arg <- NULL
      if (!is.null(current_rows_n) && !is.na(current_rows_n)) {
        n_arg <- as.integer(current_rows_n)
      }

      seed_arg <- NULL
      if (!is.null(current_seed) && !is.na(current_seed)) {
        seed_arg <- as.integer(current_seed)
      }

      engine_arg <- if (!is.null(input$engine) && !identical(input$engine, "auto")) {
        input$engine
      } else {
        NULL
      }

      tryCatch(
        synth_spec(
          purpose = purpose,
          n = n_arg,
          roles = state$roles,
          name_strategy = current_name_strategy,
          seed = seed_arg,
          acknowledge_risk = isTRUE(input$acknowledge_risk),
          rare_level_min_n = input$rare_level_min_n %||% preset$rare_level_min_n,
          k_anon = input$k_anon %||% preset$k_anon %||% 5,
          coarsen_dates = isTRUE(input$coarsen_dates %||% preset$coarsen_dates),
          merge_rare = isTRUE(input$merge_rare %||% preset$merge_rare),
          free_text_strategy = preset$free_text_strategy,
          preserve_missingness = input$preserve_missingness %||% preset$preserve_missingness %||% "approx",
          engine = engine_arg
        ),
        error = function(e) {
          if (!(identical(purpose, "analytics") && !isTRUE(input$acknowledge_risk))) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
          NULL
        }
      )
    })

    output$spec_preview <- shiny::renderPrint({
      spec <- current_spec()
      shiny::req(spec)
      print(spec)

      if (identical(spec$purpose, "development")) {
        cat("Relationship-aware synthesis uses synthpop when installed.\n")
      }
    })

    shiny::observeEvent(input$confirm, ignoreNULL = TRUE, {
      spec <- current_spec()
      shiny::req(spec)

      roles <- dg_ensure_ui_roles(state$roles)
      if (!is.null(roles) && !roles_ready_for_generation(roles)) {
        unset <- length(roles_generation_pending(roles))
        shiny::showNotification(
          sprintf(
            "%d column%s still need an answer before you can generate.",
            unset, if (unset == 1L) "" else "s"
          ),
          type = "warning", duration = 6
        )
        return(invisible(NULL))
      }

      if (identical(current_purpose(), "analytics")) {
        shiny::req(isTRUE(input$acknowledge_risk))
      }

      state$spec <- spec
      # Keeps the Configure page's k-anonymity readout in step with the slider,
      # which now owns k. The Generate escape route writes both fields too.
      state$k_anon <- spec$k_anon
      state$spec_confirmed <- (state$spec_confirmed %||% 0L) + 1L
      invisible(NULL)
    })

    list(
      current_purpose = current_purpose,
      current_spec = current_spec
    )
  })
}
