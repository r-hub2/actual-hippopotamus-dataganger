#' Internal Shiny Generate Module
#'
#' @keywords internal
#' @noRd
generate_notification <- function(...) {
  hook <- getOption("dataganger.generate_notification_hook")
  if (is.function(hook)) {
    hook(list(...))
  }

  shiny::showNotification(...)
}

#' @keywords internal
#' @noRd
is_kanon_infeasible_warning <- function(text) {
  grepl(
    "Could not apply k-anonymity|no k-anonymity protection was applied to this output",
    text %||% "",
    ignore.case = TRUE
  )
}

#' @keywords internal
#' @noRd
kanon_action_seed <- function(spec) {
  spec$seed %||% sample.int(.Machine$integer.max, 1L)
}

#' @keywords internal
#' @noRd
apply_kanon_provenance <- function(synthetic, provenance = NULL) {
  kanon <- attr(synthetic, "kanon", exact = TRUE)
  if (is.null(kanon)) {
    return(synthetic)
  }

  kanon$k_default <- provenance$k_default %||% kanon$k_default %||% 5L
  kanon$k_provenance <- provenance$k_provenance %||% kanon$k_provenance %||% "default"
  attr(synthetic, "kanon") <- kanon
  synthetic
}

#' @keywords internal
#' @noRd
render_kanon_escape_panel <- function(session, kanon, escape_routes) {
  qi_cols <- kanon$qi_cols %||% character(0)
  current_k <- kanon$k %||% "unknown"
  driver_col <- escape_routes$driver_col %||% NULL
  feasible_k <- escape_routes$feasible_k %||% NULL
  suggested_n <- escape_routes$suggested_n %||% NULL

  action_buttons <- list()
  if (!is.null(feasible_k) && feasible_k < current_k) {
    action_buttons[[length(action_buttons) + 1L]] <- shiny::actionButton(
      session$ns("apply_escape_k"),
      sprintf("Apply k = %s and regenerate", feasible_k),
      class = "btn btn-warning"
    )
  }
  if (!is.null(suggested_n)) {
    action_buttons[[length(action_buttons) + 1L]] <- shiny::actionButton(
      session$ns("apply_escape_n"),
      sprintf("Generate %s rows at k = %s", suggested_n, current_k),
      class = "btn btn-secondary"
    )
  }
  action_buttons[[length(action_buttons) + 1L]] <- shiny::actionButton(
    session$ns("adjust_advanced_settings"),
    "Change k in Advanced settings",
    class = "btn btn-secondary",
    onclick = paste(
      "var output=document.getElementById('synthesis_controls-advanced_settings');",
      "var panel=output ? output.closest('details') : null;",
      "if(panel){panel.open=true;",
      "window.setTimeout(function(){panel.scrollIntoView({behavior:'smooth',block:'center'});},300);}"
    )
  )

  guidance <- if (!is.null(driver_col)) {
    shiny::tags$p(
      class = "help",
      style = "margin:10px 0 0;",
      shiny::tagList(
        "If ",
        shiny::tags$code(driver_col),
        " does not need to be a ",
        dg_privacy_term("quasi-identifier (QI)", "qi"),
        ", mark it as ",
        shiny::tags$strong("No"),
        " for Q1 and regenerate. It has the most distinct values in this set."
      )
    )
  } else {
    NULL
  }

  probe_note <- if (isTRUE(escape_routes$skipped_n_probe)) {
    shiny::tags$p(
      class = "help",
      style = "margin:10px 0 0;",
      "Row-count suggestions are skipped for datasets larger than 50,000 rows."
    )
  } else {
    NULL
  }

  shiny::tags$div(
    class = "card",
    style = paste(
      "margin-top:12px; background:var(--risk-50);",
      "border:1px solid var(--risk-300); border-left:4px solid var(--risk-500);"
    ),
    shiny::tags$div(
      class = "card-header",
      shiny::tags$span(class = "title", "k-anonymity was not applied"),
      shiny::tags$span(class = "sub", "choose one of the computed ways forward")
    ),
    shiny::tags$p(
      style = "margin-top:8px;",
      shiny::tagList(
        "DataGangeR could not enforce ",
        dg_privacy_term("k-anonymity", "k_anonymity"),
        " at ",
        dg_privacy_term("k", "k"),
        " = ",
        current_k,
        " across these ",
        dg_privacy_term("quasi-identifier (QI)", "qi"),
        " columns: ",
        shiny::tags$span(
          style = "font-family:var(--font-mono); color:var(--fg-default);",
          paste(qi_cols, collapse = ", ")
        ),
        "."
      )
    ),
    shiny::tags$p(
      style = "margin:0;",
      shiny::tagList(
        "Applying it would require too much ",
        dg_privacy_term("suppression", "suppression"),
        ", so no k-anonymity protection was applied to this output."
      )
    ),
    if (length(action_buttons) > 0L) {
      shiny::tags$div(
        style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:12px;",
        action_buttons
      )
    } else {
      shiny::tags$p(
        class = "help",
        style = "margin-top:12px;",
        "No computed regenerate shortcut was feasible inside the current probe limits."
      )
    },
    guidance,
    probe_note
  )
}

#' @keywords internal
#' @noRd
render_kanon_applied_panel <- function(kanon) {
  row_frac <- kanon$suppressed_row_frac %||% 0
  detail <- sprintf(
    "Every quasi-identifier combination in this output appears in at least %s rows.",
    kanon$k %||% "k"
  )
  if (row_frac > 0) {
    detail <- sprintf(
      "%s %s%% of QI values suppressed to enforce it.",
      detail,
      round(100 * row_frac)
    )
  }

  shiny::tags$div(
    class = "card",
    style = "margin-top:12px; border-left:4px solid var(--synth-600);",
    shiny::tags$div(
      class = "card-header",
      shiny::tags$span(class = "title", "k-anonymity was applied"),
      shiny::tags$span(class = "sub", "generation privacy status")
    ),
    shiny::tags$p(style = "margin:8px 0 0;", detail)
  )
}

#' @keywords internal
#' @noRd
mod_generate_ui <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 04 \u00b7 Generation"),
        shiny::tags$h1("Generate synthetic data"),
        shiny::uiOutput(ns("header_subtitle"))
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::uiOutput(ns("header_cta"))
      )
    ),
    stale_banner_ui(
      "synthesis",
      ns = ns,
      title = NULL,
      message = "Review the config, press Generate when ready, or go back to adjust settings."
    ),
    shiny::uiOutput(ns("gen_status")),
    # When synthetic data exists, the success banner + KPI panels pin to the top,
    # above the configuration recap.
    shiny::uiOutput(ns("result_stats")),
    shiny::div(
      class = "card",
      shiny::tags$div(
        class = "card-header",
        shiny::tags$span(class = "title", "Your configuration"),
        shiny::tags$span(class = "sub", "from steps 1\u20133")
      ),
      shiny::uiOutput(ns("config_recap")),
      shiny::uiOutput(ns("decision_recap"))
    ),
    shiny::uiOutput(ns("generate_actions"))
  )
}

#' @keywords internal
#' @noRd
mod_generate_server <- function(id, state) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  shiny::moduleServer(id, function(input, output, session) {
    output$header_subtitle <- shiny::renderUI({
      if (!is.null(state$synthetic)) {
        shiny::tags$p(
          class = "subtitle",
          shiny::tags$strong("Synthetic data ready."),
          " The right panel now shows the doppelg\u00e4nger. Regenerate to see how output varies with a new seed, or continue to Compare to inspect distribution drift."
        )
      } else {
        shiny::tags$p(
          class = "subtitle",
          shiny::tags$strong("Click Generate"),
          " to create your synthetic dataset using the configuration from Step 03. Generation is fast \u2014 the synthetic preview will appear in the right panel as soon as it's done."
        )
      }
    })

    output$header_cta <- shiny::renderUI({
      if (isTRUE(generating())) {
        shiny::actionButton(
          session$ns("cancel"),
          "Cancel generation",
          class = "btn btn-danger"
        )
      } else if (!is.null(state$synthetic)) {
        shiny::div(
          class = "generate-header-actions",
          shiny::actionButton(
            session$ns("adjust_settings"),
            "\u2190 Back to settings",
            class = "btn btn-secondary"
          ),
          shiny::actionButton(
            session$ns("try_new_seed"),
            "Regenerate",
            class = "btn btn-regenerate"
          ),
          shiny::actionButton(
            session$ns("go_compare"),
            "Continue to Compare \u2192",
            class = "btn btn-primary"
          )
        )
      } else {
        shiny::actionButton(
          session$ns("generate"),
          "\u25b6 Generate Synthetic Data",
          class = "btn btn-primary"
        )
      }
    })

    output$config_recap <- shiny::renderUI({
      spec <- state$spec
      roles <- state$roles
      if (is.null(spec)) {
        return(shiny::tags$p(
          class = "subtitle",
          "Confirm your settings in Configuration to see a summary here."
        ))
      }
      raw_data <- state$raw_data %||% data.frame()
      n_over <- if (!is.null(roles)) sum(!is.na(roles$user_role) & nzchar(roles$user_role)) else 0L
      configured_engine <- spec[["engine", exact = TRUE]] %||% "auto"
      resolved_engine <- attr(state$synthetic, "engine", exact = TRUE)
      engine <- if (!is.null(state$synthetic) && !is.null(resolved_engine) && nzchar(resolved_engine)) {
        paste0(resolved_engine, " (", configured_engine, ")")
      } else {
        configured_engine
      }
      row <- function(label, value) {
        shiny::tags$tr(
          shiny::tags$td(class = "name", label),
          shiny::tags$td(value)
        )
      }
      dash <- "\u2014"
      fmt_val <- function(x) {
        if (is.null(x) || (length(x) == 1L && is.na(x))) {
          return(dash)
        }
        if (is.logical(x)) {
          return(if (isTRUE(x)) "yes" else "no")
        }
        as.character(x)
      }
      preserve_missingness <- spec[["preserve_missingness", exact = TRUE]]
      rare_level_min_n <- spec[["rare_level_min_n", exact = TRUE]]
      coarsen_dates <- spec[["coarsen_dates", exact = TRUE]]
      merge_rare <- spec[["merge_rare", exact = TRUE]]
      level <- spec[["level", exact = TRUE]]
      shiny::tags$table(
        class = "data",
        style = "margin-top:8px;",
        shiny::tags$tbody(
          row("Objective", spec$purpose %||% dash),
          row("Engine", engine),
          row("Rows to generate", as.character(spec$n %||% nrow(raw_data))),
          row("Seed", if (!is.null(spec$seed)) as.character(spec$seed) else "random per run"),
          row("Role overrides", sprintf("%d column(s) changed by you", n_over)),
          row("Privacy level", fmt_val(level)),
          row("Preserve missingness", fmt_val(preserve_missingness)),
          row("Rare level min n", fmt_val(rare_level_min_n)),
          row("Coarsen dates", fmt_val(coarsen_dates)),
          row("Merge rare levels", fmt_val(merge_rare))
        )
      )
    })

    output$decision_recap <- shiny::renderUI({
      roles <- state$roles
      if (is.null(roles) || !nrow(roles)) {
        return(NULL)
      }

      recap <- dg_decision_recap_table(roles)

      # Columns that actually go through the synthesis engine together. Their
      # univariate shapes and their mutual (multivariate) relationships are the
      # ones the engine reproduces; pass-through keeps real values, drop removes.
      sim <- if ("simulation" %in% names(roles)) roles$simulation else rep("synthesize", nrow(roles))
      sim[is.na(sim)] <- "synthesize"
      synth_cols <- roles$variable[sim == "synthesize"]

      rows <- lapply(seq_len(nrow(recap)), function(i) {
        shiny::tags$tr(
          shiny::tags$td(
            style = "width:22%; padding:6px 8px;",
            title = paste0("Modelled as: ", recap$type[[i]]),
            recap$variable[[i]]
          ),
          shiny::tags$td(style = "width:24%; padding:6px 8px;", recap$points_to_person[[i]]),
          shiny::tags$td(style = "width:12%; padding:6px 8px;", recap$sensitive[[i]]),
          shiny::tags$td(style = "width:16%; padding:6px 8px;", recap$action[[i]]),
          shiny::tags$td(style = "width:26%; padding:6px 8px;", recap$what_we_do[[i]])
        )
      })

      shiny::tags$div(
        style = "margin-top:12px;",
        shiny::tags$p(
          class = "help",
          style = "margin:4px 0 8px;",
          "These are your final per-column choices from Configure: the two privacy questions you answered, ",
          shiny::tags$strong("Action"), " for the resulting column handling, and ",
          shiny::tags$strong("What we'll do"), " as the plain-English outcome. ",
          shiny::tags$strong("TYPE"), " is shown in the tooltip on each column name. ",
          "Use \u2190 Adjust settings to change any of these."
        ),
        shiny::tags$table(
          class = "data compact",
          style = "width:100%; border-collapse:collapse; margin-top:8px;",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th(style = "width:22%; padding:6px 8px;", "Column"),
              shiny::tags$th(style = "width:24%; padding:6px 8px;", "Points to a person?"),
              shiny::tags$th(style = "width:12%; padding:6px 8px;", "Sensitive?"),
              shiny::tags$th(style = "width:16%; padding:6px 8px;", "Action"),
              shiny::tags$th(style = "width:26%; padding:6px 8px;", "What we'll do")
            )
          ),
          shiny::tags$tbody(rows)
        ),
        shiny::tags$div(
          style = paste(
            "margin-top:12px; padding:10px 12px; border:1px solid var(--paper-200);",
            "border-radius:4px; background:rgba(251,250,246,0.6);",
            "font-family:var(--font-sans); font-size:14px; line-height:1.6; color:var(--fg-muted);"
          ),
          shiny::tags$div(
            style = "margin-bottom:6px;",
            shiny::tags$strong("What the synthetic data preserves")
          ),
          shiny::tags$div(
            style = "margin-bottom:4px;",
            shiny::tags$b("Univariate \u2014 each column's own shape. "),
            "Every synthesised column reproduces its original distribution: ",
            "category frequencies, spread, and percentiles match the real data."
          ),
          shiny::tags$div(
            shiny::tags$b("Multivariate \u2014 relationships between columns. "),
            if (length(synth_cols) >= 2L) {
              shiny::tagList(
                "The engine models these columns conditionally on one another, so ",
                "correlations and joint patterns among them are carried into the output: ",
                shiny::tags$span(
                  style = "font-family:var(--font-mono); color:var(--fg-default);",
                  paste(synth_cols, collapse = ", ")
                ),
                "."
              )
            } else if (length(synth_cols) == 1L) {
              shiny::tagList(
                "Only one column (",
                shiny::tags$span(
                  style = "font-family:var(--font-mono); color:var(--fg-default);",
                  synth_cols
                ),
                ") is synthesised, so there are no cross-column relationships to preserve."
              )
            } else {
              "No columns are being synthesised, so there are no relationships to preserve."
            }
          ),
          shiny::tags$div(
            style = "margin-top:6px;",
            "Pass-through columns keep their real values; dropped columns are removed. ",
            "Verify both kinds of fidelity on the Compare step after generating."
          )
        )
      )
    })

    last_duration <- shiny::reactiveVal(NULL)
    generating <- shiny::reactiveVal(FALSE)
    proc <- shiny::reactiveVal(NULL) # callr background process
    run_started_at <- shiny::reactiveVal(NULL)
    run_seed <- shiny::reactiveVal(NULL)
    elapsed_secs <- shiny::reactiveVal(NULL) # live timer during generation

    output$stale__synthesis <- shiny::renderText({
      if (isTRUE(state$stale$synthesis)) {
        "true"
      } else {
        "false"
      }
    })

    shiny::outputOptions(output, "stale__synthesis", suspendWhenHidden = FALSE)

    # Commit a finished pipeline result into app state.
    finalize_result <- function(result) {
      synthetic <- result$synthetic
      synthetic <- apply_kanon_provenance(
        synthetic,
        provenance = state$kanon_next_provenance %||% NULL
      )
      result$synthetic <- synthetic
      last_duration(as.numeric(difftime(Sys.time(), run_started_at() %||% Sys.time(), units = "secs")))
      state$seed_used <- run_seed()
      state$synthetic <- synthetic
      state$comparison <- result$comparison
      state$privacy <- result$privacy
      state$kanon <- attr(synthetic, "kanon", exact = TRUE)
      state$pipeline_warnings <- result$warnings %||% character(0)
      state$kanon_escape_routes <- if (isTRUE(state$kanon$infeasible)) {
        kanon_escape_routes(
          data = state$raw_data,
          roles = state$roles,
          k = state$kanon$k %||% 5L
        )
      } else {
        NULL
      }
      state$generated_roles <- state$roles
      visible_warnings <- if (isTRUE(state$kanon$infeasible)) {
        state$pipeline_warnings[!vapply(state$pipeline_warnings, is_kanon_infeasible_warning, logical(1))]
      } else {
        state$pipeline_warnings
      }
      for (warning_text in visible_warnings) {
        generate_notification(warning_text, type = "warning", duration = NULL)
      }
      state$kanon_next_provenance <- NULL
      state$stale$synthesis <- FALSE
      state$stale$comparison <- FALSE
      state$stale$export <- FALSE
      invisible(NULL)
    }

    # Synchronous fallback used when callr is unavailable. No cancel button,
    # but identical results \u2014 the app stays usable without the optional dep.
    run_synthesis_sync <- function(spec_with_seed) {
      result <- tryCatch(
        shiny::withProgress(message = "Synthesizing\u2026", value = 0.05, {
          shiny::setProgress(
            value  = 0.05,
            detail = "Modelling columns \u2014 this can take a moment on larger data"
          )
          out <- dg_timeit(
            "generate: pipeline",
            run_synthesis_pipeline(state$raw_data, spec_with_seed, roles = state$roles)
          )
          shiny::setProgress(value = 1.0, detail = "Finalising\u2026")
          out
        }),
        error = function(e) {
          generate_notification(
            paste("Synthesis failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          NULL
        }
      )
      generating(FALSE)
      if (!is.null(result)) finalize_result(result)
      invisible(NULL)
    }

    # Start a run. Prefers the cancellable background process; falls back to a
    # synchronous run when callr is not installed.
    run_synthesis <- function(seed) {
      if (!roles_ready_for_generation(state$roles)) {
        generate_notification(
          "Finish the column privacy questions in Configure before generating.",
          type = "warning"
        )
        return(invisible(NULL))
      }

      spec_with_seed <- state$spec
      spec_with_seed$seed <- seed
      state$kanon_acknowledged <- FALSE
      state$kanon_escape_routes <- NULL
      state$generation_count <- (state$generation_count %||% 0L) + 1L
      run_started_at(Sys.time())
      run_seed(seed)
      dg_log(sprintf(
        "generate: starting on %d row(s) x %d column(s), engine=%s",
        nrow(state$raw_data), ncol(state$raw_data),
        spec_with_seed[["engine", exact = TRUE]] %||% "auto"
      ))

      # Background generation needs callr and an installed package (the
      # subprocess can't load a devtools::load_all'd dataganger). The
      # `dataganger.synthesis_async` option also lets tests (and CI) force the
      # deterministic synchronous path, where mocked bindings apply and no
      # subprocess is spawned.
      use_async <- isTRUE(getOption("dataganger.synthesis_async", TRUE)) &&
        rlang::is_installed("callr") &&
        !synthesis_dev_loaded()
      if (!use_async) {
        generating(TRUE)
        return(run_synthesis_sync(spec_with_seed))
      }

      handle <- tryCatch(
        start_synthesis_process(state$raw_data, spec_with_seed, state$roles),
        error = function(e) {
          generate_notification(
            paste("Could not start background synthesis:", conditionMessage(e)),
            type = "error", duration = NULL
          )
          NULL
        }
      )
      if (is.null(handle)) {
        return(invisible(NULL))
      }
      proc(handle)
      generating(TRUE)
      invisible(NULL)
    }

    # Poll the background process; collect its result or surface its error.
    shiny::observe({
      handle <- proc()
      if (is.null(handle)) {
        return()
      }
      shiny::invalidateLater(300, session)
      if (handle$is_alive()) {
        return()
      }
      # Process finished \u2014 collect exactly once.
      proc(NULL)
      generating(FALSE)
      result <- tryCatch(handle$get_result(), error = function(e) {
        generate_notification(
          paste("Synthesis failed:", conditionMessage(e)),
          type = "error", duration = NULL
        )
        NULL
      })
      if (!is.null(result)) {
        dg_log(sprintf(
          "generate: done in %.2fs",
          as.numeric(difftime(Sys.time(), run_started_at() %||% Sys.time(), units = "secs"))
        ))
        finalize_result(result)
      }
    })

    # Cancel: kill the background process and return to a usable state.
    shiny::observeEvent(input$cancel, ignoreNULL = TRUE, {
      handle <- proc()
      if (!is.null(handle) && handle$is_alive()) {
        handle$kill()
        dg_log("generate: cancelled by user")
      }
      proc(NULL)
      generating(FALSE)
      generate_notification("Generation cancelled.", type = "message")
    })

    # Don't leave an orphaned process if the session ends mid-run.
    session$onSessionEnded(function() {
      handle <- shiny::isolate(proc())
      if (!is.null(handle) && handle$is_alive()) {
        handle$kill()
      }
    })

    # Live elapsed-time ticker while generation is running.
    shiny::observe({
      if (!isTRUE(generating())) {
        return()
      }
      shiny::invalidateLater(1000, session)
      started <- run_started_at()
      if (!is.null(started)) {
        elapsed_secs(as.integer(difftime(Sys.time(), started, units = "secs")))
      }
    })

    output$gen_status <- shiny::renderUI({
      if (!isTRUE(generating())) {
        return(NULL)
      }
      secs <- elapsed_secs() %||% 0L
      timer <- sprintf("%02d:%02d", secs %/% 60L, secs %% 60L)
      # Fake-deterministic bar: advances 0\u219290 % over 60 s to give the user
      # a sense of progress; jumps to 100% on completion (handled by hiding).
      pct <- min(90L, as.integer(secs * 90L / 60L))
      shiny::tags$div(
        class = "card gen-status",
        shiny::tags$div(
          class = "gen-status-row",
          shiny::tags$span(class = "spinner"),
          shiny::tags$div(
            style = "flex:1;",
            shiny::tags$div(
              style = "display:flex; align-items:center; justify-content:space-between;",
              shiny::tags$b("Synthesizing\u2026"),
              shiny::tags$span(
                style = "font-family:var(--font-mono); font-size:12px; color:var(--fg-muted);",
                timer
              )
            ),
            shiny::tags$div(
              style = "margin-top:6px; height:4px; background:var(--paper-200); border-radius:2px; overflow:hidden;",
              shiny::tags$div(
                style = sprintf(
                  "height:100%%; width:%d%%; background:var(--synth-600); border-radius:2px; transition:width 0.9s ease;",
                  pct
                )
              )
            ),
            shiny::tags$div(
              style = "font-size:12px; color:var(--fg-muted); margin-top:4px;",
              "The app stays responsive. Use Cancel above to stop a long run."
            )
          )
        )
      )
    })
    shiny::outputOptions(output, "gen_status", suspendWhenHidden = FALSE)

    output$result_stats <- shiny::renderUI({
      shiny::req(state$synthetic)
      dur <- last_duration()
      dur_label <- if (is.null(dur)) "n/a" else sprintf("%.2fs", dur)
      seed_label <- if (is.null(state$seed_used)) "n/a" else as.character(state$seed_used)
      exact_row_matches_n <- attr(state$privacy, "exact_row_matches", exact = TRUE)
      exact_row_matches <- if (is.null(exact_row_matches_n)) "unavailable" else as.character(exact_row_matches_n)
      # Any exact-row match means a synthetic row reproduces a real record
      # verbatim -- a direct disclosure, not just a caution, so it gets the
      # stronger "danger" (red) treatment rather than the amber "risk" one.
      exact_matches_class <- if (!is.null(exact_row_matches_n) && exact_row_matches_n > 0L) {
        "stat danger"
      } else {
        "stat"
      }
      stat_cell <- function(label, value, class = "stat") {
        shiny::tags$div(
          class = class,
          shiny::tags$div(class = "label", label),
          shiny::tags$div(class = "v", value)
        )
      }

      shiny::tags$div(
        class = "result-stats-block",
        shiny::tags$div(
          class = "result-ready",
          shiny::tags$span(class = "result-ready-dot", "\u2713"),
          "Synthetic data generated"
        ),
        shiny::tags$div(
          class = "stats populated",
          stat_cell(
            "ROWS \u00d7 COLS",
            sprintf("%d \u00d7 %d", nrow(state$synthetic), ncol(state$synthetic))
          ),
          stat_cell("SEED", seed_label),
          stat_cell("DURATION", dur_label),
          stat_cell("EXACT MATCHES", exact_row_matches, exact_matches_class)
        )
      )
    })

    output$generate_actions <- shiny::renderUI({
      kanon <- state$kanon %||% attr(state$synthetic, "kanon", exact = TRUE)
      if (is.null(kanon) || !length(kanon$qi_cols %||% character(0))) {
        return(NULL)
      }

      if (isTRUE(kanon$infeasible)) {
        render_kanon_escape_panel(
          session = session,
          kanon = kanon,
          escape_routes = state$kanon_escape_routes %||% list()
        )
      } else {
        render_kanon_applied_panel(kanon)
      }
    })

    shiny::observeEvent(input$generate, ignoreNULL = TRUE, {
      if (isTRUE(generating())) {
        return(invisible(NULL))
      }
      if (is.null(state$raw_data) || is.null(state$spec)) {
        generate_notification("No data or spec available.", type = "warning")
        return(invisible(NULL))
      }

      seed <- if (!is.null(state$spec$seed)) state$spec$seed else sample.int(.Machine$integer.max, 1L)
      run_synthesis(seed)
    })

    shiny::observeEvent(input$try_new_seed, ignoreNULL = TRUE, {
      if (isTRUE(generating())) {
        return(invisible(NULL))
      }
      if (is.null(state$synthetic)) {
        return(invisible(NULL))
      }
      if (is.null(state$raw_data) || is.null(state$spec)) {
        generate_notification("No data or spec available.", type = "warning")
        return(invisible(NULL))
      }

      seed <- sample.int(.Machine$integer.max, 1L)
      run_synthesis(seed)
    })

    shiny::observeEvent(input$apply_escape_k, ignoreNULL = TRUE, {
      escape_routes <- state$kanon_escape_routes %||% list()
      suggested_k <- escape_routes$feasible_k %||% NULL
      current_k <- state$kanon$k %||% state$spec$k_anon %||% 5L
      if (is.null(suggested_k) || isTRUE(generating()) || is.null(state$spec)) {
        return(invisible(NULL))
      }

      state$spec$k_anon <- suggested_k
      state$k_anon <- suggested_k
      state$kanon_next_provenance <- list(
        k_default = current_k,
        k_provenance = "user_selected_after_infeasible"
      )
      run_synthesis(kanon_action_seed(state$spec))
    })

    shiny::observeEvent(input$apply_escape_n, ignoreNULL = TRUE, {
      escape_routes <- state$kanon_escape_routes %||% list()
      suggested_n <- escape_routes$suggested_n %||% NULL
      if (is.null(suggested_n) || isTRUE(generating()) || is.null(state$spec)) {
        return(invisible(NULL))
      }

      state$spec$n <- suggested_n
      state$kanon_next_provenance <- NULL
      run_synthesis(kanon_action_seed(state$spec))
    })

    shiny::observeEvent(input$adjust_settings, ignoreNULL = TRUE, {
      state$nav_request <- "configure"
    })

    shiny::observeEvent(input$adjust_advanced_settings, ignoreNULL = TRUE, {
      state$nav_request <- "configure"
    })

    shiny::observeEvent(input$go_compare, ignoreNULL = TRUE, ignoreInit = TRUE, {
      state$nav_request <- "compare"
    })
  })
}
