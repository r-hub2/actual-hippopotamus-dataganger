#' Internal Shiny Compare Module
#'
#' @keywords internal
#' @noRd
mod_compare_ui <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 05 \u00b7 Compare"),
        shiny::tags$h1("Compare datasets"),
        shiny::tags$div(
          class = "subtitle compare-explainer",
          shiny::tags$p(
            class = "compare-explainer-lead",
            "Click any variable to compare its distribution. ",
            shiny::tags$span(class = "tok-real", "Green"),
            " is your original data; ",
            shiny::tags$span(class = "tok-synth", "magenta"),
            " is the synthetic data."
          ),
          shiny::tags$div(
            class = "compare-explainer-defs",
            shiny::tags$p(
              shiny::tags$strong("\u0394 (delta)"),
              " is the gap between an original and synthetic statistic \u2014 bigger means more drift. ",
              shiny::tags$strong("SMD"),
              " (standardized mean difference) is that \u0394 between the means divided by the original SD \u2014 a scale-free measure of drift."
            ),
            shiny::tags$p(
              shiny::tags$strong("TVD (total variation distance)"),
              " summarises how far two category distributions are apart, from 0 (identical) to 1 (no overlap)."
            ),
            shiny::tags$p(
              class = "compare-explainer-hint",
              "Investigate large \u0394 or TVD values before sharing the data."
            )
          )
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::actionButton(
          ns("go_export"),
          "Continue to Export \u2192",
          class = "btn btn-primary"
        )
      )
    ),
    stale_banner_ui("comparison", ns = ns),
    shiny::uiOutput(ns("compare_body"))
  )
}

# Shared explainer for the effect column: what the colour bands mean and which
# test produced each p-value. `tests` is an optional named character vector of
# "statistic" -> "test name" lines appended below the colour key.
fidelity_legend <- function(tests = NULL) {
  swatch <- function(bg, border, label) {
    shiny::tags$span(
      style = "display:inline-flex; align-items:center; gap:5px; margin-right:14px;",
      shiny::tags$span(style = sprintf(
        "display:inline-block; width:11px; height:11px; border-radius:3px; background:%s; border:1px solid %s;",
        bg, border
      )),
      shiny::tags$span(label)
    )
  }
  shiny::tags$div(
    class = "fidelity-legend",
    style = paste(
      "margin-top:10px; padding:8px 10px; border:1px solid var(--paper-200);",
      "border-radius:4px; background:rgba(251,250,246,0.6);",
      "font-family:var(--font-sans); font-size:11px; line-height:1.7; color:var(--fg-muted);"
    ),
    shiny::tags$div(
      style = "margin-bottom:4px;",
      shiny::tags$strong("Effect colour = how confidently the two distributions differ "),
      "(from the test's p-value):"
    ),
    shiny::tags$div(
      swatch("var(--real-50)", "var(--real-100)", "consistent (p \u2265 0.05)"),
      swatch("var(--risk-50)", "#F2B36A", "some difference (p < 0.05)"),
      swatch("var(--risk-50)", "var(--risk-500)", "strong difference (p < 0.01)"),
      swatch("var(--paper-200)", "var(--paper-200)", "no inference (\u2014)")
    ),
    if (!is.null(tests) && length(tests)) {
      shiny::tags$div(
        style = "margin-top:6px;",
        shiny::tags$strong("Tests: "),
        do.call(shiny::tagList, lapply(seq_along(tests), function(i) {
          shiny::tagList(
            if (i > 1L) " \u00b7 ",
            shiny::tags$span(shiny::tags$b(names(tests)[[i]]), " ", tests[[i]])
          )
        }))
      )
    }
  )
}

compare_numeric_table <- function(num_cmp, orig_vec = NULL, synth_vec = NULL) {
  if (is.null(num_cmp) || nrow(num_cmp) == 0L) {
    return(shiny::tags$p(
      style = "font-family:var(--font-sans); font-size:13px; color:var(--fg-muted); margin-top:8px;",
      "No numeric comparison available."
    ))
  }

  row <- num_cmp[1, , drop = FALSE]

  fmt_val <- function(x) {
    if (length(x) == 0L || is.na(x)) return("\u2014")
    if (abs(x) >= 1000) formatC(x, format = "f", digits = 0, big.mark = ",")
    else sprintf("%.2f", x)
  }

  fidelity_style <- function(band) {
    switch(
      band,
      good = "display:inline-block; padding:2px 8px; border-radius:999px; background:var(--real-50); color:var(--real-700); border:1px solid var(--real-100);",
      warn = "display:inline-block; padding:2px 8px; border-radius:999px; background:var(--risk-50); color:var(--risk-700); border:1px solid #F2B36A;",
      bad = "display:inline-block; padding:2px 8px; border-radius:999px; background:var(--risk-50); color:var(--risk-500); border:1px solid var(--risk-500);",
      "display:inline-block; padding:2px 8px; border-radius:999px; background:var(--paper-200); color:var(--fg-muted); border:1px solid var(--paper-200);"
    )
  }

  effect_cell <- function(value, p, label, infer = TRUE, metric = NULL) {
    band <- if (infer) fidelity_color(p) else "none"
    shiny::tags$span(
      class = paste("fidelity-band", paste0("fidelity-", band)),
      style = fidelity_style(band),
      title = if (infer && !is.na(p)) sprintf("%s p = %.3g", label, p) else label,
      if (!is.null(metric)) {
        shiny::tags$span(
          style = "opacity:0.65; font-size:10px; margin-right:5px; text-transform:uppercase; letter-spacing:.03em;",
          metric
        )
      },
      fmt_val(value)
    )
  }

  min_orig <- if ("min_orig" %in% names(row)) row$min_orig else if (!is.null(orig_vec)) suppressWarnings(min(orig_vec, na.rm = TRUE)) else NA_real_
  min_syn <- if ("min_syn" %in% names(row)) row$min_syn else if (!is.null(synth_vec)) suppressWarnings(min(synth_vec, na.rm = TRUE)) else NA_real_
  max_orig <- if ("max_orig" %in% names(row)) row$max_orig else if (!is.null(orig_vec)) suppressWarnings(max(orig_vec, na.rm = TRUE)) else NA_real_
  max_syn <- if ("max_syn" %in% names(row)) row$max_syn else if (!is.null(synth_vec)) suppressWarnings(max(synth_vec, na.rm = TRUE)) else NA_real_

  fix_inf <- function(x) if (is.infinite(x)) NA_real_ else x

  rows_html <- list(
    shiny::tags$tr(
      shiny::tags$td(class = "name", "Mean"),
      shiny::tags$td(class = "num", fmt_val(row$mean_orig)),
      shiny::tags$td(class = "num", fmt_val(row$mean_syn)),
      shiny::tags$td(class = "num", effect_cell(row$std_diff, row$mean_p, "SMD", metric = "SMD"))
    ),
    shiny::tags$tr(
      shiny::tags$td(class = "name", "SD"),
      shiny::tags$td(class = "num", fmt_val(row$sd_orig)),
      shiny::tags$td(class = "num", fmt_val(row$sd_syn)),
      shiny::tags$td(class = "num", effect_cell(row$sd_ratio, row$sd_p, "SD ratio", metric = "ratio"))
    ),
    shiny::tags$tr(
      shiny::tags$td(class = "name", "Median"),
      shiny::tags$td(class = "num", fmt_val(row$median_orig)),
      shiny::tags$td(class = "num", fmt_val(row$median_syn)),
      shiny::tags$td(class = "num", effect_cell(row$median_std_diff, row$median_p, "Median standardized difference", metric = "diff"))
    ),
    shiny::tags$tr(
      shiny::tags$td(class = "name", "Min"),
      shiny::tags$td(class = "num", fmt_val(fix_inf(min_orig))),
      shiny::tags$td(class = "num", fmt_val(fix_inf(min_syn))),
      shiny::tags$td(class = "num", effect_cell(NA_real_, NA_real_, "No inference", infer = FALSE))
    ),
    shiny::tags$tr(
      shiny::tags$td(class = "name", "Max"),
      shiny::tags$td(class = "num", fmt_val(fix_inf(max_orig))),
      shiny::tags$td(class = "num", fmt_val(fix_inf(max_syn))),
      shiny::tags$td(class = "num", effect_cell(NA_real_, NA_real_, "No inference", infer = FALSE))
    )
  )

  shiny::tagList(
    shiny::tags$table(
      class = "data",
      style = "margin-top:8px;",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("statistic"),
        shiny::tags$th(class = "real", style = "text-align:right;", "original"),
        shiny::tags$th(class = "synth", style = "text-align:right;", "synthetic"),
        shiny::tags$th(style = "text-align:right;", "effect")
      )),
      shiny::tags$tbody(rows_html)
    ),
    fidelity_legend(tests = c(
      "Mean / SMD"    = "Welch t-test",
      "SD / ratio"    = "F-test",
      "Median / diff" = "Wilcoxon rank-sum test"
    ))
  )
}

mod_compare_server <- function(id, state) {
  rlang::check_installed(
    "shiny",
    reason = "to use the DataGangeR Shiny modules"
  )

  shiny::moduleServer(id, function(input, output, session) {
    rlang::check_installed("plotly", reason = "to use the interactive comparison plots")
    selected_var <- shiny::reactiveVal(NULL)
    selected_rel_x <- shiny::reactiveVal(NULL)
    selected_rel_y <- shiny::reactiveVal(NULL)

    empty_plot <- function(label) {
      plotly::plot_ly(type = "scatter", mode = "markers") |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = label, x = 0.5, y = 0.5,
            xref = "paper", yref = "paper", showarrow = FALSE,
            font = list(color = "#6E716A", size = 14)
          )),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        )
    }

    plotly_common <- function(p, showlegend = TRUE) {
      plotly::layout(
        p,
        showlegend = showlegend,
        legend = list(
          orientation = "v", x = 1, y = 1,
          xanchor = "right", yanchor = "top",
          bgcolor = "rgba(251,250,246,0.72)",
          bordercolor = "rgba(0,0,0,0.08)", borderwidth = 1,
          font = list(size = 11)
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 48, r = 16, t = 36, b = 36)
      )
    }

    # Derive the kind used for plot/stats: simulation="drop" > user_role >
    # recommended_role > column class.
    # Logical/boolean is not a distinct kind -- it is treated as categorical.
    # Free text is internally synthesized as categorical (see
    # synth_free_text()'s "categorical" strategy), so it is compared the same
    # way, subject to the same dg_max_comparable_levels() cardinality cap --
    # near-unique free text is excluded by that cap rather than unconditionally.
    # Any ID (alphanumeric ID -- there is no separate pseudo-identifier type
    # any more) is never comparable: its scrambled/dropped values carry no
    # distributional meaning.
    role_to_kind <- function(role) {
      if (is.na(role) || !nzchar(role)) return(NA_character_)
      lc <- tolower(role)
      if (grepl("alphanumeric", lc)) return("identifier")
      if (grepl("id\\b|identifier", lc)) return("identifier")
      if (grepl("categor", lc)) return("categorical")
      if (grepl("\\bdate\\b", lc)) return("date")
      if (grepl("logic|boolean", lc)) return("categorical")
      if (grepl("free.text|free_text", lc)) return("categorical")
      if (grepl("geograph", lc)) return("categorical")
      if (grepl("numeric", lc)) return("numeric")
      if (grepl("drop", lc)) return("drop")
      role
    }

    eff_kind <- function(var, roles, col_data) {
      if (!is.null(roles)) {
        idx <- match(var, roles$variable)
        if (!is.na(idx)) {
          # An explicit "drop" action is authoritative regardless of what the
          # type dropdown says -- "drop" only lives in Action override now.
          if ("simulation" %in% names(roles) &&
              identical(roles$simulation[[idx]], "drop")) {
            return("drop")
          }
          ur  <- roles$user_role[[idx]]
          rec <- if ("recommended_role" %in% names(roles)) roles$recommended_role[[idx]] else NA_character_
          kind_from_user <- role_to_kind(ur)
          if (!is.na(kind_from_user)) return(kind_from_user)
          # Map recommended_role text to a kind
          kind_from_rec <- role_to_kind(rec)
          if (!is.na(kind_from_rec)) return(kind_from_rec)
        }
      }
      # Fall back to actual column class
      if (is.null(col_data)) return("numeric")
      if (is.logical(col_data))                            return("categorical")
      if (inherits(col_data, c("Date", "POSIXct", "POSIXt"))) return("date")
      if (is.character(col_data) || is.factor(col_data))   return("categorical")
      "numeric"
    }

    output$stale__comparison <- shiny::renderText({
      if (isTRUE(state$stale$comparison)) "true" else "false"
    })
    shiny::outputOptions(output, "stale__comparison", suspendWhenHidden = FALSE)

    comparable_vars <- shiny::reactive({
      shiny::req(state$raw_data)
      vars <- intersect(names(state$raw_data), names(state$synthetic %||% state$raw_data))
      roles <- state$roles
      kinds <- stats::setNames(
        vapply(vars, function(v) eff_kind(v, roles, state$raw_data[[v]]), character(1)),
        vars
      )
      vars[!kinds %in% c("identifier")]
    })

    shiny::observe({
      vars <- comparable_vars()
      shiny::req(length(vars) > 0L)
      if (is.null(selected_var()) || !(selected_var() %in% vars)) {
        selected_var(vars[[1L]])
      }
    })

    shiny::observeEvent(input$var_select, ignoreNULL = TRUE, {
      selected_var(input$var_select)
    })

    shiny::observeEvent(input$rel_x, ignoreNULL = TRUE, {
      selected_rel_x(input$rel_x)
    })

    shiny::observeEvent(input$rel_y, ignoreNULL = TRUE, {
      selected_rel_y(input$rel_y)
    })

    # Effective selected variable, derived synchronously from comparable_vars()
    # rather than relying on the observe above having fired. On first transition
    # into Compare the observe can run *after* the renderers paint, leaving
    # selected_var() NULL/stale and the first variable showing the wrong (full)
    # table until the user clicks another tab. Falling back to the first
    # comparable variable here removes that race.
    current_var <- shiny::reactive({
      vars <- comparable_vars()
      shiny::req(length(vars) > 0L)
      sel <- selected_var()
      if (!is.null(sel) && sel %in% vars) sel else vars[[1L]]
    })

    current_rel_vars <- shiny::reactive({
      vars <- comparable_vars()
      if (length(vars) < 2L) return(c(NA_character_, NA_character_))
      x <- selected_rel_x()
      y <- selected_rel_y()
      if (is.null(x) || !(x %in% vars)) x <- vars[[1L]]
      if (is.null(y) || !(y %in% vars)) {
        y <- setdiff(vars, x)[[1L]]
      }
      c(x, y)
    })

    shiny::observe({
      vars <- comparable_vars()
      if (length(vars) >= 2L) {
        pair <- current_rel_vars()
        selected_rel_x(pair[[1L]])
        selected_rel_y(pair[[2L]])
      }
    })

    shiny::observe({
      vars <- comparable_vars()
      state$compare_selected_var <- if (length(vars) > 0L) current_var() else selected_var()
    })

    shiny::observeEvent(input$go_export, ignoreNULL = TRUE, {
      state$nav_request <- "export"
    })

    output$compare_body <- shiny::renderUI({
      if (is.null(state$synthetic) || is.null(state$raw_data)) {
        return(shiny::tags$div(
          class = "card",
          shiny::tags$p(
            style = "font-family:var(--font-sans); font-size:13px; color:var(--fg-muted); margin:0;",
            "Generate synthetic data first to see a comparison."
          )
        ))
      }

      vars    <- comparable_vars()
      roles   <- state$roles
      current <- if (length(vars) > 0L) current_var() else NULL

      if (length(vars) == 0L) {
        return(shiny::tags$div(
          class = "card",
          shiny::tags$p(
            style = "font-family:var(--font-sans); font-size:13px; color:var(--fg-muted); margin:0;",
            "No comparable variables remain after excluding ID and dropped columns."
          )
        ))
      }

      kind_map <- stats::setNames(
        vapply(vars, function(v) eff_kind(v, roles, state$raw_data[[v]]), character(1)),
        vars
      )

      rail_btns <- lapply(vars, function(v) {
        kind     <- kind_map[[v]]
        kind_lbl <- switch(kind,
          numeric     = "num",
          categorical = "cat",
          date        = "date",
          kind
        )
        is_active <- identical(v, current)
        # JS-escape the variable name so column names with quotes/backslashes
        # don't break the inline onclick handler
        v_js <- gsub("\\\\", "\\\\\\\\", v)
        v_js <- gsub("'",    "\\\\'",    v_js, fixed = TRUE)
        shiny::tags$button(
          class   = paste0("var-tab", if (is_active) " active" else ""),
          title   = v,
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            session$ns("var_select"),
            v_js
          ),
          shiny::tags$span(class = "var-name", v),
          shiny::tags$span(class = paste0("var-kind k-", kind), kind_lbl)
        )
      })

      var_detail <- shiny::tags$div(
        class = "var-detail",
        shiny::tags$div(
          class = "var-detail-header",
          shiny::tags$h3(class = "var-title", current %||% ""),
          shiny::tags$span(
            style = "font-family:var(--font-mono); font-size:11px; padding:2px 8px; background:var(--paper-200); border-radius:2px; color:var(--fg-muted);",
            if (!is.null(current) && current %in% names(kind_map)) kind_map[[current]] else "numeric"
          )
        ),
        plotly::plotlyOutput(session$ns("var_plot"), height = "360px"),
        shiny::uiOutput(session$ns("var_stats"))
      )

      univariate <- shiny::tags$div(
        class = "compare-layout compare-layout-tabs",
        shiny::tags$div(
          class = "var-rail var-tab-nav",
          shiny::tags$div(class = "var-rail-eyebrow", paste0("Variables \u00b7 ", length(vars))),
          shiny::tags$div(class = "var-matrix", rail_btns)
        ),
        var_detail
      )

      pair <- if (length(vars) >= 2L) current_rel_vars() else c(NA_character_, NA_character_)
      bivariate_body <- if (length(vars) < 2L) {
        shiny::tags$div(
          class = "card",
          shiny::tags$p(
            style = "font-family:var(--font-sans); font-size:13px; color:var(--fg-muted); margin:0;",
            "Pick two different variables"
          )
        )
      } else {
        shiny::tags$div(
          class = "card",
          shiny::tags$div(
            style = "display:flex; gap:16px; flex-wrap:wrap; margin-bottom:12px;",
            shiny::tags$div(
              style = "flex:1 1 220px;",
              shiny::selectInput(session$ns("rel_x"), "Predictor (X)", choices = vars, selected = pair[[1L]])
            ),
            shiny::tags$div(
              style = "flex:1 1 220px;",
              shiny::selectInput(session$ns("rel_y"), "Outcome (Y)", choices = vars, selected = pair[[2L]])
            )
          ),
          plotly::plotlyOutput(session$ns("rel_plot"), height = "360px"),
          shiny::uiOutput(session$ns("rel_stats"))
        )
      }

      section_heading <- function(title, description) {
        shiny::tags$div(
          style = "margin:22px 0 10px;",
          shiny::tags$h2(
            style = "font-family:var(--font-mono); font-size:16px; letter-spacing:.02em; margin:0 0 4px;",
            title
          ),
          shiny::tags$p(
            style = "font-family:var(--font-sans); font-size:12px; color:var(--fg-muted); margin:0;",
            description
          )
        )
      }

      shiny::tagList(
        shiny::tags$div(
          class = "banner info compare-exploratory-note",
          shiny::tags$span(class = "icon", "i"),
          shiny::tags$div(
            shiny::tags$b("Exploratory comparison"),
            paste(
              "These summaries highlight possible differences between the original and synthetic data.",
              "For deeper analysis, download the synthetic data and apply methods suited to your use case."
            )
          )
        ),
        section_heading("Univariate", "Compare each variable's original and synthetic marginal distribution."),
        univariate,
        section_heading("Bivariate", "Compare how a two-variable relationship is preserved."),
        bivariate_body
      )
    })

    output$var_plot <- plotly::renderPlotly({
      shiny::req(state$raw_data, state$synthetic, current_var())
      var   <- current_var()
      roles <- state$roles
      orig  <- state$raw_data
      synth <- state$synthetic
      shiny::req(var %in% names(orig), var %in% names(synth))

      kind <- eff_kind(var, roles, orig[[var]])

      explicit_missing <- function(x) {
        vals <- as.character(x)
        vals[is.na(vals)] <- "(Missing)"
        vals
      }

      prop_by_level <- function(vals, lvls) {
        vapply(lvls, function(l) mean(vals == l), numeric(1))
      }

      if (kind == "drop") {
        return(empty_plot(paste0(kind, " \u2014 no distribution plot")))
      }

      if (kind == "categorical") {
        orig_vals  <- explicit_missing(orig[[var]])
        synth_vals <- explicit_missing(synth[[var]])
        lvls <- sort(unique(c(orig_vals, synth_vals)))

        max_levels <- dg_max_comparable_levels(nrow(orig))
        if (length(lvls) > max_levels) {
          return(empty_plot(sprintf(
            "%d distinct values \u2014 too many to compare reliably for %s row%s (limit %d)",
            length(lvls), format(nrow(orig), big.mark = ","),
            if (nrow(orig) == 1L) "" else "s", max_levels
          )))
        }

        orig_prop  <- prop_by_level(orig_vals, lvls)
        synth_prop <- prop_by_level(synth_vals, lvls)
        dat <- data.frame(
          level = lvls,
          original = orig_prop,
          synthetic = synth_prop,
          stringsAsFactors = FALSE
        )
        p <- plotly::plot_ly(dat, y = ~level) |>
          plotly::add_bars(
            x = ~original,
            name = "Original",
            orientation = "h",
            marker = list(color = "#4F7D32")
          ) |>
          plotly::add_bars(
            x = ~synthetic,
            name = "Synthetic",
            orientation = "h",
            marker = list(color = "#D43A8A")
          ) |>
          plotly::layout(
            barmode = "group",
            xaxis = list(title = "proportion", tickformat = ".0%"),
            yaxis = list(title = "")
          )
        return(plotly_common(p))

      } else if (kind == "date") {
        orig_vec  <- orig[[var]][!is.na(orig[[var]])]
        synth_vec <- synth[[var]][!is.na(synth[[var]])]
        if (length(orig_vec) == 0L || length(synth_vec) == 0L) {
          return(empty_plot("No non-missing dates to plot"))
        }
        p <- plotly::plot_ly() |>
          plotly::add_histogram(
            x = orig_vec,
            name = "Original",
            marker = list(color = "#4F7D32"),
            opacity = 0.65
          ) |>
          plotly::add_histogram(
            x = synth_vec,
            name = "Synthetic",
            marker = list(color = "#D43A8A"),
            opacity = 0.65
          ) |>
          plotly::layout(
            barmode = "overlay",
            xaxis = list(title = var),
            yaxis = list(title = "count")
          )
        return(plotly_common(p))

      } else {
        orig_vec  <- as.numeric(orig[[var]])
        synth_vec <- as.numeric(synth[[var]])
        orig_vec  <- orig_vec[!is.na(orig_vec)]
        synth_vec <- synth_vec[!is.na(synth_vec)]
        if (length(orig_vec) == 0L || length(synth_vec) == 0L) {
          return(empty_plot("No non-missing numeric values to plot"))
        }

        probs <- seq(0, 1, length.out = min(101L, max(length(orig_vec), length(synth_vec))))
        qdat <- data.frame(
          original = as.numeric(stats::quantile(orig_vec, probs = probs, na.rm = TRUE, names = FALSE)),
          synthetic = as.numeric(stats::quantile(synth_vec, probs = probs, na.rm = TRUE, names = FALSE))
        )
        lims <- range(c(qdat$original, qdat$synthetic), finite = TRUE)

        qq <- plotly::plot_ly(qdat, x = ~original, y = ~synthetic) |>
          plotly::add_markers(
            name = "QQ quantiles",
            marker = list(color = "#D43A8A", size = 6, opacity = 0.78)
          ) |>
          plotly::add_lines(
            x = lims,
            y = lims,
            name = "Perfect match",
            line = list(color = "#4F7D32", dash = "dash"),
            inherit = FALSE
          ) |>
          plotly::layout(
            title = "QQ plot: original vs synthetic quantiles",
            xaxis = list(title = "Original quantiles"),
            yaxis = list(title = "Synthetic quantiles")
          )

        hist <- plotly::plot_ly() |>
          plotly::add_histogram(
            x = orig_vec,
            name = "Original",
            marker = list(color = "#4F7D32"),
            opacity = 0.65
          ) |>
          plotly::add_histogram(
            x = synth_vec,
            name = "Synthetic",
            marker = list(color = "#D43A8A"),
            opacity = 0.65
          ) |>
          plotly::layout(
            title = "Histogram overlay",
            barmode = "overlay",
            xaxis = list(title = var),
            yaxis = list(title = "count")
          )

        p <- plotly::subplot(qq, hist, nrows = 2, shareX = FALSE, titleY = TRUE, margin = 0.08)
        return(plotly_common(p))
      }
    })

    output$var_stats <- shiny::renderUI({
      shiny::req(state$raw_data, state$synthetic, current_var())
      var   <- current_var()
      roles <- state$roles
      orig  <- state$raw_data
      synth <- state$synthetic
      shiny::req(var %in% names(orig), var %in% names(synth))

      kind <- eff_kind(var, roles, orig[[var]])

      if (kind %in% c("identifier", "drop")) {
        return(shiny::tags$p(
          style = "font-family:var(--font-sans); font-size:13px; color:var(--fg-muted); margin-top:8px;",
          paste0("Role is '", kind, "' \u2014 this column is excluded from distribution comparison.")
        ))
      }

      explicit_missing <- function(x) {
        vals <- as.character(x)
        vals[is.na(vals)] <- "(Missing)"
        vals
      }

      prop_by_level <- function(vals, lvls) {
        vapply(lvls, function(l) mean(vals == l), numeric(1))
      }

      fmt_pct <- function(x) sprintf("%.0f%%", 100 * x)

      if (kind == "categorical") {
        orig_vals  <- explicit_missing(orig[[var]])
        synth_vals <- explicit_missing(synth[[var]])
        lvls <- sort(unique(c(orig_vals, synth_vals)))

        max_levels <- dg_max_comparable_levels(nrow(orig))
        if (length(lvls) > max_levels) {
          return(shiny::tags$div(
            class = "banner risk",
            style = "margin-top:8px;",
            shiny::tags$span(class = "icon", "!"),
            shiny::tags$div(
              shiny::tags$b(sprintf("%d distinct values.", length(lvls))),
              sprintf(
                " Too many to compare reliably for %s rows (limit %d) \u2014 excluded from this page.",
                format(nrow(orig), big.mark = ","), max_levels
              )
            )
          ))
        }

        orig_prop  <- prop_by_level(orig_vals, lvls)
        synth_prop <- prop_by_level(synth_vals, lvls)
        tvd  <- 0.5 * sum(abs(orig_prop - synth_prop))
        p    <- safe_categorical_p(orig_vals, synth_vals, lvls)
        band <- fidelity_color(p)
        band_bg <- switch(band,
          good = "var(--real-50)", warn = "var(--risk-50)",
          bad = "var(--risk-50)", "var(--paper-200)")
        band_border <- switch(band,
          good = "var(--real-100)", warn = "#F2B36A",
          bad = "var(--risk-500)", "var(--paper-200)")
        band_fg <- switch(band,
          good = "var(--real-700)", warn = "var(--risk-700)",
          bad = "var(--risk-500)", "var(--fg-muted)")
        band_note <- switch(band,
          good = " \u00b7 distributions consistent (p \u2265 0.05)",
          warn = " \u00b7 some difference (p < 0.05) \u2014 review",
          bad  = " \u00b7 strong difference (p < 0.01) \u2014 review",
          " \u00b7 no inference available")
        p_txt <- if (is.na(p)) "p = \u2014" else sprintf("p = %.3g", p)
        shiny::tagList(
          shiny::tags$div(
            style = sprintf(
              "margin-top:12px; padding:8px 12px; background:%s; border:1px solid %s; border-radius:4px; font-family:var(--font-sans); font-size:13px;",
              band_bg, band_border
            ),
            shiny::tags$b(
              style = sprintf("font-family:var(--font-mono); color:%s;", band_fg),
              sprintf("%s \u00b7 TVD = %.3f", p_txt, tvd)
            ),
            band_note
          ),
          fidelity_legend(tests = c(
            "Category counts" = "chi-square test (Fisher's exact test when cells are sparse)"
          ))
        )

      } else if (kind == "date") {
        orig_vec  <- orig[[var]]
        synth_vec <- synth[[var]]
        date_summary <- function(x) {
          non_missing <- x[!is.na(x)]
          missing_prop <- mean(explicit_missing(x) == "(Missing)")
          if (length(non_missing) == 0L) {
            return(list(min = "\u2014", max = "\u2014", span = "\u2014", missing = fmt_pct(missing_prop)))
          }
          min_val <- min(non_missing, na.rm = TRUE)
          max_val <- max(non_missing, na.rm = TRUE)
          span_str <- tryCatch(
            as.character(as.integer(difftime(max_val, min_val, units = "days"))),
            error = function(e) "\u2014"
          )
          list(
            min = as.character(min_val),
            max = as.character(max_val),
            span = span_str,
            missing = fmt_pct(missing_prop)
          )
        }
        orig_sum <- date_summary(orig_vec)
        synth_sum <- date_summary(synth_vec)
        shiny::tags$table(
          class = "data",
          style = "margin-top:8px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th(""),
            shiny::tags$th(class = "real",  "original"),
            shiny::tags$th(class = "synth", "synthetic")
          )),
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "name", "min"),
              shiny::tags$td(orig_sum$min),
              shiny::tags$td(synth_sum$min)
            ),
            shiny::tags$tr(
              shiny::tags$td(class = "name", "max"),
              shiny::tags$td(orig_sum$max),
              shiny::tags$td(synth_sum$max)
            ),
            shiny::tags$tr(
              shiny::tags$td(class = "name", "span (days)"),
              shiny::tags$td(class = "num", orig_sum$span),
              shiny::tags$td(class = "num", synth_sum$span)
            ),
            shiny::tags$tr(
              shiny::tags$td(class = "name", "(Missing)"),
              shiny::tags$td(class = "num", orig_sum$missing),
              shiny::tags$td(class = "num", synth_sum$missing)
            )
          )
        )

      } else {
        orig_vec  <- as.numeric(orig[[var]])
        synth_vec <- as.numeric(synth[[var]])
        num_cmp <- compare_numeric(
          stats::setNames(data.frame(orig_vec), var),
          stats::setNames(data.frame(synth_vec), var)
        )
        compare_numeric_table(num_cmp, orig_vec = orig_vec, synth_vec = synth_vec)
      }
    })

    output$rel_plot <- plotly::renderPlotly({
      shiny::req(state$raw_data, state$synthetic)
      vars <- comparable_vars()
      if (length(vars) < 2L) return(empty_plot("Pick two different variables"))

      pair <- current_rel_vars()
      var_x <- pair[[1L]]
      var_y <- pair[[2L]]
      orig <- state$raw_data
      synth <- state$synthetic
      if (is.na(var_x) || is.na(var_y) || identical(var_x, var_y) ||
          !all(c(var_x, var_y) %in% names(orig)) ||
          !all(c(var_x, var_y) %in% names(synth))) {
        return(empty_plot("Pick two different variables"))
      }

      kind_x <- eff_kind(var_x, state$roles, orig[[var_x]])
      kind_y <- eff_kind(var_y, state$roles, orig[[var_y]])
      normalize_kind <- function(kind) {
        if (kind == "date") return("numeric")
        kind
      }
      plot_kind_x <- normalize_kind(kind_x)
      plot_kind_y <- normalize_kind(kind_y)
      if (!plot_kind_x %in% c("numeric", "categorical") ||
          !plot_kind_y %in% c("numeric", "categorical")) {
        return(empty_plot("These variable roles do not support relationship plotting"))
      }

      pair_frame <- function(dat) {
        data.frame(x = dat[[var_x]], y = dat[[var_y]], stringsAsFactors = FALSE)
      }
      orig_pair <- pair_frame(orig)
      synth_pair <- pair_frame(synth)
      orig_pair <- orig_pair[stats::complete.cases(orig_pair), , drop = FALSE]
      synth_pair <- synth_pair[stats::complete.cases(synth_pair), , drop = FALSE]
      if (nrow(orig_pair) == 0L || nrow(synth_pair) == 0L) {
        return(empty_plot("No complete variable pairs to plot"))
      }

      panel_title <- function(p, title) plotly::layout(p, title = list(text = title, font = list(size = 13)))

      if (plot_kind_x == "numeric" && plot_kind_y == "numeric") {
        make_scatter <- function(dat, title, color) {
          dat$x <- as.numeric(dat$x)
          dat$y <- as.numeric(dat$y)
          smooth <- tryCatch({
            if (length(unique(dat$x)) < 4L) stop("too few distinct x values")
            fit <- stats::loess(y ~ x, data = dat)
            grid <- seq(min(dat$x), max(dat$x), length.out = 100L)
            pred <- stats::predict(fit, newdata = data.frame(x = grid), se = TRUE)
            keep <- is.finite(pred$fit) & is.finite(pred$se.fit)
            data.frame(
              x = grid[keep], fit = pred$fit[keep],
              lower = pred$fit[keep] - 1.96 * pred$se.fit[keep],
              upper = pred$fit[keep] + 1.96 * pred$se.fit[keep]
            )
          }, error = function(e) NULL)
          p <- plotly::plot_ly(dat, x = ~x, y = ~y) |>
            plotly::add_markers(
              marker = list(color = color, opacity = 0.5, size = 7),
              hovertemplate = paste0(var_x, ": %{x}<br>", var_y, ": %{y}<extra></extra>")
            )
          if (!is.null(smooth) && nrow(smooth) > 1L) {
            p <- p |>
              plotly::add_ribbons(
                data = smooth, x = ~x, ymin = ~lower, ymax = ~upper,
                fillcolor = paste0(color, "26"), line = list(color = "transparent"),
                name = "LOESS 95% CI", hoverinfo = "skip", showlegend = FALSE,
                inherit = FALSE
              ) |>
              plotly::add_lines(
                data = smooth, x = ~x, y = ~fit,
                line = list(color = color, width = 2), name = "LOESS smooth",
                hovertemplate = "LOESS: %{y:.3g}<extra></extra>",
                showlegend = FALSE, inherit = FALSE
              )
          }
          panel_title(
            p |>
              plotly::layout(
                xaxis = list(title = var_x),
                yaxis = list(title = var_y)
              ),
            title
          )
        }
        p_orig <- make_scatter(orig_pair, "Original", "#4F7D32")
        p_synth <- make_scatter(synth_pair, "Synthetic", "#D43A8A")

      } else if (plot_kind_x == "categorical" && plot_kind_y == "categorical") {
        levels_x <- sort(unique(c(as.character(orig_pair$x), as.character(synth_pair$x))))
        levels_y <- sort(unique(c(as.character(orig_pair$y), as.character(synth_pair$y))))
        if (length(levels_x) < 2L || length(levels_y) < 2L) {
          return(empty_plot("Both categorical variables need at least two levels"))
        }
        max_levels <- dg_max_comparable_levels(nrow(orig))
        if (length(levels_x) > max_levels || length(levels_y) > max_levels) {
          return(empty_plot(sprintf(
            "Too many distinct values to compare reliably for %s rows (limit %d)",
            format(nrow(orig), big.mark = ","), max_levels
          )))
        }
        heat_data <- function(dat) {
          tab <- table(
            factor(as.character(dat$x), levels = levels_x),
            factor(as.character(dat$y), levels = levels_y)
          )
          props <- sweep(tab, 2L, colSums(tab), "/")
          props[!is.finite(props)] <- 0
          t(props)
        }
        z_orig <- heat_data(orig_pair)
        z_synth <- heat_data(synth_pair)
        zmax <- max(c(z_orig, z_synth), na.rm = TRUE)
        make_heatmap <- function(z, title, color) {
          panel_title(
            plotly::plot_ly(
              x = levels_x, y = levels_y, z = z,
              type = "heatmap", zmin = 0, zmax = zmax,
              colorscale = list(c(0, "#FBFAF6"), c(1, color)),
              showscale = FALSE,
              hovertemplate = paste0(var_x, ": %{x}<br>", var_y, ": %{y}<br>column proportion: %{z:.1%}<extra></extra>")
            ) |>
              plotly::layout(
                xaxis = list(title = var_x),
                yaxis = list(title = var_y)
              ),
            title
          )
        }
        p_orig <- make_heatmap(z_orig, "Original", "#4F7D32")
        p_synth <- make_heatmap(z_synth, "Synthetic", "#D43A8A")

      } else {
        numeric_is_x <- plot_kind_x == "numeric"
        numeric_name <- if (numeric_is_x) var_x else var_y
        category_name <- if (numeric_is_x) var_y else var_x
        prepare_mixed <- function(dat) {
          data.frame(
            category = as.character(if (numeric_is_x) dat$y else dat$x),
            value = as.numeric(if (numeric_is_x) dat$x else dat$y),
            stringsAsFactors = FALSE
          )
        }
        orig_mixed <- prepare_mixed(orig_pair)
        synth_mixed <- prepare_mixed(synth_pair)
        categories <- sort(unique(c(orig_mixed$category, synth_mixed$category)))
        if (length(categories) < 2L) return(empty_plot("The categorical variable needs at least two groups"))
        max_levels <- dg_max_comparable_levels(nrow(orig))
        if (length(categories) > max_levels) {
          return(empty_plot(sprintf(
            "Too many distinct values to compare reliably for %s rows (limit %d)",
            format(nrow(orig), big.mark = ","), max_levels
          )))
        }
        make_box <- function(dat, title, color) {
          dat$category <- factor(dat$category, levels = categories)
          panel_title(
            plotly::plot_ly(dat, x = ~category, y = ~value) |>
              plotly::add_boxplot(
                marker = list(color = color), line = list(color = color),
                fillcolor = color, opacity = 0.55,
                boxpoints = "outliers", name = title,
                hovertemplate = paste0(category_name, ": %{x}<br>", numeric_name, ": %{y}<extra></extra>")
              ) |>
              plotly::layout(
                xaxis = list(title = category_name),
                yaxis = list(title = numeric_name)
              ),
            title
          )
        }
        p_orig <- make_box(orig_mixed, "Original", "#4F7D32")
        p_synth <- make_box(synth_mixed, "Synthetic", "#D43A8A")
      }

      combined <- plotly::subplot(
        p_orig, p_synth, nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.08
      )
      plotly_common(combined, showlegend = FALSE)
    })

    output$rel_stats <- shiny::renderUI({
      shiny::req(state$raw_data, state$synthetic)
      vars <- comparable_vars()
      if (length(vars) < 2L) return(NULL)
      pair <- current_rel_vars()
      var_x <- pair[[1L]]
      var_y <- pair[[2L]]
      if (is.na(var_x) || is.na(var_y) || identical(var_x, var_y) ||
          !all(c(var_x, var_y) %in% names(state$raw_data)) ||
          !all(c(var_x, var_y) %in% names(state$synthetic))) return(NULL)

      kind_x <- eff_kind(var_x, state$roles, state$raw_data[[var_x]])
      kind_y <- eff_kind(var_y, state$roles, state$raw_data[[var_y]])
      result <- relationship_interaction(
        state$raw_data[[var_x]], state$raw_data[[var_y]],
        state$synthetic[[var_x]], state$synthetic[[var_y]],
        kind_x, kind_y
      )
      band <- fidelity_color(result$p_value)
      band_style <- switch(
        band,
        good = "background:var(--real-50); color:var(--real-700); border:1px solid var(--real-100);",
        warn = "background:var(--risk-50); color:var(--risk-700); border:1px solid #F2B36A;",
        bad = "background:var(--risk-50); color:var(--risk-500); border:1px solid var(--risk-500);",
        "background:var(--paper-200); color:var(--fg-muted); border:1px solid var(--paper-200);"
      )
      effect_text <- if (is.na(result$estimate)) {
        sprintf("Joint interaction across %d terms", result$n_terms)
      } else {
        estimate <- if (result$family == "continuous") {
          sprintf("%+.3f", result$estimate)
        } else {
          sprintf("%.3f", result$estimate)
        }
        sprintf("%s %s (null %g)", result$effect_label, estimate, result$null_value)
      }
      is_correlation <- identical(result$effect_label, "Difference in correlation")
      p_label <- if (is_correlation) "Correlation difference p-value" else "Interaction p-value"
      p_text <- if (is.na(result$p_value)) paste0(p_label, " = \u2014") else
        sprintf("%s = %.3g", p_label, result$p_value)
      model_name <- switch(
        result$family,
        binary = "Logistic",
        count = "Poisson",
        continuous = "Gaussian",
        multinomial = "Multinomial"
      )
      note_text <- if (nzchar(result$note)) {
        paste0(" \u00b7 not estimable \u2014 ", result$note)
      } else {
        ""
      }

      shiny::tagList(
        shiny::tags$div(
          class = paste("fidelity-band", paste0("fidelity-", band)),
          style = paste("margin-top:10px; padding:8px 12px; border-radius:4px;", band_style),
          shiny::tags$strong(effect_text),
          shiny::tags$span(style = "margin-left:12px;", p_text)
        ),
        shiny::tags$p(
          style = "font-family:var(--font-sans); font-size:11px; color:var(--fg-muted); margin:6px 0 0;",
          paste0(
            if (is_correlation) "Fisher correlation comparison" else
              paste0(model_name, " interaction Y ~ X \u00d7 synthetic"),
            note_text
          )
        ),
        fidelity_legend(tests = c(
          "Relationship" = if (is_correlation) {
            "Fisher comparison of original and synthetic Pearson correlations"
          } else {
            "joint interaction test comparing Y ~ X + synthetic with Y ~ X * synthetic"
          }
        ))
      )
    })

    invisible(NULL)
  })
}
