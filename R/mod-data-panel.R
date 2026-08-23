#' Internal Shiny Data Panel Module
#'
#' @keywords internal
#' @noRd
mod_data_panel_ui <- function(id) {
  rlang::check_installed(
    c("shiny", "DT"),
    reason = "to use the DataGangeR Shiny modules"
  )
  ns <- shiny::NS(id)

  shiny::tags$aside(
    class = "data-panel",
    id    = ns("panel"),
    shiny::tags$div(
      class = "dp-header",
      shiny::tags$div(
        class = "dp-title",
        shiny::tags$span(class = "dp-eyebrow", "Data preview"),
        shiny::uiOutput(ns("dp_name"))
      ),
      shiny::uiOutput(ns("dp_tabs"))
    ),
    shiny::uiOutput(ns("dp_body"))
  )
}

#' @keywords internal
#' @noRd
mod_data_panel_server <- function(id, state) {
  rlang::check_installed(
    c("shiny", "DT"),
    reason = "to use the DataGangeR Shiny modules"
  )

  shiny::moduleServer(id, function(input, output, session) {
    active_tab <- shiny::reactiveVal("real")

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

    shiny::observeEvent(input$active_tab, ignoreNULL = TRUE, {
      active_tab(input$active_tab)
    })

    shiny::observeEvent(state$synthetic, ignoreNULL = TRUE, {
      active_tab("synth")
    })

    shiny::observeEvent(state$raw_data, ignoreNULL = TRUE, {
      active_tab("real")
    })

    # Per-row exact-match detail: which rows are reproduced verbatim, at what
    # severity (1 = match, 2 = match exposing a value the user marked sensitive
    # in question 2), plus the per-column breakdown behind the Exact matches
    # tab. Memoised: recomputed only when the data or roles change, not on every
    # tab switch or table page. NULL until synthetic data exists. Uses the same
    # role_map (recommended_role) and original-name alignment as the EXACT
    # MATCHES stat so the highlight and the count always agree.
    exact_match_detail_r <- shiny::reactive({
      orig <- state$raw_data
      syn <- state$synthetic
      if (is.null(orig) || is.null(syn)) {
        return(NULL)
      }
      roles <- state$generated_roles %||% state$roles
      role_map <- NULL
      if (!is.null(roles) && "variable" %in% names(roles) &&
          "recommended_role" %in% names(roles)) {
        role_map <- stats::setNames(roles$recommended_role, roles$variable)
      }
      exact_match_detail(orig, dg_original_names(syn), roles, role_map)
    })

    output$dp_name <- shiny::renderUI({
      if (is.null(state$raw_data)) {
        return(shiny::tags$span(
          class = "dp-name",
          style = "color:var(--fg-subtle)",
          "\u2014"
        ))
      }
      nm <- if (!is.null(state$filename)) state$filename else "dataset"
      shiny::tags$span(class = "dp-name", nm)
    })

    output$dp_tabs <- shiny::renderUI({
      # On the compare step the data panel shows a two-column comparison
      # table (item 11), not the real/synth tabs - suppress them entirely.
      if (identical(state$active_step, "compare") &&
          !is.null(state$synthetic) &&
          !is.null(state$compare_selected_var)) {
        return(NULL)
      }

      has_synth <- !is.null(state$synthetic)
      tab       <- active_tab()

      real_active  <- !tab %in% c("synth", "matches")
      synth_active <- tab == "synth"

      real_btn <- shiny::tags$button(
        id      = session$ns("tab_real"),
        class   = paste0("dp-tab real", if (real_active) " active" else ""),
        onclick = sprintf(
          "Shiny.setInputValue('%s', 'real', {priority:'event'})",
          session$ns("active_tab")
        ),
        shiny::tags$span(class = "dot"),
        "Original"
      )

      synth_lbl <- if (!has_synth) "Synthetic \u2014 pending" else "Synthetic"
      synth_class <- paste0(
        "dp-tab synth",
        if (synth_active && has_synth) " active" else "",
        if (!has_synth) " disabled" else ""
      )
      synth_onclick <- if (has_synth) {
        sprintf(
          "Shiny.setInputValue('%s', 'synth', {priority:'event'})",
          session$ns("active_tab")
        )
      } else {
        "void(0)"
      }
      synth_btn <- shiny::tags$button(
        id      = session$ns("tab_synth"),
        class   = synth_class,
        onclick = synth_onclick,
        shiny::tags$span(class = "dot"),
        synth_lbl
      )

      # Exact matches tab: only meaningful once synthetic data exists, and only
      # shown when there is something to look at, so a clean run does not carry
      # a permanently empty tab.
      detail <- exact_match_detail_r()
      n_match <- if (is.null(detail)) 0L else sum(detail$synthetic_severity > 0L)
      matches_btn <- NULL
      if (has_synth && n_match > 0L) {
        n_red <- exact_match_sensitive_count(detail)
        matches_btn <- shiny::tags$button(
          id      = session$ns("tab_matches"),
          class   = paste0(
            "dp-tab matches",
            if (identical(tab, "matches")) " active" else "",
            if (n_red > 0L) " danger" else ""
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', 'matches', {priority:'event'})",
            session$ns("active_tab")
          ),
          shiny::tags$span(class = "dot"),
          sprintf("Exact matches (%d)", n_match)
        )
      }

      shiny::tags$div(
        class = "dp-tabs",
        real_btn,
        synth_btn,
        matches_btn
      )
    })

    output$dp_body <- shiny::renderUI({
      if (is.null(state$raw_data)) {
        return(shiny::tags$div(
          class = "dp-empty",
          shiny::tags$span(class = "glyph", "\u2191"),
          shiny::tags$p(
            class = "msg",
            "Upload a file or load a sample dataset to preview your data here."
          )
        ))
      }

      if (identical(state$active_step, "compare") &&
          !is.null(state$synthetic) &&
          !is.null(state$compare_selected_var)) {
        var <- state$compare_selected_var
        return(shiny::tagList(
          shiny::tags$div(
            class = "dp-eyebrow",
            style = "margin:8px 0;",
            sprintf("Row-by-row \u00b7 %s", var)
          ),
          shiny::tags$div(
            class = "dp-scroll",
            DT::DTOutput(session$ns("dp_compare_table"), height = "auto")
          )
        ))
      }

      # Exact matches tab: the per-column breakdown of every reproduced row.
      if (identical(active_tab(), "matches") && !is.null(state$synthetic)) {
        detail <- exact_match_detail_r()
        n_match <- if (is.null(detail)) 0L else sum(detail$synthetic_severity > 0L)
        n_red <- exact_match_sensitive_count(detail)
        note <- if (n_red > 0L) {
          shiny::tags$div(
            class = "banner danger",
            style = "margin:8px 0;",
            sprintf(
              paste0(
                "\u26a0 %d of %d reproduced row(s) expose a value you marked ",
                "sensitive. Regenerate before exporting."
              ),
              n_red, n_match
            )
          )
        } else {
          shiny::tags$div(
            class = "banner risk",
            style = "margin:8px 0;",
            sprintf(
              paste0(
                "%d synthetic row(s) reproduce a real record verbatim. No ",
                "sensitive value is exposed, so export is not blocked."
              ),
              n_match
            )
          )
        }
        return(shiny::tagList(
          note,
          shiny::tags$div(
            class = "dp-scroll",
            DT::DTOutput(session$ns("dp_matches_table"), height = "auto")
          ),
          shiny::tags$div(
            class = "dp-footer",
            shiny::tags$span(sprintf("%d row \u00d7 column pair(s)", nrow(detail$breakdown))),
            shiny::tags$span("row numbers match the Original / Synthetic tabs")
          )
        ))
      }

      df <- if (active_tab() == "synth" && !is.null(state$synthetic)) {
        state$synthetic
      } else {
        state$raw_data
      }

      n_rows  <- nrow(df)
      n_cols  <- ncol(df)
      pct_na  <- round(mean(is.na(df)) * 100, 1)
      src_lbl <- if (active_tab() == "synth") {
        paste0("seed = ", if (!is.null(state$seed_used)) state$seed_used else "?")
      } else {
        "source dataset"
      }

      shiny::tagList(
        shiny::tags$div(
          class = "dp-stats",
          shiny::tags$div(
            class = "dp-stat",
            shiny::tags$div(class = "lbl", "Rows"),
            shiny::tags$div(class = "val", as.character(n_rows))
          ),
          shiny::tags$div(
            class = "dp-stat",
            shiny::tags$div(class = "lbl", "Cols"),
            shiny::tags$div(class = "val", as.character(n_cols))
          ),
          shiny::tags$div(
            class = "dp-stat",
            shiny::tags$div(class = "lbl", "Missing"),
            shiny::tags$div(class = "val", paste0(pct_na, "%"))
          )
        ),
        shiny::tags$div(
          class = "dp-scroll",
          DT::DTOutput(session$ns("dp_table"), height = "auto")
        ),
        shiny::tags$div(
          class = "dp-footer",
          shiny::tags$span(sprintf("%d rows total", n_rows)),
          shiny::tags$span(src_lbl)
        )
      )
    })

    output$dp_table <- DT::renderDT({
      shiny::req(state$raw_data)
      df <- if (active_tab() == "synth" && !is.null(state$synthetic)) {
        state$synthetic
      } else {
        state$raw_data
      }

      # Coerce ID-candidate columns to character so they render as strings
      # ("1078541") rather than comma-formatted numbers ("1,078,541.00").
      roles <- state$roles
      if (!is.null(roles) && "recommended_role" %in% names(roles)) {
        eff_role <- ifelse(
          !is.na(roles$user_role) & nzchar(roles$user_role),
          roles$user_role, roles$recommended_role
        )
        id_cols <- roles$variable[eff_role == "alphanumeric ID"]
        for (id_col in intersect(id_cols, names(df))) {
          if (is.numeric(df[[id_col]])) {
            df[[id_col]] <- as.character(df[[id_col]])
          }
        }
      }

      # Explicit row-number column rather than DT rownames: it survives the
      # column filters and paging unchanged, so the number the user reads here
      # is the same number the Exact matches tab points at.
      df <- cbind(`#` = seq_len(nrow(df)), df)

      # Exact-match highlight: append a hidden severity column marking rows that
      # are (Synthetic tab) verbatim copies of an original row, or (Original
      # tab) reproduced verbatim in the synthetic output. 1 = reproduced,
      # 2 = reproduced *and* exposing a value marked sensitive in question 2.
      # Hidden via columnDefs; drives the amber/red row style below.
      sev_vec <- rep(0L, nrow(df))
      detail <- exact_match_detail_r()
      if (!is.null(detail)) {
        sv <- if (active_tab() == "synth" && !is.null(state$synthetic)) {
          detail$synthetic_severity
        } else {
          detail$original_severity
        }
        if (length(sv) == nrow(df)) {
          sev_vec <- sv
        }
      }
      flag_col_index <- ncol(df)  # 0-based; rownames = FALSE, flag appended last
      df[[".exact_match"]] <- as.integer(sev_vec)

      dt <- DT::datatable(
        df,
        # Per-column filter row under the header (search box for text/factor,
        # range slider for numeric) so users can filter each column.
        filter   = "top",
        options  = list(
          dom        = "tp",
          ordering   = FALSE,
          scrollX    = TRUE,
          pageLength = 24L,
          lengthChange = FALSE,
          # Hide the internal exact-match flag column.
          columnDefs = list(list(visible = FALSE, targets = flag_col_index)),
          # Blank DT's transient "Processing.../No data available" strings so the
          # busy-state spinner (CSS) is the only thing shown mid-refresh, rather
          # than flashing error-looking placeholder text.
          language   = list(processing = "", emptyTable = "", zeroRecords = "")
        ),
        rownames  = FALSE,
        class     = "compact",
        selection = "none"
      )

      # Format columns based on original data types (so synth integers display as integers).
      # Skip ID-candidate columns -- they've been coerced to character above and
      # DT::formatRound would parseFloat() them back into "1,078,541.00".
      id_col_set <- if (!is.null(roles) && "recommended_role" %in% names(roles)) {
        eff_role2 <- ifelse(
          !is.na(roles$user_role) & nzchar(roles$user_role),
          roles$user_role, roles$recommended_role
        )
        roles$variable[eff_role2 == "alphanumeric ID"]
      } else {
        character(0)
      }
      orig_df <- state$raw_data
      for (col_name in intersect(names(df), names(orig_df))) {
        if (col_name %in% id_col_set) next
        orig_col <- orig_df[[col_name]]
        if (is_whole_number_column(orig_col)) {
          dt <- DT::formatRound(dt, columns = col_name, digits = 0)
        } else if (is.numeric(orig_col)) {
          dt <- DT::formatRound(dt, columns = col_name, digits = 2)
        }
      }

      # Tint reproduced rows: red when the row exposes a sensitive value (the
      # same danger cue as the EXACT MATCHES stat box, and the condition that
      # blocks export), amber when the row is reproduced but discloses nothing
      # marked sensitive. Semi-transparent so both read on light and dark
      # backgrounds.
      dt <- DT::formatStyle(
        dt, ".exact_match",
        target          = "row",
        backgroundColor = DT::styleEqual(
          c(1L, 2L),
          c("rgba(217, 119, 6, 0.14)", "rgba(220, 38, 38, 0.16)")
        )
      )

      dt
    })


    # One row per (reproduced row x match column). A match is a whole-row
    # collision, so every match column participates -- listing them per column
    # is what lets the user jump to a cell instead of scanning a wide table.
    output$dp_matches_table <- DT::renderDT({
      shiny::req(state$raw_data, state$synthetic)
      detail <- exact_match_detail_r()
      shiny::req(detail)
      b <- detail$breakdown
      shiny::req(nrow(b) > 0)

      out <- data.frame(
        Column      = b$column,
        `Synth row` = b$synthetic_row,
        `Orig row`  = b$original_row,
        Sensitive   = ifelse(b$sensitive, "Yes", "No"),
        Value       = ifelse(is.na(b$value), "\u2014", b$value),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      # Hidden severity flag: a sensitive column with an actual value is what
      # makes the row red and blocks export; a blank one does not.
      out[[".sens"]] <- as.integer(b$sensitive & !is.na(b$value))

      dt <- DT::datatable(
        out,
        filter  = "top",
        options = list(
          dom          = "tp",
          ordering     = FALSE,
          scrollX      = TRUE,
          pageLength   = 24L,
          lengthChange = FALSE,
          columnDefs   = list(list(visible = FALSE, targets = ncol(out) - 1L)),
          language     = list(processing = "", emptyTable = "", zeroRecords = "")
        ),
        rownames  = FALSE,
        class     = "compact",
        selection = "none"
      )
      DT::formatStyle(
        dt, ".sens",
        target          = "row",
        backgroundColor = DT::styleEqual(1L, "rgba(220, 38, 38, 0.16)")
      )
    })

    output$dp_compare_table <- DT::renderDT({
      shiny::req(
        identical(state$active_step, "compare"),
        state$raw_data,
        state$synthetic,
        state$compare_selected_var
      )
      var <- state$compare_selected_var
      shiny::req(var %in% names(state$raw_data), var %in% names(state$synthetic))
      n <- max(nrow(state$raw_data), nrow(state$synthetic))
      pad <- function(x) {
        length(x) <- n
        x
      }
      cmp <- data.frame(
        Original = pad(state$raw_data[[var]]),
        Synthetic = pad(state$synthetic[[var]]),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        cmp,
        options = list(
          dom = "tp",
          ordering = FALSE,
          scrollX = TRUE,
          pageLength = 24L,
          lengthChange = FALSE
        ),
        rownames = TRUE,
        class = "compact",
        selection = "none"
      )
    })

    invisible(NULL)
  })
}
