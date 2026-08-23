#' Internal Shiny Export Module
#'
#' @keywords internal
#' @noRd
mod_export_ui <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 06 \u00b7 Export"),
        shiny::tags$h1("Export your data"),
        shiny::tags$p(
          class = "subtitle",
          "Download the full bundle: your synthetic data as CSV plus the ",
          "human guide, comparison report, agent recipe, and manifest."
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::downloadButton(
          ns("download"),
          label = "Download bundle \u2192",
          class = "btn btn-primary"
        )
      )
    ),
    stale_banner_ui("export", ns = ns),
    shiny::tags$div(class = "double-rule"),
    shiny::tags$div(
      class = "card",
      shiny::tags$div(
        class = "card-header",
        shiny::tags$span(class = "title", "What's in the bundle"),
        shiny::tags$span(class = "sub", "export_synthetic()")
      ),
      shiny::tags$ul(
        class = "bundle-contents",
        shiny::tags$li(shiny::tags$strong("synthetic_data.csv"), " \u2014 the synthetic dataset"),
        shiny::tags$li(shiny::tags$strong("human/human.md"), " \u2014 start here; explains the bundle, privacy notes, and agent guidance"),
        shiny::tags$li(shiny::tags$strong("human/comparison_report.html"), " \u2014 fidelity + privacy comparison"),
        shiny::tags$li(shiny::tags$strong("agent/recipe.yaml"), " \u2014 synthesis settings plus per-column role decisions"),
        shiny::tags$li(shiny::tags$strong("agent/AGENT.md"), " \u2014 packaged instructions for using the bundle safely"),
        shiny::tags$li(shiny::tags$strong("agent/manifest.json"), " \u2014 provenance and disclosure metrics")
      ),
      shiny::tags$p(
        class = "help",
        "The bundle downloads to your browser's Downloads folder. ",
        "See human/human.md inside it for what each file is for."
      )
    ),
    shiny::uiOutput(ns("exact_match_export_gate")),
    shiny::uiOutput(ns("kanon_export_gate"))
  )
}

#' @keywords internal
#' @noRd
mod_export_server <- function(id, state) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  shiny::moduleServer(id, function(input, output, session) {
    output$stale__export <- shiny::renderText({
      if (isTRUE(state$stale$export)) {
        "true"
      } else {
        "false"
      }
    })

    shiny::outputOptions(output, "stale__export", suspendWhenHidden = FALSE)

    # Exact-match detail for the export gate. Computed on the same inputs and
    # via the same helper the data panel uses, so the gate and the "Exact
    # matches" tab can never disagree about what is blocking.
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

    # Number of reproduced rows exposing a sensitive value. Non-zero blocks the
    # browser export.
    exact_match_blockers <- function() {
      exact_match_sensitive_count(exact_match_detail_r())
    }

    # Disclosure-risk brief shown when the user arrives at the Export step.
    # Pure report: it grants nothing. The inline acknowledgment gates above and
    # the build_export() stops below are untouched. Reads the stored kanon
    # attribute and the same exact-match detail the gate uses, so the brief and
    # the gate cannot disagree. n_exact equals exact_row_match_count() by
    # construction (both come from the same per-row flags).
    disclosure_modal <- function() {
      orig <- state$raw_data
      syn <- state$synthetic
      if (is.null(orig) || is.null(syn)) {
        return(NULL)
      }
      kanon <- state$kanon %||% attr(syn, "kanon", exact = TRUE)
      detail <- exact_match_detail_r()
      n_exact <- if (is.null(detail)) {
        0L
      } else {
        as.integer(sum(detail$synthetic_severity > 0L))
      }
      n_sensitive <- exact_match_sensitive_count(detail)
      roles <- state$generated_roles %||% state$roles
      flags <- state$privacy %||% privacy_check(orig, syn, roles = roles, stage = "post", spec = state$spec)
      disclosure_risk_modal(
        kanon = kanon,
        n_exact = n_exact,
        n_sensitive = n_sensitive,
        privacy_flags = flags
      )
    }

    shiny::observeEvent(state$active_tab, ignoreNULL = TRUE, {
      if (!identical(state$active_tab, "export")) {
        return()
      }
      modal <- disclosure_modal()
      if (!is.null(modal)) {
        shiny::showModal(modal)
      }
    })

    output$exact_match_export_gate <- shiny::renderUI({
      n_red <- exact_match_blockers()
      if (n_red == 0L) {
        return(NULL)
      }

      # Only offer the override once the user has regenerated at least once.
      # Some datasets (few columns, all low-cardinality) collide by
      # construction and no seed will clear them -- without this the user
      # would be locked out of their own export.
      may_override <- (state$generation_count %||% 0L) >= 2L

      shiny::tags$div(
        class = "card",
        style = "margin-top:12px; border-left:4px solid var(--danger-500, #dc2626);",
        shiny::tags$div(
          class = "card-header",
          shiny::tags$span(class = "title", "Exact matches on sensitive columns"),
          shiny::tags$span(class = "sub", "export blocked")
        ),
        shiny::tags$p(
          style = "margin-top:8px;",
          sprintf(
            paste0(
              "%d synthetic row(s) reproduce a real record verbatim, including ",
              "at least one value you marked sensitive. See the Exact matches ",
              "tab in the data preview for the exact rows and columns."
            ),
            n_red
          )
        ),
        if (may_override) {
          shiny::tagList(
            shiny::tags$p(
              class = "help",
              paste0(
                "Regenerating has not cleared these. With few, low-cardinality ",
                "columns some collisions are unavoidable. You can proceed, but ",
                "the decision is recorded in the bundle manifest."
              )
            ),
            shiny::checkboxInput(
              session$ns("exact_match_acknowledged"),
              label = paste0(
                "I understand that this output reproduces real records ",
                "including sensitive values, and I still want to export it."
              ),
              value = FALSE
            )
          )
        } else {
          shiny::tags$p(
            class = "help",
            "Go back to Generate and regenerate with a new seed."
          )
        }
      )
    })

    output$kanon_export_gate <- shiny::renderUI({
      kanon <- state$kanon %||% attr(state$synthetic, "kanon", exact = TRUE)
      if (is.null(kanon) || !isTRUE(kanon$infeasible)) {
        return(NULL)
      }

      current_k <- kanon$k %||% state$spec$k_anon %||% 5L
      qi_cols <- kanon$qi_cols %||% character(0)
      qi_nodes <- list()
      for (i in seq_along(qi_cols)) {
        if (i > 1L) {
          qi_nodes[[length(qi_nodes) + 1L]] <- if (i == length(qi_cols)) " and " else ", "
        }
        qi_nodes[[length(qi_nodes) + 1L]] <- shiny::tags$code(qi_cols[[i]])
      }
      smallest_cell <- kanon$smallest_cell %||% NULL
      shortfall <- if (!is.null(smallest_cell)) {
        sprintf(
          "In this output, the smallest combination has %s row%s, which is fewer than %s.",
          smallest_cell,
          if (identical(as.integer(smallest_cell), 1L)) "" else "s",
          current_k
        )
      } else {
        sprintf("At least one combination appears in fewer than %s rows.", current_k)
      }

      shiny::tags$div(
        class = "card",
        style = "margin-top:12px; border-left:4px solid var(--risk-500);",
        shiny::tags$div(
          class = "card-header",
          shiny::tags$span(class = "title", "Acknowledge missing k-anonymity protection"),
          shiny::tags$span(class = "sub", "required before browser export")
        ),
        shiny::tags$p(
          style = "margin-top:8px;",
          shiny::tagList(
            sprintf("For this output, k = %s means every combination of values across ", current_k),
            dg_privacy_term("quasi-identifier (QI)", "qi"),
            " columns ",
            qi_nodes,
            sprintf(" must appear in at least %s rows. ", current_k),
            shortfall
          )
        ),
        shiny::tags$p(
          style = "margin:0 0 8px;",
          paste(
            "Enforcing that rule would suppress too much of this output, so",
            "k-anonymity was not applied. The bundle remains blocked until",
            "you acknowledge the missing protection."
          )
        ),
        shiny::checkboxInput(
          session$ns("kanon_acknowledged"),
          label = "I understand that no k-anonymity protection was applied to this output, and I still want to export this bundle.",
          value = FALSE
        )
      )
    })

    export_base_name <- function() {
      seed <- shiny::isolate(state$seed_used)
      if (!is.null(seed)) {
        paste0("synthetic_data_seed", seed)
      } else {
        "synthetic_data"
      }
    }

    use_original_names <- function() {
      NULL
    }

    # Build the full bundle into `bundle_dir` and return the path to the ZIP.
    build_export <- function(bundle_dir) {
      shiny::req(state$synthetic, state$spec)
      kanon <- shiny::isolate(state$kanon %||% attr(state$synthetic, "kanon", exact = TRUE))
      kanon_acknowledged <- isTRUE(shiny::isolate(input$kanon_acknowledged))
      if (isTRUE(kanon$infeasible) && !kanon_acknowledged) {
        stop(
          "Export requires explicit acknowledgment because k-anonymity was not applied to this output.",
          call. = FALSE
        )
      }

      # Reproduced rows that expose a sensitive value block the export. Before
      # the user has regenerated there is no override at all; afterwards an
      # explicit acknowledgment is required and is recorded in the manifest.
      n_red <- shiny::isolate(exact_match_blockers())
      exact_match_acknowledged <- isTRUE(shiny::isolate(input$exact_match_acknowledged))
      if (n_red > 0L) {
        may_override <- (shiny::isolate(state$generation_count) %||% 0L) >= 2L
        if (!may_override) {
          stop(
            sprintf(
              paste0(
                "%d synthetic row(s) reproduce a real record verbatim including ",
                "sensitive values. Regenerate with a new seed before exporting."
              ),
              n_red
            ),
            call. = FALSE
          )
        }
        if (!exact_match_acknowledged) {
          stop(
            sprintf(
              paste0(
                "Export requires explicit acknowledgment because %d synthetic ",
                "row(s) reproduce real records including sensitive values."
              ),
              n_red
            ),
            call. = FALSE
          )
        }
      } else {
        exact_match_acknowledged <- FALSE
      }

      export_roles <- shiny::isolate(state$generated_roles %||% state$roles)
      if (is.null(export_roles) && !is.null(state$raw_data)) {
        export_roles <- detect_roles(state$raw_data)
      }

      export_synthetic(
        synthetic = state$synthetic,
        original = state$raw_data,
        comparison = state$comparison,
        privacy = state$privacy,
        path = bundle_dir,
        format = "dir",
        overwrite = TRUE,
        include_report = TRUE,
        include_dictionary = FALSE,
        fail_on_exact_match = FALSE,
        roles = export_roles,
        include_original_names = use_original_names(),
        kanon_acknowledged = kanon_acknowledged,
        exact_match_acknowledged = exact_match_acknowledged
      )

      zip_path <- file.path(bundle_dir, paste0(export_base_name(), "_bundle.zip"))
      files <- list.files(bundle_dir, full.names = TRUE, recursive = TRUE)
      files <- files[!file.info(files)$isdir]
      files <- sub(paste0("^", normalizePath(bundle_dir, winslash = "/", mustWork = TRUE), "/?"), "", normalizePath(files, winslash = "/", mustWork = TRUE))
      # Avoid zipping the zip into itself.
      files <- files[files != basename(zip_path)]
      zip::zip(zipfile = zip_path, files = files, root = bundle_dir)
      zip_path
    }

    output$download <- shiny::downloadHandler(
      filename = function() paste0(export_base_name(), "_bundle.zip"),
      content = function(file) {
        bundle_dir <- tempfile("mod-export-bundle-")
        dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(bundle_dir, recursive = TRUE))

        artefact <- tryCatch(
          build_export(bundle_dir),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
            stop(e)
          }
        )
        file.copy(from = artefact, to = file, overwrite = TRUE)
        invisible(NULL)
      }
    )
  })
}
