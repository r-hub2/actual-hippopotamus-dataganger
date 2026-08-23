#' Header-only suggestion for which columns look like identifiers
#'
#' Pure name-based check (no data scan) so it can run the instant a file's
#' column names are known, before profiling or full role detection. Reuses
#' the same identifier name pattern that [detect_roles()] later confirms
#' against the full data.
#'
#' @param col_names Character vector of column names.
#' @return The subset of `col_names` whose name looks like an identifier.
#' @keywords internal
#' @noRd
column_filter_suggested_drop <- function(col_names) {
  col_names[grepl(dg_id_name_pattern(), col_names, perl = TRUE)]
}

#' Build the column-filter triage modal
#'
#' Three drop zones -- Synthesise, Pass through, Drop -- pre-populated from
#' `column_filter_suggested_drop()`. The user drags (or clicks, to cycle)
#' columns between zones; "Continue" reads the final zone membership back
#' into a single Shiny input.
#'
#' @keywords internal
#' @noRd
column_filter_modal <- function(col_names, ns) {
  suggested_drop <- column_filter_suggested_drop(col_names)
  bucket_of <- function(col) if (col %in% suggested_drop) "drop" else "synthesize"

  zone_info <- list(
    synthesize = list(
      title = "Synthesise",
      hint = "Recreated from the data's patterns."
    ),
    pass_through = list(
      title = "Pass through",
      hint = "Kept exactly as-is, unchanged."
    ),
    drop = list(
      title = "Drop",
      hint = "Removed entirely, not included in the output."
    )
  )

  make_chip <- function(col) {
    shiny::tags$span(
      class = "cf-chip",
      draggable = "true",
      tabindex = "0",
      role = "button",
      `data-col` = col,
      col
    )
  }

  make_zone <- function(key) {
    info <- zone_info[[key]]
    cols_here <- col_names[vapply(col_names, bucket_of, character(1)) == key]
    shiny::tags$div(
      class = "cf-zone",
      `data-bucket` = key,
      shiny::tags$div(
        class = "cf-zone-header",
        shiny::tags$span(class = "cf-zone-title", info$title),
        shiny::tags$span(class = "cf-zone-hint", info$hint)
      ),
      shiny::tags$div(
        class = "cf-zone-chips",
        lapply(cols_here, make_chip)
      )
    )
  }

  shiny::modalDialog(
    title = "Sort your columns before you continue",
    size = "l",
    easyClose = FALSE,
    shiny::tags$p(
      class = "cf-modal-intro",
      "Drag each variable into the box for how it should be treated (or click ",
      "a variable to cycle it). Variables in the Drop box will be excluded ",
      "from all downstream steps (profiling, role detection, synthesis, and ",
      "export). Columns that look like IDs are pre-suggested for Drop \u2014 ",
      "move them out if that's wrong."
    ),
    shiny::tags$div(
      class = "cf-zones",
      make_zone("synthesize"),
      make_zone("pass_through"),
      make_zone("drop")
    ),
    footer = shiny::tags$button(
      type = "button",
      class = "btn btn-primary cf-apply",
      `data-input-id` = ns("buckets"),
      "Continue"
    )
  )
}

#' Internal Shiny Column Filter Module
#'
#' Shows the column-filter triage modal as soon as `state$upload_source` is
#' set, driven only by that source's column names (no column values are read
#' here). Stores the user's choice in `state$column_filter`, a named list
#' mapping column name to `"synthesize"`, `"pass_through"`, or `"drop"`. On
#' Continue it reads only the non-dropped columns (via
#' `upload_source$read(col_select = keep)`) into `state$raw_data`. For CSV
#' files, dropped columns are never parsed; for other formats they are read
#' then immediately discarded. Nothing downstream (profiling, role detection,
#' synthesis, export) ever sees a dropped column.
#'
#' @keywords internal
#' @noRd
mod_column_filter_server <- function(id, state) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # A new upload announces its column NAMES via state$upload_source (no data
    # is read yet). Triage is on names only. Clear any prior working data /
    # filter choice, and do not populate state$raw_data until the user
    # confirms which columns to keep.
    shiny::observeEvent(state$upload_source, ignoreNULL = TRUE, {
      state$column_filter <- NULL
      state$raw_data <- NULL
      shiny::showModal(column_filter_modal(state$upload_source$columns, ns))
    })

    shiny::observeEvent(input$buckets, ignoreNULL = TRUE, {
      buckets <- input$buckets
      state$column_filter <- buckets

      dropped <- names(buckets)[vapply(buckets, identical, logical(1), "drop")]
      src <- state$upload_source
      keep <- setdiff(src$columns, dropped)

      # Read only the kept columns. For CSV files this means dropped columns
      # are never parsed; for other formats they are read then discarded.
      full <- tryCatch(
        src$read(col_select = keep),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(full)) {
        return(invisible(NULL))
      }
      state$raw_data <- full[, intersect(keep, names(full)), drop = FALSE]

      shiny::removeModal()
    })

    invisible(NULL)
  })
}
