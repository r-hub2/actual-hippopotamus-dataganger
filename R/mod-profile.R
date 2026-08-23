#' Internal Shiny Profile Module
#'
#' @keywords internal
#' @noRd
mod_profile_ui <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$details(
      shiny::tags$summary("Profile details"),
      shiny::verbatimTextOutput(ns("profile_print")),
      shiny::tableOutput(ns("profile_table"))
    )
  )
}

#' @keywords internal
#' @noRd
mod_profile_server <- function(id, state) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  shiny::moduleServer(id, function(input, output, session) {
    profile_variables <- function(profile_obj) {
      variables <- profile_obj$variables

      if (is.null(variables)) {
        variables <- profile_obj$profile
      }

      if (is.null(variables) || !is.data.frame(variables)) {
        return(data.frame())
      }

      tibble::as_tibble(variables)
    }

    build_profile_text <- function(profile_obj) {
      variables <- profile_variables(profile_obj)
      header <- c(
        "DataGangeR Profile",
        sprintf("%s rows x %s columns", profile_obj$n_rows, profile_obj$n_cols),
        ""
      )

      if (!nrow(variables)) {
        return(paste(header, collapse = "\n"))
      }

      detail_lines <- apply(variables, 1, function(row) {
        parts <- c(
          sprintf("%s (%s)", row[["variable"]], row[["type"]]),
          if ("n_missing" %in% names(row)) sprintf("missing=%s", row[["n_missing"]]) else NULL,
          if ("pct_missing" %in% names(row)) sprintf("pct_missing=%s", round(as.numeric(row[["pct_missing"]]), 1)) else NULL,
          if ("n_distinct" %in% names(row)) sprintf("n_distinct=%s", row[["n_distinct"]]) else NULL,
          if ("n_unique" %in% names(row)) sprintf("n_unique=%s", row[["n_unique"]]) else NULL
        )
        paste(parts, collapse = " | ")
      })

      paste(c(header, detail_lines), collapse = "\n")
    }

    profile_text <- shiny::reactive({
      shiny::req(state$profile, cancelOutput = TRUE)
      build_profile_text(state$profile)
    })

    build_profile_table <- function(profile_obj) {
      variables <- profile_variables(profile_obj)

      if (!"type" %in% names(variables) && "class" %in% names(variables)) {
        variables$type <- variables$class
      }

      if (!"n_unique" %in% names(variables) && "n_distinct" %in% names(variables)) {
        variables$n_unique <- variables$n_distinct
      }

      wanted <- c("variable", "type", "n_missing", "pct_missing", "n_unique")
      available <- intersect(wanted, names(variables))

      out <- variables[, available, drop = FALSE]

      if ("n_missing" %in% names(out)) {
        out$n_missing <- as.integer(out$n_missing)
      }

      if ("n_unique" %in% names(out)) {
        out$n_unique <- as.integer(out$n_unique)
      }

      out
    }

    output$profile_print <- shiny::renderText({
      profile_text()
    })

    profile_table <- shiny::reactive({
      shiny::req(state$profile, cancelOutput = TRUE)
      build_profile_table(state$profile)
    })

    output$profile_table <- shiny::renderTable({
      profile_table()
    }, rownames = FALSE)

    list(
      profile_text = profile_text,
      profile_table = profile_table
    )
  })
}
