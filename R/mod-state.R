#' Internal Shiny State Store
#'
#' @keywords internal
#' @noRd
mod_state_ui <- function(id) {
  NULL
}

#' @keywords internal
#' @noRd
mod_state_server <- function(id) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  shiny::moduleServer(id, function(input, output, session) {
    make_stale_flags <- function(value = FALSE) {
      list(
        synthesis = value,
        comparison = value,
        export = value
      )
    }

    state_token <- function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      if (is.data.frame(x) && nrow(x) > 500L) {
        list(nrow(x), ncol(x), names(x))
      } else {
        paste(deparse(x, control = NULL), collapse = "")
      }
    }

    state <- shiny::reactiveValues(
      # Describes a pending upload as {columns, read}: `columns` is the column
      # names (known up front, drives the column-filter box), `read` is a
      # closure that loads the full data -- called only when the user clicks
      # Continue, so dropped columns are never read in.
      upload_source = NULL,
      raw_data = NULL,
      filename = NULL,
      profile = NULL,
      roles = NULL,
      roles_confirmed = 0L,
      column_filter = NULL,
      objective_confirmed = 0L,
      spec = NULL,
      spec_confirmed = 0L,
      synthetic = NULL,
      comparison = NULL,
      compare_selected_var = NULL,
      privacy = NULL,
      seed_used = NULL,
      # Number of synthesis runs in this session. The exact-match export gate
      # blocks outright on the first run and only offers the acknowledged
      # override once the user has actually tried regenerating, so a run that
      # cannot be cleared (low-cardinality data, where every synthetic row
      # collides by construction) does not lock the user out permanently.
      generation_count = 0L,
      nav_request = NULL,
      active_step = "upload",
      attested_no_direct = FALSE,
      fail_safe_status = "idle",
      fail_safe_flagged = data.frame(
        variable = character(0),
        reason = character(0),
        stringsAsFactors = FALSE
      ),
      fail_safe_upload_token = NULL,
      stale = make_stale_flags(FALSE)
    )

    tokens <- shiny::reactiveValues(
      raw_data = NULL,
      roles = NULL,
      spec = NULL
    )

    set_stale_flags <- function(value) {
      state$stale <- make_stale_flags(value)
    }

    observe_reset_upload <- shiny::observe({
      raw_data_token <- state_token(state$raw_data)

      if (identical(raw_data_token, tokens$raw_data)) {
        return()
      }

      tokens$raw_data <- raw_data_token
      tokens$roles <- NULL
      tokens$spec <- NULL

      state$profile <- NULL
      state$roles <- NULL
      state$roles_confirmed <- 0L
      # column_filter is intentionally NOT cleared here: setting raw_data is how
      # the column-filter step *applies* the user's choice, and this observer
      # fires on that. A genuinely new upload clears column_filter via the
      # column-filter module (keyed on state$uploaded_data) instead.
      if (is.null(state$raw_data)) {
        state$filename <- NULL
      }
      state$spec <- NULL
      state$synthetic <- NULL
      state$comparison <- NULL
      state$compare_selected_var <- NULL
      state$privacy <- NULL
      state$seed_used <- NULL
      state$fail_safe_status <- "idle"
      state$fail_safe_flagged <- data.frame(
        variable = character(0),
        reason = character(0),
        stringsAsFactors = FALSE
      )
      state$fail_safe_upload_token <- NULL
      set_stale_flags(FALSE)
    })

    observe_reset_roles <- shiny::observe({
      roles_token <- state_token(state$roles)

      if (identical(roles_token, tokens$roles)) {
        return()
      }

      tokens$roles <- roles_token

      if (is.null(roles_token)) {
        return()
      }

      tokens$spec <- NULL
      state$spec <- NULL
      state$synthetic <- NULL
      state$comparison <- NULL
      state$compare_selected_var <- NULL
      state$privacy <- NULL
      set_stale_flags(TRUE)
    })

    observe_reset_spec <- shiny::observe({
      spec_token <- state_token(state$spec)

      if (identical(spec_token, tokens$spec)) {
        return()
      }

      tokens$spec <- spec_token

      if (is.null(spec_token)) {
        return()
      }

      state$synthetic <- NULL
      state$comparison <- NULL
      state$compare_selected_var <- NULL
      state$privacy <- NULL
      state$seed_used <- NULL
      set_stale_flags(TRUE)
    })

    for (flag_name in names(shiny::isolate(state$stale))) {
      local({
        current_flag <- flag_name
        output_id <- paste0("stale__", current_flag)

        output[[output_id]] <- shiny::renderText({
          if (isTRUE(state$stale[[current_flag]])) {
            "true"
          } else {
            "false"
          }
        })

        shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
      })
    }

    state
  })
}

#' @keywords internal
#' @noRd
stale_banner_ui <- function(
  flag_name,
  ns = shiny::NS(NULL),
  title = "Results stale",
  message = "Re-generate before trusting downstream outputs."
) {
  rlang::check_installed("shiny", reason = "to use the DataGangeR Shiny modules")

  output_id <- ns(paste0("stale__", flag_name))

  shiny::conditionalPanel(
    condition = sprintf("output['%s'] === 'true'", output_id),
    shiny::div(
      class = "banner info",
      shiny::tags$span(class = "icon", "i"),
      shiny::div(
        if (!is.null(title)) shiny::tags$b(title),
        if (!is.null(title)) paste0(" ", message) else message
      )
    )
  )
}
