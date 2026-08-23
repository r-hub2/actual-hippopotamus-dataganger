local({
# Regression coverage for a bug where overriding a column's Configure-page
# "type" (identifier / free_text / categorical / ...) had no effect on
# whether the column was actually dropped: enforce_kanon() removes columns
# by disclosure_role/identifies, not by the type dropdown's user_role, so a
# column auto-detected as an ID or free text stayed silently dropped even
# after the user picked "categorical". mod-roles.R's role_change handler now
# also updates identifies/disclosure_role/simulation.

is_blank <- function(x) is.na(x) | !nzchar(x %||% "")

role_override_fixture <- function() {
  data_env <- new.env(parent = emptyenv())
  utils::data(list = "example_health_survey", package = "dataganger", envir = data_env)
  df <- data_env[["example_health_survey"]]

  set.seed(42)
  df$clinical_notes <- paste(
    "Patient reports", sample(c("mild", "moderate", "severe"), nrow(df), TRUE),
    "symptoms and requests a follow-up appointment for further evaluation."
  )
  df
}

role_override_upload_path <- function(data, filename) {
  path <- tempfile(pattern = tools::file_path_sans_ext(filename))
  path <- paste0(path, ".", tools::file_ext(filename))
  readr::write_csv(data, path)
  path
}

role_override_upload_input <- function(path, type = "text/csv") {
  data.frame(
    name = basename(path),
    size = file.info(path)$size,
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

role_override_host_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- mod_state_server("state")
    mod_upload_server("upload", state)
    mod_column_filter_server("column_filter", state)
    mod_roles_server("roles", state)

    shiny::observe({
      if (!is.null(state$raw_data) &&
          !is.null(state$profile) &&
          is.null(state$roles)) {
        state$roles <- detect_roles(state$raw_data, profile = state$profile)
      }
    })

    list(state = state)
  })
}

test_that("a simulated free-text column is auto-flagged as a direct identifier", {
  df <- role_override_fixture()
  roles <- detect_roles(df)
  row <- roles[roles$variable == "clinical_notes", ]

  expect_equal(row$recommended_role, "free text")
  expect_equal(row$identifies, "direct")
  expect_equal(row$disclosure_role, "direct")
})

test_that("overriding the type to categorical clears the direct flag and the column survives synthesize_data()", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  df <- role_override_fixture()
  csv_path <- role_override_upload_path(df, "role-override-notes.csv")
  final_roles <- NULL

  shiny::testServer(role_override_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = role_override_upload_input(csv_path))
    session$flushReact()
    session$flushReact()
    cf_continue(session, state)

    notes_row <- which(state$roles$variable == "clinical_notes")
    id_row    <- which(state$roles$variable == "record_id")

    expect_equal(state$roles$identifies[[notes_row]], "direct")
    expect_equal(state$roles$identifies[[id_row]], "direct")

    session$setInputs(
      `roles-role_change` = list(row = notes_row, value = "categorical")
    )
    session$flushReact()
    session$setInputs(
      `roles-role_change` = list(row = id_row, value = "categorical")
    )
    session$flushReact()

    expect_true(is_blank(state$roles$identifies[[notes_row]]))
    expect_true(is_blank(state$roles$disclosure_role[[notes_row]]))
    expect_equal(state$roles$simulation[[notes_row]], "synthesize")

    expect_true(is_blank(state$roles$identifies[[id_row]]))
    expect_true(is_blank(state$roles$disclosure_role[[id_row]]))
    expect_equal(state$roles$simulation[[id_row]], "synthesize")

    final_roles <<- state$roles
  })

  # Before the fix, these two columns were still silently dropped by
  # enforce_kanon() despite the type override, because it keys off
  # disclosure_role/identifies rather than user_role/simulation.
  spec <- synth_spec(purpose = "development", engine = "internal")
  syn <- synthesize_data(df, spec, roles = final_roles, engine = "internal")

  expect_true("clinical_notes" %in% names(syn))
  expect_true("record_id" %in% names(syn))
})
})
