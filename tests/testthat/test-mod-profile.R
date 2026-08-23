local({
upload_fixture_path <- function(data, filename) {
  path <- tempfile(pattern = tools::file_path_sans_ext(filename))
  path <- paste0(path, ".", tools::file_ext(filename))
  readr::write_csv(data, path)
  path
}

load_example_data <- function(name) {
  data_env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "dataganger", envir = data_env)
  data_env[[name]]
}

upload_input_value <- function(path, type = "text/csv") {
  data.frame(
    name = basename(path),
    size = file.info(path)$size,
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

profile_host_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- mod_state_server("state")
    mod_upload_server("upload", state)
    mod_column_filter_server("column_filter", state)
    profile <- mod_profile_server("profile", state)
    list(state = state, profile = profile)
  })
}

test_that("profile outputs render after upload through mod-upload wiring", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- load_example_data("example_health_survey")
  csv_path <- upload_fixture_path(example_health_survey[1:5, ], "profile-five.csv")

  shiny::testServer(profile_host_server, {
    state <- session$getReturned()$state
    profile <- session$getReturned()$profile

    session$setInputs(`upload-file` = upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)

    expect_false(is.null(state$profile))
    expect_match(profile$profile_text(), "DataGangeR Profile")

    profile_table <- profile$profile_table()
    expect_s3_class(profile_table, "data.frame")
    expect_gte(nrow(profile_table), 1)
    expect_true(
      all(c("variable", "type") %in% names(profile_table)),
      info = paste("Profile columns:", paste(names(profile_table), collapse = ", "))
    )
  })
})

test_that("profile outputs stay empty when profile is NULL", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  shiny::testServer(profile_host_server, {
    state <- session$getReturned()$state
    profile <- session$getReturned()$profile

    expect_null(state$profile)
    expect_error(profile$profile_text(), class = "shiny.output.cancel")
    expect_error(profile$profile_table(), class = "shiny.output.cancel")
  })
})
})
