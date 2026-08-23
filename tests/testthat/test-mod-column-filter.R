local({
test_that("column_filter_suggested_drop flags id-like column names only", {
  cols <- c("id", "patient_id", "record_num", "age", "score", "city")
  expect_identical(
    dataganger:::column_filter_suggested_drop(cols),
    c("id", "patient_id", "record_num")
  )
})

test_that("column_filter_suggested_drop returns character(0) when nothing matches", {
  expect_identical(
    dataganger:::column_filter_suggested_drop(c("age", "score", "city")),
    character(0)
  )
})

test_that("column_filter_modal pre-suggests id-like columns into the drop zone", {
  testthat::skip_if_not_installed("shiny")

  html <- as.character(
    dataganger:::column_filter_modal(
      c("patient_id", "age", "city"),
      ns = shiny::NS("column_filter")
    )
  )

  # Zones render in a fixed order (synthesize, pass_through, drop); the text
  # between one zone's data-bucket marker and the next belongs to that zone.
  zone_chunks <- strsplit(html, 'data-bucket="', fixed = TRUE)[[1]][-1]
  bucket_of <- function(col) {
    for (chunk in zone_chunks) {
      if (grepl(sprintf('data-col="%s"', col), chunk, fixed = TRUE)) {
        return(sub('".*', "", chunk))
      }
    }
    NA_character_
  }

  expect_identical(bucket_of("patient_id"), "drop")
  expect_identical(bucket_of("age"), "synthesize")
  expect_identical(bucket_of("city"), "synthesize")
})

column_filter_host_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- mod_state_server("state")
    mod_column_filter_server("column_filter", state)
    list(state = state)
  })
}

# Build a pending upload source (as mod_upload_server does): column names known
# up front, plus a `read` closure that loads the full data lazily. `read` also
# records whether it was called, so tests can assert data is only read on
# Continue (never for a dropped-only inspection).
make_upload_source <- function(data) {
  read_count <- new.env(parent = emptyenv())
  read_count$n <- 0L
  list(
    columns = names(data),
    read = function(col_select = NULL) {
      read_count$n <- read_count$n + 1L
      if (is.null(col_select)) data else data[, intersect(col_select, names(data)), drop = FALSE]
    },
    .read_count = read_count
  )
}

test_that("data is not read until the user clicks Continue", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(column_filter_host_server, {
    state <- session$getReturned()$state

    src <- make_upload_source(
      tibble::tibble(patient_id = 1:3, age = c(20, 30, 40))
    )
    state$upload_source <- src
    session$flushReact()

    # Modal is up (names only); the data closure has NOT been called yet.
    expect_identical(src$.read_count$n, 0L)
    expect_null(state$raw_data)

    session$setInputs(
      `column_filter-buckets` = list(patient_id = "drop", age = "synthesize")
    )
    session$flushReact()

    # Continue reads the data exactly once.
    expect_identical(src$.read_count$n, 1L)
  })
})

test_that("applying the popup drops the drop-bucket columns from the working data", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(column_filter_host_server, {
    state <- session$getReturned()$state

    state$upload_source <- make_upload_source(tibble::tibble(
      patient_id = 1:3, age = c(20, 30, 40), city = c("A", "B", "C")
    ))
    session$flushReact()

    session$setInputs(
      `column_filter-buckets` = list(
        patient_id = "drop", age = "synthesize", city = "pass_through"
      )
    )
    session$flushReact()

    # The user's choice is recorded ...
    expect_identical(
      state$column_filter,
      list(patient_id = "drop", age = "synthesize", city = "pass_through")
    )
    # ... and the dropped column is never read into the working data.
    expect_false("patient_id" %in% names(state$raw_data))
    expect_setequal(names(state$raw_data), c("age", "city"))
    expect_identical(nrow(state$raw_data), 3L)
  })
})

test_that("keeping every column carries the full upload into the working data", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(column_filter_host_server, {
    state <- session$getReturned()$state

    state$upload_source <- make_upload_source(tibble::tibble(a = 1:3, b = 4:6))
    session$flushReact()
    session$setInputs(
      `column_filter-buckets` = list(a = "synthesize", b = "pass_through")
    )
    session$flushReact()

    expect_setequal(names(state$raw_data), c("a", "b"))
    expect_identical(nrow(state$raw_data), 3L)
  })
})

test_that("a fresh upload clears the previous filter and working data until re-confirmed", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(column_filter_host_server, {
    state <- session$getReturned()$state

    state$upload_source <- make_upload_source(
      tibble::tibble(patient_id = 1:3, age = c(20, 30, 40))
    )
    session$flushReact()
    session$setInputs(
      `column_filter-buckets` = list(patient_id = "drop", age = "synthesize")
    )
    session$flushReact()
    expect_false(is.null(state$column_filter))
    expect_false(is.null(state$raw_data))

    # A new upload must reset both the choice and the working data; nothing is
    # read in again until the user confirms the new file's columns.
    state$upload_source <- make_upload_source(tibble::tibble(city = c("A", "B")))
    session$flushReact()

    expect_null(state$column_filter)
    expect_null(state$raw_data)
  })
})
})
