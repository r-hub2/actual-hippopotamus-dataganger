local({
run_app <- dataganger::run_app

test_that("run_app() errors cleanly when shiny is absent", {
  skip_if(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, and DT are installed -- skipping absent-package test"
  )
  expect_error(run_app())
})

test_that("run_app() returns invisibly when launch is FALSE", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )
  expect_invisible(run_app(launch = FALSE))
})

test_that("run_app(launch = FALSE) does not call shiny::runApp()", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    .run_shiny_app = function(...) {
      called <<- TRUE
      stop("runApp should not be called", call. = FALSE)
    },
    .env = environment(run_app)
  )

  run_app(launch = FALSE)
  expect_false(called)
})

test_that("run_app() forwards NULL port to shiny::runApp()", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )

  call_args <- NULL
  testthat::local_mocked_bindings(
    .run_shiny_app = function(...) {
      call_args <<- list(...)
      invisible(NULL)
    },
    .env = environment(run_app)
  )

  run_app(launch = TRUE)
  expect_named(call_args, c("appDir", "port", "display.mode"))
  expect_null(call_args$port)
})

test_that("run_app() forwards explicit port to shiny::runApp()", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )

  call_args <- NULL
  testthat::local_mocked_bindings(
    .run_shiny_app = function(...) {
      call_args <<- list(...)
      invisible(NULL)
    },
    .env = environment(run_app)
  )

  run_app(port = 7654, launch = TRUE)
  expect_identical(call_args$port, 7654)
})

test_that("run_app() sets shiny.maxRequestSize from max_upload_mb", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )
  withr::local_options(shiny.maxRequestSize = NULL)

  run_app(max_upload_mb = 50, launch = FALSE)
  expect_identical(getOption("shiny.maxRequestSize"), 50 * 1024^2)
})

test_that("run_app() forwards additional arguments to shiny::runApp()", {
  skip_if_not(
    requireNamespace("shiny", quietly = TRUE) &&
      requireNamespace("bslib", quietly = TRUE) &&
      requireNamespace("DT", quietly = TRUE),
    "shiny, bslib, or DT not installed"
  )

  call_args <- NULL
  testthat::local_mocked_bindings(
    .run_shiny_app = function(...) {
      call_args <<- list(...)
      invisible(NULL)
    },
    .env = environment(run_app)
  )

  run_app(launch = TRUE, quiet = TRUE)
  expect_true(isTRUE(call_args$quiet))
})
})
