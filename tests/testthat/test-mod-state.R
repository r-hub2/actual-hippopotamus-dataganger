test_that("new upload resets downstream state and clears stale flags", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(mod_state_server, {
    state <- session$getReturned()

    state$roles <- tibble::tibble(variable = "x", user_role = "measure")
    state$spec <- list(purpose = "development")
    state$synthetic <- tibble::tibble(x = 1)
    state$comparison <- list(ok = TRUE)
    state$privacy <- tibble::tibble(flag = "none")
    state$stale <- list(synthesis = TRUE, comparison = TRUE, export = TRUE)

    state$raw_data <- tibble::tibble(x = 1:3)
    session$flushReact()

    expect_s3_class(state$raw_data, "data.frame")
    expect_null(state$profile)
    expect_null(state$roles)
    expect_null(state$spec)
    expect_null(state$synthetic)
    expect_null(state$comparison)
    expect_null(state$privacy)
    expect_identical(
      state$stale,
      list(synthesis = FALSE, comparison = FALSE, export = FALSE)
    )
  })
})

test_that("roles change invalidates downstream state and marks all stale", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(mod_state_server, {
    state <- session$getReturned()

    state$raw_data <- tibble::tibble(x = 1:3)
    session$flushReact()

    state$roles <- tibble::tibble(variable = "x", user_role = "measure")
    session$flushReact()

    state$spec <- list(purpose = "development")
    state$synthetic <- NULL
    state$comparison <- list(ok = TRUE)
    state$privacy <- tibble::tibble(flag = "none")
    state$stale <- list(synthesis = FALSE, comparison = FALSE, export = FALSE)

    state$roles <- tibble::tibble(variable = "x", user_role = "identifier")
    session$flushReact()

    expect_null(state$spec)
    expect_null(state$synthetic)
    expect_null(state$comparison)
    expect_null(state$privacy)
    expect_true(isTRUE(state$stale$synthesis))
    expect_true(isTRUE(state$stale$comparison))
    expect_true(isTRUE(state$stale$export))
  })
})

test_that("spec change invalidates synthesis outputs and marks all stale", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(mod_state_server, {
    state <- session$getReturned()

    state$raw_data <- tibble::tibble(x = 1:3)
    session$flushReact()

    state$roles <- tibble::tibble(variable = "x", user_role = "measure")
    session$flushReact()

    state$spec <- list(purpose = "development")
    session$flushReact()

    state$synthetic <- tibble::tibble(x = 4:6)
    state$comparison <- list(ok = TRUE)
    state$privacy <- tibble::tibble(flag = "none")
    state$stale <- list(synthesis = FALSE, comparison = FALSE, export = FALSE)

    state$spec <- list(purpose = "demo")
    session$flushReact()

    expect_identical(state$spec, list(purpose = "demo"))
    expect_null(state$synthetic)
    expect_null(state$comparison)
    expect_null(state$privacy)
    expect_true(isTRUE(state$stale$synthesis))
    expect_true(isTRUE(state$stale$comparison))
    expect_true(isTRUE(state$stale$export))
  })
})


test_that("state initializes active_step and compare_selected_var", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(mod_state_server, {
    state <- session$getReturned()
    expect_identical(state$active_step, "upload")
    expect_null(state$compare_selected_var)
  })
})
