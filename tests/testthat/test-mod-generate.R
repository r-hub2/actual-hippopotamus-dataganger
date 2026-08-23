local({
# Generation runs in a callr subprocess in production; force the synchronous
# in-process path here so mocked bindings apply and testServer can observe the
# result without driving an async poll loop. Reset after this file.
withr::local_options(dataganger.synthesis_async = FALSE, .local_envir = testthat::teardown_env())

generate_ready_roles <- function(data) {
  roles <- dg_seed_disclosure(detect_roles(data))
  blank <- is.na(roles$identifies) | !nzchar(roles$identifies)
  roles$identifies[blank] <- "none"
  dg_sync_roles_axes(roles)
}

generate_test_state <- function(data = NULL, spec = NULL) {
  shiny::reactiveValues(
    raw_data = data,
    roles = if (!is.null(data)) generate_ready_roles(data) else NULL,
    spec = spec,
    synthetic = NULL,
    comparison = NULL,
    privacy = NULL,
    seed_used = NULL,
    nav_request = NULL,
    stale = list(
      synthesis = TRUE,
      comparison = TRUE,
      export = TRUE
    )
  )
}

test_that("generate UI exposes a configuration recap output", {
  html <- as.character(mod_generate_ui("generate"))
  expect_match(html, "Your configuration")
  expect_match(html, "generate-config_recap")
  expect_match(html, "generate-decision_recap")
  expect_lt(
    regexpr("generate-decision_recap", html, fixed = TRUE)[[1]],
    regexpr("generate-generate_actions", html, fixed = TRUE)[[1]]
  )
})

test_that("generate stale banner uses friendly guidance", {
  html <- as.character(mod_generate_ui("generate"))

  expect_match(
    html,
    "Review the config, press Generate when ready, or go back to adjust settings.",
    fixed = TRUE
  )
  expect_no_match(html, "Results stale", fixed = TRUE)
  expect_no_match(html, "Re-generate before trusting", fixed = TRUE)
})

capture_notifications <- function() {
  notifications <- list()

  list(
    options = list(
      dataganger.generate_notification_hook = function(args) {
        notifications[[length(notifications) + 1]] <<- list(
          ui = args[[1]],
          duration = if (is.null(args$duration)) 5 else args$duration,
          type = if (is.null(args$type)) "default" else args$type
        )
      }
    ),
    get = function() notifications
  )
}

test_that("generate warns when state raw_data is NULL", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(spec = synth_spec(purpose = "development"))
  recorder <- capture_notifications()
  withr::local_options(recorder$options)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  notifications <- recorder$get()
  expect_length(notifications, 1)
  expect_identical(notifications[[1]]$ui, "No data or spec available.")
  expect_identical(notifications[[1]]$type, "warning")
  expect_null(shiny::isolate(state$synthetic))
})

test_that("generate warns when state spec is NULL", {
  testthat::skip_if_not_installed("shiny")

  data("example_health_survey", package = "dataganger")
  state <- generate_test_state(data = example_health_survey, spec = NULL)
  recorder <- capture_notifications()
  withr::local_options(recorder$options)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  notifications <- recorder$get()
  expect_length(notifications, 1)
  expect_identical(notifications[[1]]$ui, "No data or spec available.")
  expect_identical(notifications[[1]]$type, "warning")
  expect_null(shiny::isolate(state$synthetic))
})

test_that("successful generation populates synthetic outputs", {
  testthat::skip_if_not_installed("shiny")

  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "development", seed = 1)
  state <- generate_test_state(data = example_health_survey, spec = spec)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  expect_s3_class(shiny::isolate(state$synthetic), "dataganger_synthetic")
  expect_s3_class(shiny::isolate(state$comparison), "dataganger_comparison")
  expect_s3_class(shiny::isolate(state$privacy), "dataganger_privacy_check")
  expect_false(is.null(attr(shiny::isolate(state$privacy), "exact_row_matches", exact = TRUE)))
})

test_that("successful generation clears stale flags", {
  testthat::skip_if_not_installed("shiny")

  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "development", seed = 2)
  state <- generate_test_state(data = example_health_survey, spec = spec)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  stale <- shiny::isolate(state$stale)
  expect_false(isTRUE(stale$synthesis))
  expect_false(isTRUE(stale$comparison))
  expect_false(isTRUE(stale$export))
})

test_that("synthesis errors notify and leave state untouched", {
  testthat::skip_if_not_installed("shiny")

  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "development", seed = 3)
  state <- generate_test_state(data = example_health_survey, spec = spec)
  recorder <- capture_notifications()
  withr::local_options(recorder$options)
  testthat::local_mocked_bindings(synthesize_data = function(...) stop("kaboom", call. = FALSE))

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  notifications <- recorder$get()
  expect_length(notifications, 1)
  expect_match(notifications[[1]]$ui, "^Synthesis failed: kaboom$")
  expect_identical(notifications[[1]]$type, "error")
  stale <- shiny::isolate(state$stale)
  expect_null(shiny::isolate(state$synthetic))
  expect_null(shiny::isolate(state$comparison))
  expect_null(shiny::isolate(state$privacy))
  expect_true(isTRUE(stale$synthesis))
  expect_true(isTRUE(stale$comparison))
  expect_true(isTRUE(stale$export))
})

# ---- seed / try-new-seed / adjust-settings tests ----------------------------

make_stub_bindings <- function() {
  toy_synthetic <- structure(
    data.frame(x = 1:3),
    class = c("dataganger_synthetic", "data.frame")
  )
  toy_comparison <- structure(
    list(numeric = tibble::tibble(), categorical = tibble::tibble()),
    class = "dataganger_comparison"
  )
  toy_privacy <- local({
    p <- tibble::tibble()
    class(p) <- c("dataganger_privacy_check", class(p))
    attr(p, "exact_row_matches") <- 0L
    p
  })

  list(
    synthesize_data = function(...) toy_synthetic,
    compare_synthetic = function(...) toy_comparison,
    privacy_check = function(...) toy_privacy
  )
}

test_that("generate stores seed_used when spec seed is NULL", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development")
  )
  stubs <- make_stub_bindings()
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  seed <- shiny::isolate(state$seed_used)
  expect_false(is.null(seed))
  expect_true(is.integer(seed) || (is.numeric(seed) && seed == round(seed)))
})

test_that("generate uses spec seed when not NULL", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development", seed = 42L)
  )
  stubs <- make_stub_bindings()
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  expect_identical(shiny::isolate(state$seed_used), 42L)
})

test_that("engine label resolves to the generated engine after synthesis", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development", seed = 99L)
  )
  stubs <- make_stub_bindings()
  stubs$synthesize_data <- function(...) {
    out <- structure(data.frame(x = 1:3), class = c("dataganger_synthetic", "data.frame"))
    attr(out, "engine") <- "internal"
    out
  }
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    recap_html <- paste(as.character(output$config_recap), collapse = "\n")
    expect_match(recap_html, "internal \\(auto\\)")
  })
})

test_that("preservation note uses readable body text", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:6, y = 7:12),
    spec = synth_spec(purpose = "development", seed = 99L)
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$flushReact()
    recap_html <- paste(as.character(output$decision_recap), collapse = "\n")
    expect_match(recap_html, "What the synthetic data preserves")
    expect_match(recap_html, "font-size:14px", fixed = TRUE)
    expect_no_match(recap_html, "font-size:11px", fixed = TRUE)
  })
})

test_that("regenerate is disabled before generation and enabled after", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development", seed = 5L)
  )
  stubs <- make_stub_bindings()
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    before_html <- paste(as.character(output$header_cta), collapse = "\n")
    expect_match(before_html, "Generate Synthetic Data")
    expect_no_match(before_html, "Regenerate")

    session$setInputs(generate = 1L)
    session$flushReact()

    after_html <- paste(as.character(output$header_cta), collapse = "\n")
    expect_match(after_html, "Regenerate")
    expect_no_match(after_html, "disabled")
    expect_match(after_html, "generate-header-actions")
    expect_match(after_html, "btn-regenerate")
    expect_no_match(after_html, "regenerate-box")
    positions <- vapply(
      c("adjust_settings", "try_new_seed", "go_compare"),
      function(id) regexpr(id, after_html, fixed = TRUE)[[1]],
      integer(1)
    )
    expect_true(
      all(positions > 0L),
      info = paste("CTA positions:", paste(positions, collapse = ", "))
    )
    expect_true(
      all(diff(positions) > 0L),
      info = paste("CTA positions:", paste(positions, collapse = ", "))
    )
  })
})

test_that("result stats include exact row matches after generation", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development", seed = 11L)
  )
  stubs <- make_stub_bindings()
  stubs$privacy_check <- local({
    p <- tibble::tibble()
    class(p) <- c("dataganger_privacy_check", class(p))
    attr(p, "exact_row_matches") <- 2L
    function(...) p
  })
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    stats_html <- paste(as.character(output$result_stats), collapse = "\n")
    expect_match(stats_html, "EXACT MATCHES")
    expect_match(stats_html, ">2<")
    # A synthetic row reproducing a real one verbatim is a direct disclosure,
    # so the stat box gets the red "danger" treatment, not just neutral.
    expect_match(stats_html, "stat danger", fixed = TRUE)
  })
})

test_that("exact matches stat box is not flagged danger when the count is zero", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development", seed = 11L)
  )
  stubs <- make_stub_bindings()
  stubs$privacy_check <- local({
    p <- tibble::tibble()
    class(p) <- c("dataganger_privacy_check", class(p))
    attr(p, "exact_row_matches") <- 0L
    function(...) p
  })
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    stats_html <- paste(as.character(output$result_stats), collapse = "\n")
    expect_match(stats_html, ">0<")
    expect_false(grepl("stat danger", stats_html, fixed = TRUE))
  })
})

test_that("try_new_seed runs synthesis and stores a new seed_used", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development")
  )
  stubs <- make_stub_bindings()
  testthat::local_mocked_bindings(
    synthesize_data   = stubs$synthesize_data,
    compare_synthetic = stubs$compare_synthetic,
    privacy_check     = stubs$privacy_check
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    first_seed <- state$seed_used

    session$setInputs(try_new_seed = 1L)
    session$flushReact()

    expect_false(is.null(first_seed))
    expect_false(identical(state$seed_used, first_seed))
  })

  expect_false(is.null(shiny::isolate(state$synthetic)))
  expect_false(is.null(shiny::isolate(state$seed_used)))
})

test_that("adjust_settings sets nav_request to configure", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development")
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(adjust_settings = 1L)
    session$flushReact()
  })

  expect_identical(shiny::isolate(state$nav_request), "configure")
})


test_that("decision recap renders the revised review columns", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(name = "a", zip = "100", bp = 10, stringsAsFactors = FALSE),
    spec = synth_spec(purpose = "development")
  )
  state$roles <- tibble::tibble(
    variable = c("name", "zip", "bp"),
    recommended_role = c("alphanumeric ID", "categorical candidate", "numeric"),
    user_role = c(NA_character_, "date", NA_character_),
    class = c("alphanumeric ID", "categorical candidate", "numeric"),
    identifies = c("direct", "combination", "none"),
    sensitive = c(FALSE, TRUE, FALSE),
    simulation = c("drop", "pass_through", "synthesize")
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    html <- paste(as.character(output$decision_recap), collapse = "\n")
    expect_match(html, ">Column<")
    expect_match(html, ">Points to a person\\?<")
    expect_match(html, ">Sensitive\\?<")
    expect_match(html, ">Action<")
    expect_match(html, ">What we.ll do<")
    expect_false(grepl("<th[^>]*>TYPE<", html, perl = TRUE))
    expect_false(grepl("<th[^>]*>DISCLOSURE<", html, perl = TRUE))
    expect_match(html, 'title="Modelled as: date"')
    expect_match(html, "Only in combination with other columns")
    expect_match(html, "Pass through")
    expect_match(html, "Use .* Adjust settings to change any of these")
  })
})


test_that("generate blocks when column privacy questions are incomplete", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(id = c("P1", "P2"), zip = c("100", "200"), stringsAsFactors = FALSE),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$identifies[roles$variable == "zip"] <- ""
    state$roles <- dg_sync_roles_axes(roles)
  })

  recorder <- capture_notifications()
  withr::local_options(recorder$options)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
  })

  notifications <- recorder$get()
  expect_length(notifications, 1)
  expect_match(notifications[[1]]$ui, "Finish the column privacy questions")
  expect_null(shiny::isolate(state$synthetic))
})

test_that("generate removes k-anon and high-flag KPI tiles", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(qi = rep(letters[1:2], each = 5), value = 1:10),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$user_identifies <- "none"
    roles$user_sensitive <- FALSE
    roles$identifies <- "none"
    roles$sensitive <- FALSE
    roles <- dg_sync_roles_axes(roles)
    state$roles <- roles
  })

  toy_synthetic <- structure(
    data.frame(qi = rep(letters[1:2], each = 5), value = 1:10),
    class = c("dataganger_synthetic", "data.frame")
  )
  attr(toy_synthetic, "kanon") <- list(
    qi_cols = "qi",
    k = 5L,
    smallest_cell = 1L,
    suppressed_cells = 0L,
    infeasible = TRUE
  )
  toy_privacy <- tibble::tibble(
    variable = "(quasi-identifiers)",
    flag = "high",
    severity = "HIGH",
    recommendation = "review"
  )
  class(toy_privacy) <- c("dataganger_privacy_check", class(toy_privacy))
  attr(toy_privacy, "exact_row_matches") <- 0L
  result <- list(
    synthetic = toy_synthetic,
    comparison = structure(list(), class = "dataganger_comparison"),
    privacy = toy_privacy,
    warnings = paste(
      "Could not apply k-anonymity at k = 5 to the selected quasi-identifier (QI) columns: qi.",
      "To avoid destroying the dataset, no k-anonymity protection was applied to this output."
    ),
    kanon = attr(toy_synthetic, "kanon", exact = TRUE)
  )

  testthat::local_mocked_bindings(run_synthesis_pipeline = function(...) result)
  withr::local_options(dataganger.synthesis_async = FALSE)
  recorder <- capture_notifications()
  withr::local_options(recorder$options)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    html <- paste(as.character(output$result_stats), collapse = "\n")
    expect_no_match(html, "K-anonymity", fixed = TRUE)
    expect_no_match(html, "HIGH FLAGS", fixed = TRUE)
  })

  notifications <- recorder$get()
  infeasible <- vapply(
    notifications, function(x) is_kanon_infeasible_warning(x$ui), logical(1)
  )
  expect_false(
    any(infeasible),
    info = paste("Infeasible notification indices:",
                 paste(which(infeasible), collapse = ", "))
  )
  expect_true(isTRUE(shiny::isolate(state$kanon$infeasible)))
  expect_false(is.null(shiny::isolate(state$generated_roles)))
})

test_that("bottom k-anon status surfaces heavy QI suppression", {
  testthat::skip_if_not_installed("shiny")

  # k-anonymity was successfully enforced (not infeasible), but whole-cell
  # suppression ended up blanking all the QI rows -- this must be visible in
  # the stats card, not just reported as a bare "enforced" success.
  state <- generate_test_state(
    data = data.frame(qi = rep(letters[1:2], each = 5), value = 1:10),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$user_identifies <- "none"
    roles$user_sensitive <- FALSE
    roles$identifies <- "none"
    roles$sensitive <- FALSE
    roles <- dg_sync_roles_axes(roles)
    state$roles <- roles
  })

  toy_synthetic <- structure(
    data.frame(qi = rep(NA_character_, 10), value = 1:10),
    class = c("dataganger_synthetic", "data.frame")
  )
  attr(toy_synthetic, "kanon") <- list(
    qi_cols = "qi",
    k = 5L,
    smallest_cell = 10L,
    suppressed_cells = 2L,
    suppressed_rows = 10L,
    suppressed_row_frac = 1,
    infeasible = FALSE
  )
  toy_privacy <- tibble::tibble(
    variable = character(0), flag = character(0),
    severity = character(0), recommendation = character(0)
  )
  class(toy_privacy) <- c("dataganger_privacy_check", class(toy_privacy))
  attr(toy_privacy, "exact_row_matches") <- 0L
  result <- list(
    synthetic = toy_synthetic,
    comparison = structure(list(), class = "dataganger_comparison"),
    privacy = toy_privacy,
    warnings = character(0),
    kanon = attr(toy_synthetic, "kanon", exact = TRUE)
  )

  testthat::local_mocked_bindings(run_synthesis_pipeline = function(...) result)
  withr::local_options(dataganger.synthesis_async = FALSE)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    html <- paste(as.character(output$generate_actions), collapse = "\n")
    expect_match(html, "100% of QI values suppressed", fixed = TRUE)
    expect_match(html, "k-anonymity was applied", fixed = TRUE)
  })
})

test_that("generate renders the structured k-anon panel after an infeasible run", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(
      age = c(30, 31, 32),
      sex = c("F", "M", "F"),
      education = c("A", "B", "A"),
      smoker = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$identifies[] <- "none"
    roles$sensitive[] <- FALSE
    roles$identifies[roles$variable %in% c("age", "sex", "education", "smoker")] <- "combination"
    state$roles <- dg_sync_roles_axes(roles)
  })

  toy_synthetic <- structure(
    shiny::isolate(state$raw_data),
    class = c("dataganger_synthetic", "data.frame")
  )
  attr(toy_synthetic, "kanon") <- list(
    qi_cols = c("age", "sex", "education", "smoker"),
    k = 5L,
    smallest_cell = 1L,
    suppressed_cells = 0L,
    infeasible = TRUE
  )
  toy_privacy <- tibble::tibble()
  class(toy_privacy) <- c("dataganger_privacy_check", class(toy_privacy))
  attr(toy_privacy, "exact_row_matches") <- 0L
  testthat::local_mocked_bindings(
    run_synthesis_pipeline = function(...) {
      list(
        synthetic = toy_synthetic,
        comparison = structure(list(), class = "dataganger_comparison"),
        privacy = toy_privacy,
        warnings = paste(
          "Could not apply k-anonymity at k = 5 to the selected quasi-identifier (QI) columns: age, sex, education, smoker.",
          "To avoid destroying the dataset, no k-anonymity protection was applied to this output."
        ),
        kanon = attr(toy_synthetic, "kanon", exact = TRUE)
      )
    },
    kanon_escape_routes = function(...) {
      list(
        qi_cols = c("age", "sex", "education", "smoker"),
        feasible_k = 3L,
        feasible_k_suppressed_cells = 29L,
        suggested_n = 1000L,
        suggested_n_suppressed_cells = 49L,
        skipped_n_probe = FALSE,
        driver_col = "age"
      )
    }
  )
  recorder <- capture_notifications()
  withr::local_options(
    dataganger.synthesis_async = FALSE,
    recorder$options
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    html <- paste(as.character(output$generate_actions), collapse = "\n")
    expect_match(html, "k-anonymity was not applied", fixed = TRUE)
    expect_match(html, "background:var(--risk-50", fixed = TRUE)
    expect_match(html, "Apply k = 3 and regenerate", fixed = TRUE)
    expect_match(html, "Generate 1000 rows at k = 5", fixed = TRUE)
    expect_match(html, "Change k in Advanced settings", fixed = TRUE)
    expect_match(html, "adjust_advanced_settings", fixed = TRUE)
    expect_match(html, "synthesis_controls-advanced_settings", fixed = TRUE)
    expect_match(html, "age", fixed = TRUE)
  })

  notifications <- recorder$get()
  infeasible <- vapply(
    notifications, function(x) is_kanon_infeasible_warning(x$ui), logical(1)
  )
  expect_false(
    any(infeasible),
    info = paste("Infeasible notification indices:",
                 paste(which(infeasible), collapse = ", "))
  )
})

test_that("advanced-settings action requests Configure", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(x = 1:3),
    spec = synth_spec(purpose = "development")
  )

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(adjust_advanced_settings = 1L)
    session$flushReact()
  })

  expect_identical(shiny::isolate(state$nav_request), "configure")
})

test_that("generate k-action button updates k and stores provenance before regenerating", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(
      age = c(30, 31, 32),
      sex = c("F", "M", "F"),
      education = c("A", "B", "A"),
      smoker = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$identifies[] <- "none"
    roles$sensitive[] <- FALSE
    roles$identifies[roles$variable %in% c("age", "sex", "education", "smoker")] <- "combination"
    state$roles <- dg_sync_roles_axes(roles)
  })

  calls <- list()
  testthat::local_mocked_bindings(
    run_synthesis_pipeline = function(data, spec, roles) {
      calls[[length(calls) + 1L]] <<- list(k = spec$k_anon %||% 5L, n = spec$n %||% nrow(data))
      out <- structure(data, class = c("dataganger_synthetic", "data.frame"))
      if ((spec$k_anon %||% 5L) == 3L) {
        attr(out, "kanon") <- list(
          qi_cols = c("age", "sex", "education", "smoker"),
          k = 3L,
          smallest_cell = 3L,
          suppressed_cells = 29L,
          infeasible = FALSE
        )
      } else {
        attr(out, "kanon") <- list(
          qi_cols = c("age", "sex", "education", "smoker"),
          k = 5L,
          smallest_cell = 1L,
          suppressed_cells = 0L,
          infeasible = TRUE
        )
      }
      privacy <- tibble::tibble()
      class(privacy) <- c("dataganger_privacy_check", class(privacy))
      attr(privacy, "exact_row_matches") <- 0L
      list(
        synthetic = out,
        comparison = structure(list(), class = "dataganger_comparison"),
        privacy = privacy,
        warnings = character(0),
        kanon = attr(out, "kanon", exact = TRUE)
      )
    },
    kanon_escape_routes = function(...) {
      list(
        qi_cols = c("age", "sex", "education", "smoker"),
        feasible_k = 3L,
        feasible_k_suppressed_cells = 29L,
        suggested_n = 1000L,
        suggested_n_suppressed_cells = 49L,
        skipped_n_probe = FALSE,
        driver_col = "age"
      )
    }
  )
  withr::local_options(dataganger.synthesis_async = FALSE)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    session$setInputs(apply_escape_k = 1L)
    session$flushReact()
  })

  expect_equal(length(calls), 2L)
  expect_identical(calls[[2]]$k, 3L)
  expect_identical(shiny::isolate(state$spec$k_anon), 3L)
  expect_identical(shiny::isolate(state$k_anon), 3L)
  expect_identical(shiny::isolate(state$kanon$k_default), 5L)
  expect_identical(shiny::isolate(state$kanon$k_provenance), "user_selected_after_infeasible")
})

test_that("generate n-action button updates the row count before regenerating", {
  testthat::skip_if_not_installed("shiny")

  state <- generate_test_state(
    data = data.frame(
      age = c(30, 31, 32),
      sex = c("F", "M", "F"),
      education = c("A", "B", "A"),
      smoker = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    spec = synth_spec(purpose = "development", seed = 1L)
  )
  shiny::isolate({
    roles <- state$roles
    roles$identifies[] <- "none"
    roles$sensitive[] <- FALSE
    roles$identifies[roles$variable %in% c("age", "sex", "education", "smoker")] <- "combination"
    state$roles <- dg_sync_roles_axes(roles)
  })

  calls <- list()
  testthat::local_mocked_bindings(
    run_synthesis_pipeline = function(data, spec, roles) {
      calls[[length(calls) + 1L]] <<- list(k = spec$k_anon %||% 5L, n = spec$n %||% nrow(data))
      out <- structure(data, class = c("dataganger_synthetic", "data.frame"))
      attr(out, "kanon") <- list(
        qi_cols = c("age", "sex", "education", "smoker"),
        k = spec$k_anon %||% 5L,
        smallest_cell = if ((spec$n %||% nrow(data)) >= 1000L) 5L else 1L,
        suppressed_cells = if ((spec$n %||% nrow(data)) >= 1000L) 49L else 0L,
        infeasible = (spec$n %||% nrow(data)) < 1000L
      )
      privacy <- tibble::tibble()
      class(privacy) <- c("dataganger_privacy_check", class(privacy))
      attr(privacy, "exact_row_matches") <- 0L
      list(
        synthetic = out,
        comparison = structure(list(), class = "dataganger_comparison"),
        privacy = privacy,
        warnings = character(0),
        kanon = attr(out, "kanon", exact = TRUE)
      )
    },
    kanon_escape_routes = function(...) {
      list(
        qi_cols = c("age", "sex", "education", "smoker"),
        feasible_k = 3L,
        feasible_k_suppressed_cells = 29L,
        suggested_n = 1000L,
        suggested_n_suppressed_cells = 49L,
        skipped_n_probe = FALSE,
        driver_col = "age"
      )
    }
  )
  withr::local_options(dataganger.synthesis_async = FALSE)

  shiny::testServer(mod_generate_server, args = list(state = state), {
    session$setInputs(generate = 1L)
    session$flushReact()
    session$setInputs(apply_escape_n = 1L)
    session$flushReact()
  })

  expect_equal(length(calls), 2L)
  expect_identical(calls[[2]]$n, 1000L)
  expect_identical(shiny::isolate(state$spec$n), 1000L)
})
})
