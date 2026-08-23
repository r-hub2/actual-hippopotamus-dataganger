test_that("data panel compare mode renders a compare table output", {
  testthat::skip_if_not_installed("shiny")

  state <- shiny::reactiveValues(
    raw_data = data.frame(a = 1:3),
    synthetic = data.frame(a = 3:1),
    compare_selected_var = "a",
    active_step = "compare",
    seed_used = 1L
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()
    body_html <- paste(as.character(output$dp_body), collapse = "\n")
    expect_match(body_html, "dp_compare_table")
    expect_match(body_html, "Row-by-row")
  })
})

test_that("data panel flags exact-match rows for highlighting on both tabs", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  # 30 rows meets the >= 20 threshold; inject two verbatim copies at 3 and 7.
  original <- data.frame(
    a = sprintf("%02d", 1:30), b = rep(c("x", "y"), 15), stringsAsFactors = FALSE
  )
  synthetic <- data.frame(
    a = sprintf("%02d", 31:60), b = rep(c("y", "x"), 15), stringsAsFactors = FALSE
  )
  synthetic[c(3, 7), ] <- original[c(3, 7), ]

  state <- shiny::reactiveValues(
    raw_data = original, synthetic = synthetic, roles = NULL,
    compare_selected_var = NULL, active_step = "generate", seed_used = 1L
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()

    fl <- exact_match_detail_r()
    expect_false(is.null(fl))
    expect_equal(sum(fl$synthetic_severity > 0L), 2L)
    expect_true(
      all(fl$synthetic_severity[c(3, 7)] > 0L),
      info = paste("Synthetic severity:", paste(fl$synthetic_severity, collapse = ", "))
    )
    expect_equal(sum(fl$original_severity > 0L), 2L)
    expect_true(
      all(fl$original_severity[c(3, 7)] > 0L),
      info = paste("Original severity:", paste(fl$original_severity, collapse = ", "))
    )

    # roles = NULL means no column was marked sensitive in question 2, so the
    # rows are reproduced (amber, 1) but disclose nothing sensitive (never 2).
    expect_equal(unique(fl$synthetic_severity[c(3, 7)]), 1L)
    expect_equal(exact_match_sensitive_count(fl), 0L)

    # The table renders without error on both tabs (hidden flag column + row
    # style must not break DT).
    session$setInputs(active_tab = "synth")
    session$flushReact()
    expect_false(is.null(output$dp_table))

    session$setInputs(active_tab = "real")
    session$flushReact()
    expect_false(is.null(output$dp_table))
  })
})

test_that("data panel has no exact-match flags before synthesis", {
  testthat::skip_if_not_installed("shiny")

  state <- shiny::reactiveValues(
    raw_data = data.frame(a = sprintf("%02d", 1:30)), synthetic = NULL,
    roles = NULL, compare_selected_var = NULL, active_step = "configure",
    seed_used = NULL
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()
    expect_null(exact_match_detail_r())
  })
})

test_that("each new synthetic result switches the data panel to Synthetic", {
  testthat::skip_if_not_installed("shiny")

  state <- shiny::reactiveValues(
    raw_data = data.frame(a = 1:3),
    synthetic = NULL,
    compare_selected_var = NULL,
    active_step = "generate",
    seed_used = 1L,
    roles = NULL
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()

    state$synthetic <- data.frame(a = 3:1)
    session$flushReact()
    expect_match(paste(as.character(output$dp_body), collapse = "\n"), "seed = 1")

    session$setInputs(active_tab = "real")
    session$flushReact()
    expect_match(paste(as.character(output$dp_body), collapse = "\n"), "source dataset")

    state$seed_used <- 2L
    state$synthetic <- data.frame(a = 4:2)
    session$flushReact()
    expect_match(paste(as.character(output$dp_body), collapse = "\n"), "seed = 2")
  })
})

test_that("a reproduced row exposing a sensitive value is severity 2, not 1", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  original <- data.frame(
    a = sprintf("%02d", 1:30), dx = rep(c("flu", "cold"), 15),
    stringsAsFactors = FALSE
  )
  synthetic <- data.frame(
    a = sprintf("%02d", 31:60), dx = rep(c("cold", "flu"), 15),
    stringsAsFactors = FALSE
  )
  synthetic[c(3, 7), ] <- original[c(3, 7), ]

  roles <- data.frame(
    variable = c("a", "dx"), sensitive = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  state <- shiny::reactiveValues(
    raw_data = original, synthetic = synthetic, roles = roles,
    compare_selected_var = NULL, active_step = "generate", seed_used = 1L
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()
    d <- exact_match_detail_r()

    expect_equal(unique(d$synthetic_severity[c(3, 7)]), 2L)
    expect_equal(exact_match_sensitive_count(d), 2L)

    # Breakdown is k x p: 2 matched rows x 2 match columns.
    expect_equal(nrow(d$breakdown), 4L)
    expect_setequal(unique(d$breakdown$column), c("a", "dx"))
    expect_setequal(unique(d$breakdown$synthetic_row), c(3L, 7L))
    expect_true(
      all(d$breakdown$sensitive[d$breakdown$column == "dx"]),
      info = paste("Sensitive dx flags:",
                   paste(d$breakdown$sensitive[d$breakdown$column == "dx"], collapse = ", "))
    )
    expect_false(
      any(d$breakdown$sensitive[d$breakdown$column == "a"]),
      info = paste("Sensitive a flags:",
                   paste(d$breakdown$sensitive[d$breakdown$column == "a"], collapse = ", "))
    )

    # The reported original row really does hold the same record.
    b <- d$breakdown[1, ]
    expect_equal(
      unlist(synthetic[b$synthetic_row, ]),
      unlist(original[b$original_row, ])
    )
  })
})

test_that("the Exact matches tab appears only when there are matches", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  original <- data.frame(a = sprintf("%02d", 1:30), stringsAsFactors = FALSE)
  clean <- data.frame(a = sprintf("%02d", 31:60), stringsAsFactors = FALSE)
  dirty <- clean
  dirty[c(3, 7), ] <- original[c(3, 7), ]

  state <- shiny::reactiveValues(
    raw_data = original, synthetic = clean, roles = NULL,
    compare_selected_var = NULL, active_step = "generate", seed_used = 1L
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$flushReact()
    expect_false(grepl("Exact matches", paste(as.character(output$dp_tabs), collapse = "")))

    state$synthetic <- dirty
    session$flushReact()
    tabs <- paste(as.character(output$dp_tabs), collapse = "")
    expect_match(tabs, "Exact matches \\(2\\)")

    session$setInputs(active_tab = "matches")
    session$flushReact()
    body_html <- paste(as.character(output$dp_body), collapse = "\n")
    expect_match(body_html, "dp_matches_table")
    expect_false(is.null(output$dp_matches_table))
  })
})

test_that("the preview tables carry a row number column", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  state <- shiny::reactiveValues(
    raw_data = data.frame(a = 1:25), synthetic = data.frame(a = 26:50),
    roles = NULL, compare_selected_var = NULL, active_step = "generate",
    seed_used = 1L
  )

  shiny::testServer(mod_data_panel_server, args = list(state = state), {
    session$setInputs(active_tab = "real")
    session$flushReact()
    tbl <- output$dp_table
    expect_false(is.null(tbl))
    # The "#" column is the first column of the rendered payload.
    expect_match(paste(as.character(tbl), collapse = ""), '"#"')
  })
})
