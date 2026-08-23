local({
compare_test_state <- function(raw_data = NULL, synthetic = NULL,
                               comparison = NULL, privacy = NULL,
                               roles = NULL, stale = NULL) {
  shiny::reactiveValues(
    raw_data   = raw_data,
    synthetic  = synthetic,
    comparison = comparison,
    compare_selected_var = NULL,
    privacy    = privacy,
    roles      = roles,
    stale      = stale %||% list(comparison = FALSE),
    nav_request = NULL,
    active_step = NULL
  )
}

comparison_fixture <- function(seed = 1) {
  data("example_health_survey", package = "dataganger")

  spec <- synth_spec(purpose = "development", seed = seed)
  synthetic <- synthesize_data(example_health_survey, spec)
  roles <- detect_roles(example_health_survey)
  comparison <- compare_synthetic(
    example_health_survey,
    synthetic,
    roles = roles
  )
  privacy <- privacy_check(
    example_health_survey,
    synthetic,
    roles = roles,
    stage = "post",
    spec = spec
  )

  list(
    raw_data   = example_health_survey,
    synthetic  = synthetic,
    comparison = comparison,
    compare_selected_var = NULL,
    privacy    = privacy,
    roles      = roles
  )
}

test_that("mod_compare_ui has header, subtitle, export button, and stale banner", {
  testthat::skip_if_not_installed("shiny")

  ui   <- mod_compare_ui("compare")
  html <- paste(as.character(ui), collapse = "\n")

  expect_match(html, "Compare datasets")
  expect_match(html, "Step 05")
  expect_match(html, "go_export")
  expect_match(html, "stale__comparison")
})

test_that("compare subtitle defines delta and TVD", {
  html <- as.character(mod_compare_ui("compare"))
  expect_match(html, "TVD")
  expect_match(html, "total variation distance", ignore.case = TRUE)
  expect_match(html, "\u0394")
})

test_that("compare_body renders empty-state card when no synthetic data", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  state <- compare_test_state()

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    body_html <- paste(as.character(output$compare_body), collapse = "\n")
    expect_match(body_html, "Generate synthetic data first")
  })
})

test_that("compare_body renders var-rail and var-detail when data is present", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  fixture <- comparison_fixture(seed = 7)
  state   <- compare_test_state(
    raw_data  = fixture$raw_data,
    synthetic = fixture$synthetic,
    roles     = fixture$roles
  )

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    body_html <- paste(as.character(output$compare_body), collapse = "\n")
    expect_match(body_html, "compare-layout")
    expect_match(body_html, "var-rail")
    expect_match(body_html, "var-detail")
    expect_match(body_html, "Univariate")
    expect_match(body_html, "Bivariate")
    expect_match(body_html, "Predictor (X)", fixed = TRUE)
    expect_match(body_html, "Outcome (Y)", fixed = TRUE)
    expect_match(body_html, "Exploratory comparison")
    expect_match(body_html, "compare-exploratory-note")
    expect_match(body_html, "download the synthetic data", ignore.case = TRUE)
  })
})

test_that("bivariate outputs render all supported pair types", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  raw <- data.frame(
    x = 1:12,
    y = 2 * (1:12) + rep(c(0, 1), 6),
    group = rep(c("a", "b", "c"), each = 4),
    flag = rep(c(TRUE, FALSE), 6),
    stringsAsFactors = FALSE
  )
  synthetic <- raw
  synthetic$y <- rev(synthetic$y)
  synthetic$flag <- rep(c(TRUE, TRUE, FALSE), 4)
  roles <- detect_roles(raw)
  roles$user_role[roles$variable %in% c("x", "y")] <- "numeric"
  roles$user_role[roles$variable == "group"] <- "categorical"
  roles$user_role[roles$variable == "flag"] <- "logical"
  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    for (pair in list(c("x", "y"), c("x", "flag"))) {
      session$setInputs(rel_x = pair[[1]], rel_y = pair[[2]])
      session$flushReact()

      expect_no_error(output$rel_plot)
      if (identical(pair, c("x", "y"))) {
        plot_html <- paste(as.character(output$rel_plot), collapse = "\n")
        expect_match(plot_html, "LOESS smooth")
      }
      stats_html <- paste(as.character(output$rel_stats), collapse = "\n")
      expect_match(stats_html, "(Difference in correlation|Slope ratio|Odds ratio)")
      if (identical(pair, c("x", "y"))) {
        expect_match(stats_html, "Correlation difference p-value")
        expect_match(stats_html, "Fisher correlation comparison")
      } else {
        expect_match(stats_html, "interaction p-value", ignore.case = TRUE)
      }
      expect_match(stats_html, "fidelity-band")
    }
  })
})

test_that("compare_body excludes identifier variables from navigation", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  raw <- data.frame(
    record_id = sprintf("ID%03d", 1:30),
    age = 1:30,
    group = rep(c("a", "b"), 15),
    stringsAsFactors = FALSE
  )
  synthetic <- raw
  synthetic$age <- rev(synthetic$age)
  roles <- detect_roles(raw)
  roles$user_role[roles$variable == "record_id"] <- "identifier"
  roles$user_role[roles$variable == "age"] <- "numeric"

  state <- compare_test_state(
    raw_data = raw,
    synthetic = synthetic,
    roles = roles
  )

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    body_html <- paste(as.character(output$compare_body), collapse = "\n")
    expect_no_match(body_html, "record_id")
    expect_match(body_html, "age")
    expect_match(body_html, "group")
  })
})

test_that("compare_body excludes alpha-numeric ID variables from navigation", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  set.seed(1)
  raw <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, sample(1:99, 30, TRUE)),
    age = 1:30,
    group = rep(c("a", "b"), 15),
    stringsAsFactors = FALSE
  )
  synthetic <- raw
  synthetic$age <- rev(synthetic$age)
  roles <- detect_roles(raw)
  expect_equal(roles$recommended_role[roles$variable == "order_id"], "alphanumeric ID")

  state <- compare_test_state(
    raw_data = raw,
    synthetic = synthetic,
    roles = roles
  )

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    body_html <- paste(as.character(output$compare_body), collapse = "\n")
    expect_no_match(body_html, "order_id")
    expect_match(body_html, "age")
    expect_match(body_html, "group")
  })
})

test_that("go_export sets nav_request to export", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  state <- shiny::reactiveValues(
    raw_data    = NULL,
    synthetic   = NULL,
    comparison  = NULL,
    privacy     = NULL,
    roles       = NULL,
    stale       = list(comparison = FALSE),
    nav_request = NULL
  )

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$setInputs(go_export = 1L)
    session$flushReact()
  })

  expect_identical(shiny::isolate(state$nav_request), "export")
})

test_that("categorical comparison treats NA as an explicit missing level", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  raw <- data.frame(group = c("a", NA, NA), stringsAsFactors = FALSE)
  synthetic <- data.frame(group = c("a", "b", NA), stringsAsFactors = FALSE)
  roles <- detect_roles(raw)
  roles$user_role[roles$variable == "group"] <- "categorical"

  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    stats_html <- paste(as.character(output$var_stats), collapse = "\n")
    expect_match(stats_html, "TVD =")
    expect_no_match(stats_html, "NaN")
  })
})

test_that("geographic-named categorical columns render through the categorical comparison path", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  raw <- data.frame(region = c("north", "north", "south", NA), stringsAsFactors = FALSE)
  synthetic <- data.frame(region = c("north", "south", "south", NA), stringsAsFactors = FALSE)
  roles <- detect_roles(raw)
  expect_equal(roles$recommended_role[roles$variable == "region"], "categorical candidate")

  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    stats_html <- paste(as.character(output$var_stats), collapse = "\n")
    expect_match(stats_html, "TVD =")
    expect_no_match(stats_html, "excluded from distribution comparison")
  })
})

test_that("a categorical column with more distinct values than dg_max_comparable_levels is excluded with a warning", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  n <- 10
  expect_equal(dg_max_comparable_levels(n), 5L)

  # 10 distinct values across only 10 rows -- well above the limit of 5.
  raw <- data.frame(code = paste0("code_", 1:n), stringsAsFactors = FALSE)
  synthetic <- data.frame(code = paste0("code_", 1:n), stringsAsFactors = FALSE)
  roles <- detect_roles(raw)
  roles$user_role[roles$variable == "code"] <- "categorical"

  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()

    stats_html <- paste(as.character(output$var_stats), collapse = "\n")
    expect_match(stats_html, "Too many to compare reliably")
    expect_match(stats_html, "excluded from this page")
    expect_no_match(stats_html, "TVD =")

    plot <- output$var_plot
    expect_no_error(plot)
  })
})

test_that("a categorical column within dg_max_comparable_levels still renders normally", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  n <- 100
  expect_equal(dg_max_comparable_levels(n), 20L)

  # 3 distinct values across 100 rows -- comfortably under the limit.
  raw <- data.frame(group = rep(c("a", "b", "c"), length.out = n), stringsAsFactors = FALSE)
  synthetic <- data.frame(group = rep(c("a", "b", "c"), length.out = n), stringsAsFactors = FALSE)
  roles <- detect_roles(raw)
  roles$user_role[roles$variable == "group"] <- "categorical"

  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    stats_html <- paste(as.character(output$var_stats), collapse = "\n")
    expect_match(stats_html, "TVD =")
    expect_no_match(stats_html, "Too many to compare reliably")
  })
})

test_that("date comparison handles all-missing dates without Inf summaries", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  raw <- data.frame(visit_date = as.Date(c(NA, NA, NA)))
  synthetic <- data.frame(visit_date = as.Date(c(NA, NA, NA)))
  roles <- detect_roles(raw)
  roles$user_role[roles$variable == "visit_date"] <- "date"

  state <- compare_test_state(raw_data = raw, synthetic = synthetic, roles = roles)

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$flushReact()
    stats_html <- paste(as.character(output$var_stats), collapse = "\n")
    expect_match(stats_html, "\\(Missing\\)")
    expect_no_match(stats_html, "Inf")
    expect_no_match(stats_html, "NaN")
  })
})


test_that("compare publishes selected variable to shared state", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("plotly")

  state <- shiny::reactiveValues(
    raw_data = data.frame(a = 1:5, b = letters[1:5]),
    synthetic = data.frame(a = 5:1, b = letters[5:1]),
    comparison = NULL,
    compare_selected_var = NULL,
    privacy = NULL,
    roles = NULL,
    stale = list(comparison = FALSE),
    nav_request = NULL,
    active_step = NULL
  )

  shiny::testServer(mod_compare_server, args = list(state = state), {
    session$setInputs(var_select = "a")
    session$flushReact()
    expect_equal(state$compare_selected_var, "a")
  })
})

test_that("numeric comparison renders SMD/ratio labels and a p-value colour class", {
  cmp <- structure(list(numeric = data.frame(
    variable = "x", mean_orig = 10, mean_syn = 14, sd_orig = 2, sd_syn = 2,
    median_orig = 10, median_syn = 14, iqr_orig = 3, iqr_syn = 3,
    missing_orig_pct = 0, missing_syn_pct = 0, std_diff = 2,
    sd_ratio = 1, median_std_diff = 1.33, mean_p = 0.001, sd_p = 0.9, median_p = 0.001
  )), class = "dataganger_comparison")
  ui <- compare_numeric_table(cmp$numeric)
  html <- paste(as.character(ui), collapse = "\n")
  expect_match(html, "SMD|Std. diff", ignore.case = TRUE)
  expect_match(html, "ratio", ignore.case = TRUE)
  expect_match(html, "bad")
})
})
