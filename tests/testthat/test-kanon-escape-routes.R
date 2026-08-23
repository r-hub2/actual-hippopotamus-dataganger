local({
escape_route_roles <- function(data, qi_cols) {
  roles <- detect_roles(data)
  roles$identifies[] <- "none"
  roles$sensitive[] <- FALSE
  roles$identifies[roles$variable %in% qi_cols] <- "combination"
  dg_sync_roles_axes(roles)
}

test_that("kanon_escape_routes finds the largest feasible k and the driver column", {
  data("individual_sample", package = "dataganger")
  roles <- escape_route_roles(
    individual_sample,
    c("age", "sex", "education", "smoker")
  )

  routes <- kanon_escape_routes(individual_sample, roles, 5)

  expect_equal(routes$feasible_k, 3)
  # NA coarsening now merges into the existing <NA> bucket, so fewer
  # suppression events are needed; the privacy guarantee is unchanged.
  expect_equal(routes$feasible_k_suppressed_cells, 12)
  expect_identical(routes$driver_col, "age")
})

test_that("kanon_escape_routes probes larger row counts and returns the first feasible size", {
  data <- data.frame(
    qi = sprintf("q%03d", 1:200),
    value = seq_len(200),
    stringsAsFactors = FALSE
  )
  roles <- escape_route_roles(data, "qi")

  testthat::local_mocked_bindings(
    synthesize_data = function(data, spec, roles, ...) {
      out <- data.frame(qi = seq_len(spec$n), stringsAsFactors = FALSE)
      class(out) <- c("dataganger_synthetic", "data.frame")
      attr(out, "kanon") <- list(
        qi_cols = "qi",
        k = spec$k_anon,
        smallest_cell = if (spec$n >= 1000L) 5L else 1L,
        suppressed_cells = if (spec$n >= 1000L) 49L else 0L,
        infeasible = spec$n < 1000L
      )
      out
    }
  )

  routes <- kanon_escape_routes(data, roles, 5)

  expect_equal(routes$suggested_n, 1000L)
  expect_equal(routes$suggested_n_suppressed_cells, 49L)
})

test_that("kanon_escape_routes never suggests a k below 3", {
  data <- data.frame(
    qi = sprintf("q%03d", 1:200),
    value = seq_len(200),
    stringsAsFactors = FALSE
  )
  roles <- escape_route_roles(data, "qi")

  testthat::local_mocked_bindings(
    enforce_kanon = function(synthetic, roles, k = 5, ...) {
      attr(synthetic, "kanon") <- list(
        qi_cols = "qi",
        k = k,
        smallest_cell = if (k <= 3L) 3L else 1L,
        suppressed_cells = if (k <= 3L) 7L else 0L,
        infeasible = k > 3L
      )
      synthetic
    },
    synthesize_data = function(data, spec, roles, ...) {
      out <- data
      class(out) <- c("dataganger_synthetic", "data.frame")
      attr(out, "kanon") <- list(
        qi_cols = "qi",
        k = spec$k_anon,
        smallest_cell = 1L,
        suppressed_cells = 0L,
        infeasible = TRUE
      )
      out
    }
  )

  routes <- kanon_escape_routes(data, roles, 9)

  expect_equal(routes$feasible_k, 3)
  expect_null(routes$suggested_n)
})

test_that("kanon_escape_routes skips row-count probes above 50k rows", {
  data <- data.frame(qi = seq_len(50001), stringsAsFactors = FALSE)
  roles <- escape_route_roles(data, "qi")

  testthat::local_mocked_bindings(
    synthesize_data = function(...) {
      stop("row-count probe should have been skipped", call. = FALSE)
    }
  )

  routes <- kanon_escape_routes(data, roles, 5)

  expect_true(isTRUE(routes$skipped_n_probe))
  expect_null(routes$suggested_n)
  expect_null(routes$suggested_n_suppressed_cells)
})
})
