test_that("run_synthesis_pipeline returns synthetic, comparison, and privacy", {
  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "development", seed = 1)

  result <- run_synthesis_pipeline(example_health_survey, spec)

  expect_named(result, c("synthetic", "comparison", "privacy", "warnings", "kanon"))
  expect_s3_class(result$synthetic, "dataganger_synthetic")
  expect_s3_class(result$comparison, "dataganger_comparison")
  expect_s3_class(result$privacy, "dataganger_privacy_check")
})

test_that("synthesis_dev_loaded() returns a single logical", {
  # Returns TRUE under devtools::test() (load_all), FALSE under R CMD check.
  result <- synthesis_dev_loaded()
  expect_type(result, "logical")
  expect_length(result, 1L)
})

test_that("start_synthesis_process runs the pipeline in a background process", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_on_cran()
  # The subprocess loads dataganger from the library; under devtools::load_all
  # the package isn't installed, so this can only run against an installed build.
  testthat::skip_if(
    synthesis_dev_loaded(),
    "dataganger is dev-loaded, not installed (subprocess can't find it)"
  )

  spec <- synth_spec(purpose = "development", seed = 1)
  # Need >=2 columns: purpose="development" auto-routes to synthpop, which
  # rejects single-column input ("Data should contain at least two columns").
  handle <- start_synthesis_process(data.frame(x = 1:50, y = rnorm(50)), spec)
  on.exit(if (handle$is_alive()) handle$kill(), add = TRUE)

  handle$wait() # block until the subprocess finishes
  result <- handle$get_result()
  expect_named(result, c("synthetic", "comparison", "privacy", "warnings", "kanon"))
  expect_s3_class(result$synthetic, "dataganger_synthetic")
})


test_that("run_synthesis_pipeline blocks incomplete role answers", {
  spec <- synth_spec(purpose = "development", seed = 1)
  roles <- data.frame(
    variable = c("zip", "score"),
    identifies = c("", "none"),
    sensitive = c(FALSE, FALSE),
    simulation = c("synthesize", "synthesize"),
    stringsAsFactors = FALSE
  )

  expect_error(
    run_synthesis_pipeline(data.frame(zip = c("100", "200"), score = c(1, 2)), spec, roles = roles),
    "privacy questions"
  )
})

test_that("run_synthesis_pipeline carries infeasible k-anon warnings and metadata", {
  df <- data.frame(
    qi_a = sprintf("a%03d", 1:100),
    qi_b = sprintf("b%03d", 1:100),
    value = seq_len(100),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$user_identifies <- "combination"
  roles$user_sensitive <- FALSE
  roles$identifies <- "combination"
  roles$sensitive <- FALSE
  roles$disclosure_role <- "quasi"
  roles$simulation <- c("pass_through", "pass_through", "synthesize")
  # Pin the internal engine: this tests pipeline surfacing, not synthpop, and
  # the 3-column fixture leaves synthpop too few columns after exclusions.
  spec <- synth_spec(purpose = "development", seed = 9, n = 100, k_anon = 5,
                     engine = "internal")

  result <- run_synthesis_pipeline(df, spec, roles = roles)

  expect_true(isTRUE(result$kanon$infeasible))
  expect_match(paste(result$warnings, collapse = "\n"),
               "Could not apply k-anonymity", fixed = TRUE)
})
