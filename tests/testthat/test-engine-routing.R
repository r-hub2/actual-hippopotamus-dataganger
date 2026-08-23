test_that("engine_from_correlations() maps correlation settings to engines", {
  expect_equal(engine_from_correlations(list(preserve_correlations = "none")), "internal")
  expect_equal(engine_from_correlations(list(preserve_correlations = "low")), "internal")
  expect_equal(engine_from_correlations(list(preserve_correlations = "moderate")), "synthpop")
  expect_equal(engine_from_correlations(list(preserve_correlations = "high")), "synthpop")
  expect_equal(engine_from_correlations(list(preserve_correlations = NULL)), "internal")
  expect_equal(engine_from_correlations(list()), "internal")
})

test_that("engine_from_correlations() routes objective presets", {
  expected <- c(
    demo        = "internal",
    development = "synthpop",
    analytics   = "synthpop"
  )

  for (purpose in names(expected)) {
    spec <- synth_spec(
      purpose = purpose,
      acknowledge_risk = identical(purpose, "analytics")
    )
    expect_equal(engine_from_correlations(spec), expected[[purpose]], info = purpose)
  }
})
