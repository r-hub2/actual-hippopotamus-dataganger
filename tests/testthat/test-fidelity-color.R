test_that("fidelity_color maps p-values to good/warn/bad and passes NA through", {
  expect_equal(fidelity_color(0.001), "bad")
  expect_equal(fidelity_color(0.03), "warn")
  expect_equal(fidelity_color(0.5), "good")
  expect_equal(fidelity_color(NA_real_), "none")
  expect_equal(fidelity_color(0.01), "warn")
  expect_equal(fidelity_color(0.05), "good")
})
