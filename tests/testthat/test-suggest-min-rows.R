test_that("suggest_min_rows() rejects non-profile input", {
  expect_error(suggest_min_rows(list()), class = "rlang_error")
})

test_that("small inputs keep the original row count", {
  p <- profile_data(data.frame(g = rep(letters[1:3], 4), x = 1:12))
  res <- suggest_min_rows(p, threshold = 1000L)

  expect_equal(res$n, 12L)
  expect_equal(res$original_n, 12L)
  expect_false(res$reduced)
})

test_that("large inputs are reduced to the observed combination coverage", {
  set.seed(1)
  df <- data.frame(
    g1 = sample(letters[1:4], 2000, replace = TRUE),
    g2 = sample(LETTERS[1:5], 2000, replace = TRUE),
    x  = rnorm(2000)
  )
  p <- profile_data(df)
  res <- suggest_min_rows(p, threshold = 1000L)

  combos <- p$coverage$combination_count
  expect_equal(res$n, as.integer(combos))
  expect_true(res$reduced)
  expect_true(res$n < res$original_n)
  expect_false(res$capped)
})

test_that("combination coverage is capped", {
  set.seed(2)
  df <- data.frame(
    g1 = sample(letters, 6000, replace = TRUE),
    g2 = sample(LETTERS, 6000, replace = TRUE)
  )
  p <- profile_data(df)
  res <- suggest_min_rows(p, threshold = 1000L, cap = 100L)

  expect_lte(res$n, 100L)
  expect_true(res$capped)
})

test_that("no low-cardinality columns falls back to the original count", {
  df <- data.frame(id = sprintf("id-%04d", 1:1500))
  p <- profile_data(df)
  res <- suggest_min_rows(p, threshold = 1000L)

  expect_equal(res$n, 1500L)
  expect_true(is.na(res$combination_count))
})
