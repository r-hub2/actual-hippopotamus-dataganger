
test_that("assess_kanonymity counts records in cells smaller than k", {
  df <- data.frame(
    zip = c(rep("A", 8), "B", "B"),
    sex = c(rep("F", 4), rep("M", 4), "F", "M"),
    stringsAsFactors = FALSE
  )
  res <- assess_kanonymity(df, qi_cols = c("zip", "sex"), k = 5)

  expect_equal(res$smallest_cell, 1L)
  expect_equal(res$n_below, 10L)
  expect_equal(res$pct_below, 100)
  expect_true(nrow(res$worst_cells) >= 1)
  expect_equal(min(res$worst_cells$n), 1L)
})

test_that("assess_kanonymity handles no QI columns", {
  df <- data.frame(x = 1:10)
  res <- assess_kanonymity(df, qi_cols = character(0), k = 5)
  expect_true(res$no_qi)
  expect_equal(res$n_below, 0L)
})

test_that("assess_kanonymity treats all-unique combinations as fully unsafe", {
  df <- data.frame(a = 1:10, b = letters[1:10], stringsAsFactors = FALSE)
  res <- assess_kanonymity(df, qi_cols = c("a", "b"), k = 5)
  expect_equal(res$smallest_cell, 1L)
  expect_equal(res$n_below, 10L)
})

test_that("assess_kanonymity counts NA as its own combination level", {
  df <- data.frame(
    zip = c(rep("A", 6), NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  res <- assess_kanonymity(df, qi_cols = "zip", k = 5)
  expect_equal(res$n_below, 4L)
})

test_that("looks_aggregated flags count-style tables and clears plain microdata", {
  agg <- data.frame(
    region = c("N", "S", "E", "W"),
    age_band = c("0-18", "19-65", "0-18", "19-65"),
    n = c(120L, 340L, 88L, 210L),
    stringsAsFactors = FALSE
  )
  expect_true(looks_aggregated(agg)$aggregated)

  micro <- data.frame(
    id = 1:100, age = sample(20:80, 100, TRUE), x = rnorm(100)
  )
  expect_false(looks_aggregated(micro)$aggregated)
})

# Regression: kanon_key() used to paste quasi-identifier values together under
# a "\u0001" separator, with NA rewritten to the literal "<NA>". Both are
# values that can occur in real data, so two genuinely distinct combinations
# could collide into one -- and a collision always makes a cell look BIGGER
# than it is, i.e. it understates disclosure risk. The tests below are built so
# that the collided cell clears k while the true cells do not.

test_that("assess_kanonymity does not merge combinations that collide on the separator", {
  df <- data.frame(
    a = c(rep("x\u0001y", 3), rep("x", 2), rep("p", 6)),
    b = c(rep("z", 3), rep("y\u0001z", 2), rep("q", 6)),
    stringsAsFactors = FALSE
  )
  res <- assess_kanonymity(df, qi_cols = c("a", "b"), k = 5)

  # Under the old keying the first five rows share one key, so the cell reads
  # as size 5 and nothing is below k.
  expect_equal(res$smallest_cell, 2L)
  expect_equal(res$n_below, 5L)
})

test_that("assess_kanonymity does not merge real NA with the literal string <NA>", {
  df <- data.frame(
    zip = c(rep("A", 6), NA, NA, NA, "<NA>", "<NA>"),
    stringsAsFactors = FALSE
  )
  res <- assess_kanonymity(df, qi_cols = "zip", k = 5)

  # Old keying: three NA plus two "<NA>" become one cell of 5, clearing k.
  expect_equal(res$smallest_cell, 2L)
  expect_equal(res$n_below, 5L)
})

test_that("enforce_kanon suppresses combinations that collide on the separator", {
  df <- data.frame(
    a = c(rep("x\u0001y", 3), rep("x", 2), rep("p", 6)),
    b = c(rep("z", 3), rep("y\u0001z", 2), rep("q", 6)),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("a", "b"),
    disclosure_role = c("quasi", "quasi"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(df, roles = roles, k = 5)
  rep <- attr(out, "kanon")

  # The colliding rows are genuinely below k, so enforcement must act on them
  # rather than pass them through as one safe cell.
  expect_true(rep$suppressed_rows > 0L)
})
