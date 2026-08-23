test_that("exact: Case A preserves joint NA pattern row-for-row", {
  df <- data.frame(
    x = c(NA, 1,  NA, 2,  3),
    y = c(NA, NA, NA, 1,  2),
    z = c(1,  2,  3,  NA, 5)
  )
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = nrow(df))
  syn  <- synthesize_data(df, spec)
  expect_equal(which(is.na(syn$x)), which(is.na(df$x)))
  expect_equal(which(is.na(syn$y)), which(is.na(df$y)))
  expect_equal(which(is.na(syn$z)), which(is.na(df$z)))
})

test_that("exact: Case A preserves joint co-occurrence of NAs across columns", {
  df <- data.frame(
    x = c(NA, 1,  NA, 2,  3),
    y = c(NA, 2,  NA, 3,  4)
  )
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = nrow(df))
  syn  <- synthesize_data(df, spec)
  # rows 1 and 3 are jointly NA in both x and y
  joint_na_orig <- which(is.na(df$x) & is.na(df$y))
  joint_na_syn  <- which(is.na(syn$x) & is.na(syn$y))
  expect_equal(joint_na_syn, joint_na_orig)
})

test_that("exact: Case B preserves marginal NA rates within 5pp", {
  set.seed(42)
  df <- data.frame(
    x = c(NA, 1, NA, 2, 3, NA, 4, 5, NA, 6),
    y = c(1,  2,  3, NA, 5,  6, 7, NA, 9, 10)
  )
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = 1000, seed = 1)
  syn  <- synthesize_data(df, spec)
  expect_equal(mean(is.na(syn$x)), mean(is.na(df$x)), tolerance = 0.05)
  expect_equal(mean(is.na(syn$y)), mean(is.na(df$y)), tolerance = 0.05)
})

test_that("exact: Case B preserves conditional joint NA structure", {
  set.seed(99)
  # Construct data where NA in x always co-occurs with NA in y
  df <- data.frame(
    x = c(NA, NA, 1, 2, 3, 4, 5, 6, 7, 8),
    y = c(NA, NA, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = 500, seed = 2)
  syn  <- synthesize_data(df, spec)
  # If x is NA, y must also be NA (comes from same sampled row)
  x_na_rows <- which(is.na(syn$x))
  expect_true(
    all(is.na(syn$y[x_na_rows])),
    info = paste("Non-missing y rows in x-missing positions:",
                 paste(x_na_rows[!is.na(syn$y[x_na_rows])], collapse = ", "))
  )
})

test_that("exact: all-NA column remains all-NA", {
  df <- data.frame(x = c(NA, NA, NA), y = c(1, 2, 3))
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = nrow(df))
  syn  <- synthesize_data(df, spec)
  expect_true(
    all(is.na(syn$x)),
    info = paste("Non-missing x rows:", paste(which(!is.na(syn$x)), collapse = ", "))
  )
})

test_that("exact: column with no NAs has no NAs in output", {
  df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(NA, 1, NA, 2, NA))
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = nrow(df))
  syn  <- synthesize_data(df, spec)
  expect_false(
    any(is.na(syn$x)),
    info = paste("Missing x rows:", paste(which(is.na(syn$x)), collapse = ", "))
  )
})

test_that("exact: name_strategy generic does not corrupt NA pattern", {
  df <- data.frame(
    x = c(NA, 1, NA, 2, 3),
    y = c(NA, 2, NA, 3, 4)
  )
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact", n = nrow(df),
                     name_strategy = "generic")
  syn  <- synthesize_data(df, spec)
  # col_1 maps to x, col_2 maps to y
  expect_equal(which(is.na(syn$col_1)), which(is.na(df$x)))
  expect_equal(which(is.na(syn$col_2)), which(is.na(df$y)))
})

test_that("exact: does not throw not-implemented error", {
  df <- data.frame(x = c(NA, 1, 2), y = c(1, NA, 3))
  spec <- synth_spec(purpose = "demo", preserve_missingness = "exact")
  expect_no_error(synthesize_data(df, spec))
})
