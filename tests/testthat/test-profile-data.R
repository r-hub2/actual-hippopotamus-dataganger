test_that("profile_data() returns correct S3 class", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  p <- profile_data(df)
  expect_s3_class(p, "dataganger_profile")
  expect_named(p, c("profile", "n_rows", "n_cols", "coverage", "generated_at"))
  expect_equal(p$n_rows, 5)
  expect_equal(p$n_cols, 2)
})

test_that("profile_data() profiles numeric columns", {
  df <- data.frame(val = c(1, 2, 3, NA, 5))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "val", ]
  expect_equal(r$type, "numeric")
  expect_equal(r$n_missing, 1)
  expect_equal(r$pct_missing, 20)
  expect_equal(r$n_distinct, 4)
  expect_equal(r$min, 1)
  expect_equal(r$max, 5)
  expect_equal(r$mean, mean(c(1, 2, 3, 5)))
})

test_that("profile_data() profiles character columns", {
  df <- data.frame(txt = c("hello", "world", NA, "foo", "bar"))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "txt", ]
  expect_equal(r$type, "character")
  expect_equal(r$n_missing, 1)
  expect_equal(r$n_distinct, 4)
})

test_that("profile_data() profiles factor columns", {
  df <- data.frame(f = factor(c("a", "b", "a", NA, "a")))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "f", ]
  expect_equal(r$type, "factor")
  expect_equal(r$n_missing, 1)
  expect_equal(r$n_distinct, 2)
  expect_equal(r$most_common_level, "a")
  expect_equal(r$most_common_n, 3)
})

test_that("profile_data() profiles logical columns", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE, NA, TRUE))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "flag", ]
  expect_equal(r$type, "logical")
  expect_equal(r$n_missing, 1)
  expect_equal(r$n_true, 3)
  expect_equal(r$pct_true, 75)
})

test_that("profile_data() profiles Date columns", {
  df <- data.frame(d = as.Date(c("2024-01-01", "2024-06-15", NA)))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "d", ]
  expect_equal(r$type, "Date")
  expect_equal(r$n_missing, 1)
  expect_s3_class(r$min_date, "Date")
  expect_s3_class(r$max_date, "Date")
})

test_that("profile_data() profiles POSIXct columns", {
  df <- data.frame(
    ts = as.POSIXct(c("2024-01-01 12:00:00", "2024-06-15 08:30:00", NA))
  )
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "ts", ]
  expect_equal(r$type, "POSIXct")
  expect_equal(r$n_missing, 1)
  expect_s3_class(r$min_datetime, "POSIXct")
})

test_that("profile_data() profiles haven_labelled columns", {
  df <- data.frame(
    status = haven::labelled(
      c(1, 2, 1, NA, 2),
      labels = c(Active = 1, Inactive = 2),
      label = "Status"
    ),
    stringsAsFactors = FALSE
  )
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "status", ]
  expect_equal(r$type, "haven_labelled")
  expect_equal(r$n_missing, 1)
  expect_equal(r$n_labels, 2)
})

test_that("profile_data() profiles all-NA columns", {
  df <- data.frame(x = c(NA_real_, NA_real_, NA_real_))
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "x", ]
  expect_equal(r$type, "all_NA")
  expect_equal(r$n_missing, 3)
  expect_equal(r$pct_missing, 100)
  expect_equal(r$n_distinct, 0)
})

test_that("profile_data() flags free-text columns", {
  # 30 unique long strings, each > 50 characters
  long_strings <- sprintf(
    "very_long_unique_text_for_testing_free_text_detection_number_%030d",
    1:30
  )
  # 30 unique long strings, 10 NAs -> n_distinct=30, nrow=40
  df <- data.frame(
    notes = c(long_strings, rep(NA_character_, 10)),
    stringsAsFactors = FALSE
  )
  p <- profile_data(df)
  r <- p$profile[p$profile$variable == "notes", ]
  expect_true(r$is_free_text)
})

test_that("profile_data() handles mixed column types", {
  df <- data.frame(
    num = 1:10,
    chr = letters[1:10],
    fac = factor(rep(c("x", "y"), 5)),
    lgl = rep(c(TRUE, FALSE), 5),
    stringsAsFactors = FALSE
  )
  p <- profile_data(df)
  types <- p$profile$type
  expect_true("numeric" %in% types)
  expect_true("character" %in% types)
  expect_true("factor" %in% types)
  expect_true("logical" %in% types)
})

test_that("profile_data() rejects non-data-frame input", {
  expect_error(profile_data("not a data frame"), "must be a data frame")
  expect_error(profile_data(matrix(1:4, nrow = 2)), "must be a data frame")
})

test_that("print.dataganger_profile works without error", {
  df <- data.frame(x = 1:3)
  p <- profile_data(df)
  expect_s3_class(p, "dataganger_profile")
  expect_no_error(print(p))
})
