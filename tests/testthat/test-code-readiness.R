test_that("check_code_readiness() returns correct S3 class and structure", {
  orig <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  syn  <- data.frame(x = 11:20, y = letters[1:10], stringsAsFactors = FALSE)
  r <- check_code_readiness(orig, syn)
  expect_s3_class(r, "dataganger_code_readiness")
  expect_named(r, c("checks", "summary", "meta"))
  expect_s3_class(r$checks, "tbl_df")
  expect_named(r$checks, c("check", "scope", "column", "status", "message"))
  expect_true(is.list(r$summary))
  expect_named(r$summary, c("n_pass", "n_warn", "n_fail", "ready"))
  expect_true(is.list(r$meta))
})

test_that("check_code_readiness() passes on matching frames", {
  orig <- data.frame(x = 1:10, y = factor(letters[1:10]), stringsAsFactors = FALSE)
  syn  <- data.frame(x = 11:20, y = factor(letters[1:10]), stringsAsFactors = FALSE)
  r <- check_code_readiness(orig, syn)
  expect_equal(r$summary$n_fail, 0L)
  expect_true(r$summary$ready)
})

test_that("check_code_readiness() fails when synthetic is missing columns", {
  orig <- data.frame(x = 1:5, y = 1:5, z = 1:5)
  syn  <- data.frame(x = 1:5)
  r <- check_code_readiness(orig, syn)
  fail_rows <- r$checks[r$checks$status == "fail", ]
  expect_true(
    any(fail_rows$check == "column_names_match"),
    info = paste("Failing checks:", paste(fail_rows$check, collapse = ", "))
  )
  expect_false(r$summary$ready)
})

test_that("check_code_readiness() warns on extra synthetic columns", {
  orig <- data.frame(x = 1:5)
  syn  <- data.frame(x = 1:5, z = 1:5)
  r <- check_code_readiness(orig, syn)
  warn_rows <- r$checks[r$checks$status == "warn", ]
  expect_true(
    any(warn_rows$check == "no_extra_columns"),
    info = paste("Warning checks:", paste(warn_rows$check, collapse = ", "))
  )
})

test_that("check_code_readiness() fails on class mismatch", {
  orig <- data.frame(x = 1:10)
  syn  <- data.frame(x = as.character(1:10), stringsAsFactors = FALSE)
  r <- check_code_readiness(orig, syn)
  fail_rows <- r$checks[r$checks$status == "fail" & r$checks$check == "class_match", ]
  expect_true(nrow(fail_rows) > 0L)
  expect_equal(fail_rows$column[1], "x")
})


test_that("check_code_readiness() notes labelled to character as expected for now", {
  orig <- tibble::tibble(x = haven::labelled(1:5, labels = c(A = 1, B = 2)))
  syn <- data.frame(x = as.character(1:5), stringsAsFactors = FALSE)
  r <- check_code_readiness(orig, syn)
  fail_rows <- r$checks[r$checks$status == "fail" & r$checks$check == "class_match", ]
  expect_match(fail_rows$message[1], "expected for now")
})
test_that("check_code_readiness() fails on all-NA synthetic column", {
  orig <- data.frame(x = 1:10, y = 1:10)
  syn  <- data.frame(x = 11:20, y = rep(NA_real_, 10))
  r <- check_code_readiness(orig, syn)
  fail_rows <- r$checks[r$checks$status == "fail" & r$checks$check == "all_na", ]
  expect_true(nrow(fail_rows) > 0L)
  expect_equal(fail_rows$column[1], "y")
})

test_that("check_code_readiness() warns on zero-variance synthetic column", {
  orig <- data.frame(x = 1:10)
  syn  <- data.frame(x = rep(1L, 10))
  r <- check_code_readiness(orig, syn)
  warn_rows <- r$checks[r$checks$status == "warn" & r$checks$check == "zero_variance", ]
  expect_true(nrow(warn_rows) > 0L)
})

test_that("check_code_readiness() warns on missing factor levels", {
  orig <- data.frame(x = factor(c("a", "b", "c", "a", "b")))
  # synthetic only has levels a and b
  syn  <- data.frame(x = factor(c("a", "b", "a", "b", "a"),
                                 levels = c("a", "b")))
  r <- check_code_readiness(orig, syn)
  warn_rows <- r$checks[r$checks$status == "warn" & r$checks$check == "factor_levels", ]
  expect_true(nrow(warn_rows) > 0L)
})

test_that("check_code_readiness() warns on missingness spike", {
  orig <- data.frame(x = 1:20)          # 0% NA
  syn  <- data.frame(x = c(1:10, rep(NA_real_, 10)))  # 50% NA - just at threshold, warn
  r <- check_code_readiness(orig, syn)
  # 50% NA is > 50% threshold, should warn
  # Actually 10/20 = 50% which is not > 50%, so let's use 11 NAs
  orig2 <- data.frame(x = 1:20)
  syn2  <- data.frame(x = c(1:9, rep(NA_real_, 11)))  # 55% NA
  r2 <- check_code_readiness(orig2, syn2)
  warn_rows <- r2$checks[r2$checks$status == "warn" & r2$checks$check == "missingness_spike", ]
  expect_true(nrow(warn_rows) > 0L)
})

test_that("check_code_readiness() warns on duplicate IDs in synthetic", {
  # Create a column that detect_roles() will flag as ID candidate
  orig <- data.frame(record_id = paste0("R", 1:25), stringsAsFactors = FALSE)
  # Synthetic has duplicates - a join would give wrong row counts
  syn  <- data.frame(record_id = c(paste0("R", 1:20), paste0("R", 1:5)),
                     stringsAsFactors = FALSE)
  roles <- detect_roles(orig)
  r <- check_code_readiness(orig, syn, roles = roles)
  warn_rows <- r$checks[r$checks$status == "warn" & r$checks$check == "id_uniqueness", ]
  expect_true(nrow(warn_rows) > 0L)
})

test_that("print.dataganger_code_readiness() works without error", {
  orig <- data.frame(x = 1:10)
  syn  <- data.frame(x = 11:20)
  r <- check_code_readiness(orig, syn)
  expect_no_error(print(r))
})

test_that("check_code_readiness() rejects non-data-frame inputs", {
  expect_error(check_code_readiness("not a df", data.frame()), "must be a data frame")
  expect_error(check_code_readiness(data.frame(), "not a df"), "must be a data frame")
})
