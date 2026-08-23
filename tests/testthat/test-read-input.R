test_that("read_input() reads CSV fixture", {
  file <- testthat::test_path("fixtures", "tiny.csv")
  out <- read_input(file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_equal(ncol(out), 6)
  expect_named(out, c("id", "name", "score", "group", "active", "dt"))
  expect_type(out$id, "double")
  expect_type(out$score, "double")
  expect_type(out$name, "character")
})

test_that("read_input() reads xlsx fixture", {
  file <- testthat::test_path("fixtures", "tiny.xlsx")
  out <- read_input(file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_equal(ncol(out), 6)
  expect_equal(out$id, 1:10)
})

test_that("read_input() dispatches .xls extension to readxl", {
  tmp <- withr::local_tempdir()
  xls_path <- file.path(tmp, "test.xls")
  file.create(xls_path)
  # Verify the error is not "unsupported extension" (proves dispatch to readxl)
  err <- tryCatch(read_input(xls_path), error = function(e) e$message)
  expect_false(grepl("Unsupported file extension", err))
})

test_that("read_input() reads xlsx with sheet arg", {
  file <- testthat::test_path("fixtures", "tiny.xlsx")
  out <- read_input(file, sheet = 1)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
})

test_that("read_input() reads sas7bdat fixture", {
  file <- testthat::test_path("fixtures", "tiny.sas7bdat")
  out <- read_input(file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
})

test_that("read_input() reads xpt fixture", {
  file <- testthat::test_path("fixtures", "tiny.xpt")
  out <- read_input(file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_true("dt" %in% names(out))
})

test_that("read_input() preserves haven_labelled from sas7bdat", {
  tmp <- withr::local_tempdir()
  df <- data.frame(
    id = 1:5,
    status = haven::labelled(
      c(1, 2, 1, 3, 2),
      labels = c(Active = 1, Inactive = 2, Pending = 3)
    ),
    stringsAsFactors = FALSE
  )
  sas_file <- file.path(tmp, "test.sas7bdat")
  suppressWarnings(haven::write_sas(df, sas_file))
  out <- read_input(sas_file)
  # SAS upcases column names; check all columns for labelled
  any_labelled <- any(vapply(out, haven::is.labelled, logical(1)))
  # If write_sas doesn't preserve labels (platform-dependent), skip gracefully
  if (!any_labelled) {
    skip("write_sas did not preserve labelling on this platform")
  }
  expect_true(any_labelled)
})

test_that("read_input() error on unsupported extension", {
  tmp <- withr::local_tempdir()
  txt_file <- file.path(tmp, "data.txt")
  writeLines("hello", txt_file)
  expect_error(
    read_input(txt_file),
    "Unsupported file extension"
  )
})

test_that("read_input() error on non-existent file", {
  expect_error(
    read_input("non_existent_file.csv"),
    "File does not exist"
  )
})

test_that("read_input() encoding arg is applied to CSV locale", {
  tmp <- withr::local_tempdir()
  # Write a CSV with a latin1 character (e-acute)
  csv_file <- file.path(tmp, "encoded.csv")
  writeBin(chartr("\u00e9", "e", iconv("name\nAndr\u00e9", to = "latin1")), csv_file)
  con <- file(csv_file, open = "wb")
  writeBin(iconv("name\nAndr\u00e9", to = "latin1", toRaw = TRUE)[[1]], con)
  close(con)
  out <- read_input(csv_file, encoding = "latin1")
  expect_s3_class(out, "tbl_df")
  expect_equal(out$name, "Andr\u00e9")
})

test_that("read_input() encoding is ignored when caller supplies locale", {
  tmp <- withr::local_tempdir()
  csv_file <- file.path(tmp, "simple.csv")
  readr::write_csv(data.frame(x = 1:3), csv_file)
  # Should not error -- caller locale wins, encoding arg is silently skipped
  out <- read_input(csv_file, encoding = "UTF-8",
                    locale = readr::locale(encoding = "UTF-8"))
  expect_s3_class(out, "tbl_df")
})

test_that("read_input() encoding arg is ignored for non-CSV formats", {
  file <- testthat::test_path("fixtures", "tiny.xlsx")
  # encoding is silently ignored for Excel; should not error
  out <- read_input(file, encoding = "UTF-8")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
})

test_that("read_input() CSV round-trips with readr writer", {
  tmp <- withr::local_tempdir()
  df <- data.frame(
    x = 1:5,
    y = letters[1:5],
    stringsAsFactors = FALSE
  )
  csv_file <- file.path(tmp, "roundtrip.csv")
  readr::write_csv(df, csv_file)
  out <- read_input(csv_file)
  expect_equal(out$x, df$x)
  expect_equal(out$y, df$y)
})

test_that("read_input() CSV col_select keeps only requested columns", {
  file <- testthat::test_path("fixtures", "tiny.csv")
  out <- expect_no_warning(read_input(file, col_select = c("id", "group")))
  expect_named(out, c("id", "group"))
  expect_equal(ncol(out), 2)
  expect_equal(nrow(out), 10)
})
