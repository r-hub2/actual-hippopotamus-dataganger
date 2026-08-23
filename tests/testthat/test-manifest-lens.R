local({
make_manifest_for_test <- function(purpose = "development", seed = 42L,
                                    n = 10L, include_report = TRUE) {
  df <- data.frame(
    id    = seq_len(n),
    score = rnorm(n),
    grp   = rep(c("a", "b"), length.out = n),
    stringsAsFactors = FALSE
  )
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "bundle.zip")

  spec      <- synth_spec(purpose = purpose, seed = seed)
  synthetic <- synthesize_data(df, spec)
  export_synthetic(synthetic, original = df, path = out, format = "zip",
                   include_report = include_report)

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out, exdir = extract_dir)
  jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
}

test_that("manifest.json contains Lens source field", {
  m <- make_manifest_for_test()
  expect_equal(m$source, "dataganger")
})

test_that("manifest.json contains original_rows_bucket when original is supplied", {
  m <- make_manifest_for_test(n = 10L)
  expect_type(m$original_rows_bucket, "character")
  expect_equal(m$original_rows_bucket, "<100")
})

test_that("manifest.json contains original_columns_count when original is supplied", {
  m <- make_manifest_for_test(n = 10L)
  expect_equal(m$original_columns_count, 3L)
})

test_that("manifest.json raw_rows_included is always false", {
  m <- make_manifest_for_test()
  expect_false(isTRUE(m$raw_rows_included))
})

test_that("manifest.json free_text_included is always false", {
  m <- make_manifest_for_test()
  expect_false(isTRUE(m$free_text_included))
})

test_that("manifest.json ids_included is always false", {
  m <- make_manifest_for_test()
  expect_false(isTRUE(m$ids_included))
})

test_that("manifest.json plots_included is false when no report is written", {
  m <- make_manifest_for_test(include_report = FALSE)
  expect_false(isTRUE(m$plots_included))
})

test_that("manifest.json plots_included is true when the comparison report is written", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  m <- make_manifest_for_test(include_report = TRUE)
  expect_true(isTRUE(m$plots_included))
})

test_that("manifest.json factor_levels_included is true for marginal synthesis", {
  m <- make_manifest_for_test(purpose = "development")
  expect_true(isTRUE(m$factor_levels_included))
})

test_that("manifest.json factor_levels_included is false for schema synthesis", {
  df <- data.frame(x = 1:10, y = rep(c("a", "b"), 5), stringsAsFactors = FALSE)
  tmp  <- withr::local_tempdir()
  out  <- file.path(tmp, "bundle.zip")
  spec <- synth_spec(purpose = "demo", level = "schema")
  synthetic <- synthesize_data(df, spec)
  export_synthetic(synthetic, original = df, path = out, format = "zip")

  extract_dir <- file.path(tmp, "ext")
  dir.create(extract_dir)
  utils::unzip(out, exdir = extract_dir)
  m <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_false(isTRUE(m$factor_levels_included))
})

test_that("manifest.json numeric_ranges_included is always false", {
  m <- make_manifest_for_test()
  expect_false(isTRUE(m$numeric_ranges_included))
})

test_that("manifest.json policy_file is null", {
  m <- make_manifest_for_test()
  expect_null(m$policy_file)
})

test_that("manifest.json original_rows_bucket is null when original not supplied", {
  df  <- data.frame(x = 1:5)
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "bundle.zip")
  spec      <- synth_spec(purpose = "demo")
  synthetic <- synthesize_data(df, spec)
  export_synthetic(synthetic, path = out, format = "zip")

  extract_dir <- file.path(tmp, "ext")
  dir.create(extract_dir)
  utils::unzip(out, exdir = extract_dir)
  m <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_null(m$original_rows_bucket)
  expect_null(m$original_columns_count)
})
})
