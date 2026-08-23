
test_that("export_diagnostic_package() writes valid JSON to path", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  df  <- data.frame(
    patient_id = 1:30,
    score      = rnorm(30),
    city       = rep(c("Toronto", "Vancouver", "Montreal"), 10),
    notes      = paste("long free text note number", 1:30,
                       "with enough words to trigger detection"),
    stringsAsFactors = FALSE
  )

  export_diagnostic_package(df, path = out)

  expect_true(file.exists(out))
  diag <- jsonlite::read_json(out)
  expect_equal(diag$source, "dataganger")
  expect_type(diag$dataganger_version, "character")
  expect_type(diag$generated_at, "character")
  expect_type(diag$dataset$n_rows_bucket, "character")
  expect_type(diag$dataset$n_cols, "integer")
  expect_length(diag$columns, 4L)
  expect_true(isTRUE(diag$blocked$raw_rows))
  expect_true(isTRUE(diag$blocked$plots))
})

test_that("export_diagnostic_package() column fields are correct", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  df  <- data.frame(
    patient_id = sprintf("ID%03d", 1:30),
    score      = round(rnorm(30), 1),
    stringsAsFactors = FALSE
  )

  export_diagnostic_package(df, path = out)

  diag <- jsonlite::read_json(out)
  id_col    <- diag$columns[[1]]
  score_col <- diag$columns[[2]]

  expect_equal(id_col$name,           "patient_id")
  expect_equal(id_col$role,           "alphanumeric ID")
  expect_equal(id_col$disclosure_role, "direct")
  expect_false(isTRUE(id_col$exposed))
  expect_equal(id_col$exposure_level, "blocked")

  expect_equal(score_col$name, "score")
  expect_true( isTRUE(score_col$exposed))
  expect_true( score_col$exposure_level %in% c("schema_only", "coarsened"))
})

test_that("export_diagnostic_package() blocked flags reflect roles", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  df <- data.frame(
    record_id = rep(1:3, length.out = 30),
    note      = paste("narrative text note for patient number", 1:30,
                      "describing symptoms and history"),
    stringsAsFactors = FALSE
  )
  export_diagnostic_package(df, path = out)
  diag <- jsonlite::read_json(out)
  expect_true(isTRUE(diag$blocked$free_text_fields))
  expect_true(isTRUE(diag$blocked$id_fields))
})

test_that("export_diagnostic_package() blocked$free_text_fields is FALSE when no free text", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  df  <- data.frame(grp = rep(letters[1:3], 10), score = round(rnorm(30), 1))
  export_diagnostic_package(df, path = out)
  diag <- jsonlite::read_json(out)
  expect_false(isTRUE(diag$blocked$free_text_fields))
  expect_false(isTRUE(diag$blocked$id_fields))
})

test_that("export_diagnostic_package() accepts pre-computed roles", {
  tmp   <- withr::local_tempdir()
  out   <- file.path(tmp, "diag.json")
  df    <- data.frame(x = 1:30, y = letters[rep(1:5, 6)], stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  export_diagnostic_package(df, path = out, roles = roles)
  expect_true(file.exists(out))
})

test_that("export_diagnostic_package() aborts if path parent dir missing", {
  df <- data.frame(x = 1:5)
  expect_error(
    export_diagnostic_package(df, path = "/nonexistent_xyz/diag.json"),
    "Parent directory does not exist"
  )
})

test_that("export_diagnostic_package() aborts if path exists and overwrite = FALSE", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  writeLines("{}", out)
  df  <- data.frame(x = 1:5)
  expect_error(
    export_diagnostic_package(df, path = out),
    "already exists"
  )
})

test_that("export_diagnostic_package() overwrites when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "diag.json")
  df  <- data.frame(x = 1:5)
  writeLines("{}", out)
  expect_no_error(
    export_diagnostic_package(df, path = out, overwrite = TRUE)
  )
})
