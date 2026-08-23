
test_that("bucket_nrows() returns correct bands", {
  expect_equal(bucket_nrows(0L),     "<100")
  expect_equal(bucket_nrows(50L),    "<100")
  expect_equal(bucket_nrows(99L),    "<100")
  expect_equal(bucket_nrows(100L),   "100-999")
  expect_equal(bucket_nrows(999L),   "100-999")
  expect_equal(bucket_nrows(1000L),  "1000-9999")
  expect_equal(bucket_nrows(9999L),  "1000-9999")
  expect_equal(bucket_nrows(10000L), "10000-49999")
  expect_equal(bucket_nrows(49999L), "10000-49999")
  expect_equal(bucket_nrows(50000L), "50000+")
  expect_equal(bucket_nrows(1e6L),   "50000+")
})

test_that("build_diagnostic_view() returns correct structure", {
  roles <- tibble::tibble(
    variable         = c("patient_id", "score", "notes", "city"),
    class            = c("numeric", "numeric", "character", "character"),
    recommended_role = c("alphanumeric ID", "unknown", "free text", "categorical candidate"),
    user_role        = NA_character_,
    simulation       = "synthesize",
    reason           = c("name", "no match", "long text", "low cardinality"),
    disclosure_role  = c("direct", "none", "direct", "quasi")
  )
  class(roles) <- c("dataganger_roles", class(roles))

  dictionary <- tibble::tibble(
    synthetic_variable = c("patient_id", "score", "notes", "city"),
    treatment          = c("synthesized", "synthesized", "free_text_dropped", "synthesized")
  )

  synthetic <- data.frame(
    patient_id = 1:150, score = 1:150,
    notes = NA_character_, city = "x",
    stringsAsFactors = FALSE
  )

  result <- build_diagnostic_view(roles, dictionary, synthetic, "development")

  expect_equal(result$source,  "dataganger")
  expect_equal(result$purpose, "development")
  expect_type(result$dataganger_version, "character")
  expect_equal(result$dataset$n_rows_bucket, "100-999")
  expect_equal(result$dataset$n_cols, 4L)
  expect_length(result$columns, 4L)
  expect_equal(result$columns[[1]]$name,      "patient_id")
  expect_equal(result$columns[[1]]$role,      "alphanumeric ID")
  expect_equal(result$columns[[1]]$disclosure_role, "direct")
  expect_equal(result$columns[[1]]$treatment, "synthesized")
  expect_equal(result$columns[[2]]$disclosure_role, "none")
  expect_equal(result$columns[[3]]$treatment, "free_text_dropped")
  expect_true(result$blocked$raw_rows)
  expect_true(result$blocked$free_text_examples)
  expect_true(result$blocked$ids_synthesized)
  expect_true(result$blocked$plots)
})

test_that("build_diagnostic_view() blocked$free_text_examples is FALSE when no free text", {
  roles <- tibble::tibble(
    variable         = c("id", "score"),
    class            = c("numeric", "numeric"),
    recommended_role = c("alphanumeric ID", "unknown"),
    user_role        = NA_character_,
    simulation       = "synthesize",
    reason           = c("name", "no match"),
    disclosure_role  = c("direct", "none")
  )
  class(roles) <- c("dataganger_roles", class(roles))

  dictionary <- tibble::tibble(
    synthetic_variable = c("id", "score"),
    treatment          = c("synthesized", "synthesized")
  )

  synthetic <- data.frame(id = 1:10, score = 1:10)

  result <- build_diagnostic_view(roles, dictionary, synthetic, "demo")
  expect_false(result$blocked$free_text_examples)
  expect_true(result$blocked$ids_synthesized)
})

test_that("build_diagnostic_view() blocked$ids_synthesized is FALSE when no IDs", {
  roles <- tibble::tibble(
    variable         = c("grp", "score"),
    class            = c("character", "numeric"),
    recommended_role = c("categorical candidate", "unknown"),
    user_role        = NA_character_,
    simulation       = "synthesize",
    reason           = c("low cardinality", "no match"),
    disclosure_role  = c("none", "none")
  )
  class(roles) <- c("dataganger_roles", class(roles))

  dictionary <- tibble::tibble(
    synthetic_variable = c("grp", "score"),
    treatment          = c("synthesized", "synthesized")
  )

  synthetic <- data.frame(grp = letters[1:5], score = 1:5,
                           stringsAsFactors = FALSE)

  result <- build_diagnostic_view(roles, dictionary, synthetic, "demo")
  expect_false(result$blocked$ids_synthesized)
})
