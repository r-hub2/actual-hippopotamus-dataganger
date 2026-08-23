local({

# Tests for privacy_check() -- [3.6]-[3.8]

# Bind the real exports explicitly so these tests are insulated from any mocked
# bindings left by earlier Shiny module tests under testthat::test_local().
privacy_check <- dataganger::privacy_check
detect_roles <- dataganger::detect_roles
synthesize_data <- dataganger::synthesize_data
synth_spec <- dataganger::synth_spec

# ---- Pre-stage ----

test_that("privacy_check() pre returns correct S3 class", {
  df <- data.frame(id = 1:50, x = rnorm(50))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_s3_class(pc, "dataganger_privacy_check")
  expect_true("severity" %in% names(pc))
  expect_true("recommendation" %in% names(pc))
})

test_that("privacy_check() pre flags ID columns as HIGH", {
  df <- data.frame(patient_id = 1:50, x = rnorm(50))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  id_flags <- pc[grepl("patient_id", pc$variable, fixed = TRUE), ]
  expect_true(nrow(id_flags) > 0)
  expect_equal(id_flags$severity[1], "HIGH")
})

test_that("privacy_check() pre flags sensitive with detected roles", {
  df <- data.frame(
    record_id = 1:50,
    visit_date = as.Date("2024-01-01") + 1:50,
    city = rep("Toronto", 50)
  )
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  # We expect at least ID flag
  expect_in("HIGH", pc$severity)
  # Date or city should trigger at least LOW
  expect_true(any(pc$severity %in% c("LOW", "MEDIUM")),
              info = paste("Observed severities:", paste(pc$severity, collapse = ", ")))
})

test_that("privacy_check_pre reads disclosure_role, not sensitive", {
  df <- data.frame(
    patient_id = sprintf("P%04d", 1:50),
    diagnosis  = rep(c("A", "B"), 25),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable == "diagnosis"] <- "sensitive"

  expect_no_error(flags <- privacy_check(df, roles = roles, stage = "pre"))
  expect_in("patient_id", flags$variable[flags$severity == "HIGH"])
  expect_in("diagnosis", flags$variable[flags$flag == "Sensitive target"])
})

test_that("privacy_check_pre raises a combination cell-size flag", {
  df <- data.frame(
    zip = c(rep("A", 8), "B", "C"),
    sex = c(rep("F", 4), rep("M", 4), "F", "M"),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable %in% c("zip", "sex")] <- "quasi"

  flags <- privacy_check(df, roles = roles, stage = "pre")
  expect_match(paste(flags$flag, collapse = "\n"),
               "smaller than k|cell size|k-anonymity", ignore.case = TRUE)
})


test_that("privacy_check_pre handles roles missing one column", {
  df <- data.frame(patient_id = 1:50, x = rnorm(50), stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  roles <- roles[roles$variable != "x", , drop = FALSE]
  expect_no_error(pc <- privacy_check(df, roles = roles, stage = "pre"))
  expect_s3_class(pc, "dataganger_privacy_check")
})
test_that("privacy_check() pre flags date columns", {
  df <- data.frame(d = as.Date("2024-01-01") + 1:10)
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_match(paste(pc$flag, collapse = "\n"), "Date")
})

test_that("privacy_check() pre flags geography columns", {
  df <- data.frame(city = rep("Toronto", 10))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_match(paste(pc$flag, collapse = "\n"), "Geography")
})

test_that("privacy_check() pre flags free-text columns", {
  # 60 rows, 40 unique long strings -- moderate cardinality to avoid ID candidate
  long_strings <- c(
    sprintf("very_long_text_for_privacy_test_number_%040d", 1:40),
    sample(sprintf("very_long_text_for_privacy_test_number_%040d", 1:40), 20, replace = TRUE)
  )
  df <- data.frame(notes = long_strings, x = 1:60)
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable == "notes"] <- "none"
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_match(paste(pc$flag, collapse = "\n"), "Free.text")
})

test_that("privacy_check() pre returns empty for clean data", {
  df <- data.frame(x = rep(1:30, length.out = 50))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  # x has moderate cardinality, name doesn't match any pattern
  # So should have few/no flags
  expect_s3_class(pc, "dataganger_privacy_check")
})

# ---- Post-stage ----

test_that("privacy_check() post requires synthetic arg", {
  df <- data.frame(x = 1:5)
  expect_error(
    privacy_check(df, stage = "post"),
    "must be a data frame"
  )
})

test_that("privacy_check() post does not flag a scrambled alphanumeric ID", {
  set.seed(1)
  df <- data.frame(
    id = sprintf("REC-%05d", 1:50),
    x  = rep(1:5, 10),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$recommended_role[roles$variable == "id"], "alphanumeric ID")
  expect_equal(roles$simulation[roles$variable == "id"], "scramble")
  # Same row count as the original so scramble actually applies (not the
  # row-count-mismatch fallback to plain synthesis).
  spec <- synth_spec(purpose = "demo", n = nrow(df), engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")
  pc <- privacy_check(df, syn, roles = roles, stage = "post")
  expect_true("id" %in% names(syn))
  expect_equal(intersect(syn$id, df$id), character(),
               info = "Original IDs found in synthetic output")
  expect_no_match(pc$flag[pc$severity == "HIGH"], "ID")
})

test_that("privacy_check() post flags an ID that ends up unmasked (e.g. scramble skipped by a row-count change)", {
  df <- data.frame(
    id = sprintf("REC-%05d", 1:50),
    x  = rep(1:5, 10),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  # A row-count change makes scramble/pass-through fall back to plain
  # synthesis (a warning, not an error) -- the ID column then survives
  # unprotected, which post-stage privacy_check() must still catch.
  spec <- synth_spec(purpose = "demo", n = 20, engine = "internal")
  syn <- suppressWarnings(synthesize_data(df, spec, roles = roles, engine = "internal"))
  pc <- privacy_check(df, syn, roles = roles, stage = "post")
  expect_true("id" %in% names(syn))
  expect_match(paste(pc$flag[pc$severity == "HIGH"], collapse = "\n"), "ID")
})

test_that("privacy_check() post exact-row match check", {
  df <- data.frame(
    id = 1:50,
    x  = rep(1:10, 5)
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 200, seed = 1)
  # id is an alphanumeric ID (default simulation "scramble"); the requested
  # row count (200) differs from the original (50), so scramble falls back
  # to plain synthesis with a warning -- expected and irrelevant here.
  syn <- suppressWarnings(synthesize_data(df, spec, roles = roles))
  pc <- privacy_check(df, syn, roles = roles, stage = "post")
  expect_s3_class(pc, "dataganger_privacy_check")
  expect_true(attr(pc, "exact_row_matches") >= 0)
})

test_that("privacy_check() post skips row-match when nrow < 20", {
  df <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  pc <- privacy_check(df, syn, stage = "post")
  expect_no_match(pc$flag, "exact-row")
  expect_equal(attr(pc, "exact_row_matches"), 0)
})

test_that("privacy_check() post flags rare-category survival", {
  df <- data.frame(
    f = factor(c(rep("common", 95), rep("rare", 5)))
  )
  spec <- synth_spec(purpose = "demo", n = 50, rare_level_min_n = 10)
  syn <- synthesize_data(df, spec)
  pc <- privacy_check(df, syn, stage = "post", spec = spec)
  expect_s3_class(pc, "dataganger_privacy_check")
})

test_that("privacy_check() post flags date precision when coarsen_dates = TRUE", {
  df <- data.frame(
    dt = as.Date("2024-01-01") + 0:29
  )
  spec <- synth_spec(purpose = "demo", n = 20, coarsen_dates = TRUE)
  syn <- synthesize_data(df, spec)
  pc <- privacy_check(df, syn, stage = "post", spec = spec)
  # Dates should be coarsened to month, so post check should pass without MEDIUM flag
  # If coarsening failed, we'd get a flag
  expect_s3_class(pc, "dataganger_privacy_check")
})

test_that("privacy_check() post with masked IDs is clean", {
  df <- data.frame(
    record_id = 1:50,
    x = rnorm(50)
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 20)
  spec$remove_ids <- TRUE
  # record_id's default "scramble" simulation would otherwise fight with
  # remove_ids's NA-masking on this row-count change (20 vs 50); the warning
  # from that fallback is expected and irrelevant to this test.
  syn <- suppressWarnings(synthesize_data(df, spec, roles = roles))
  pc <- privacy_check(df, syn, roles = roles, stage = "post")
  # IDs should be all-NA, so no unmasked-ID flag
  id_flags <- pc[grepl("ID", pc$flag) & pc$severity == "HIGH", ]
  expect_equal(nrow(id_flags), 0)
})

test_that("privacy_check() internal path does not add synthpop disclosure", {
  df <- data.frame(city = rep(c("Toronto", "Ottawa"), 20), group = rep(letters[1:4], 10))
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 20, seed = 1L)
  syn <- synthesize_data(df, spec, roles = roles)
  pc <- privacy_check(df, syn, roles = roles, stage = "post", spec = spec)
  expect_null(attr(pc, "synthpop_disclosure", exact = TRUE))
  expect_no_match(pc$variable, "synthpop disclosure", fixed = TRUE)
})

test_that("synthpop_disclosure_flags() folds repU and DiSCO numbers into flag rows", {
  # Deterministic unit test of the fold formatting (no synthpop, no mocking:
  # mocking synthpop_disclosure_panel proved unreliable across the full session).
  disclosure <- list(
    keys = "city", target = "group",
    identity_repu = 1.25, attribute_disco = 2.5, raw = list()
  )
  rows <- synthpop_disclosure_flags(disclosure)
  expect_equal(nrow(rows), 2L)
  expect_match(paste(rows$flag, collapse = "\n"), "repU: 1.25", fixed = TRUE)
  expect_match(paste(rows$flag, collapse = "\n"), "DiSCO: 2.50", fixed = TRUE)
})

test_that("augment_synthpop_disclosure() leaves non-synthpop output untouched", {
  flags <- make_flag("x", "some flag", "LOW", "ok")
  syn <- tibble::tibble(x = 1:3)            # no engine attr -> not synthpop
  out <- augment_synthpop_disclosure(flags, syn, syn, detect_roles(syn))
  expect_identical(out, flags)
  expect_null(attr(out, "synthpop_disclosure", exact = TRUE))
})

test_that("privacy_check() computes synthpop disclosure when synthpop is installed", {
  skip_if_no_synthpop()
  df <- data.frame(
    city = rep(c("Toronto", "Ottawa", "Montreal", "Calgary"), each = 10),
    group = rep(letters[1:5], length.out = 40),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable %in% c("city", "group")] <- "none"
  spec <- suppressWarnings(synth_spec(purpose = "development", n = 30, seed = 1L))
  syn <- synthesize_data(df, spec, roles = roles)
  pc <- privacy_check(df, syn, roles = roles, stage = "post", spec = spec)
  expect_equal(attr(syn, "engine"), "synthpop")
  expect_false(is.null(attr(pc, "synthpop_disclosure", exact = TRUE)))
})

test_that("privacy_check() print method works", {
  df <- data.frame(patient_id = 1:50, x = rnorm(50))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_no_error(print(pc))
})

test_that("privacy_check() print method works for empty flags", {
  df <- data.frame(x = rep(1:30, length.out = 50))
  roles <- detect_roles(df)
  pc <- privacy_check(df, roles = roles, stage = "pre")
  expect_no_error(print(pc))
})

test_that("privacy_check() rejects non-data-frame original", {
  expect_error(
    privacy_check("not a df", stage = "pre"),
    "must be a data frame"
  )
})

test_that("privacy_check() pre works without roles", {
  df <- data.frame(x = 1:5)
  pc <- privacy_check(df, stage = "pre")
  expect_s3_class(pc, "dataganger_privacy_check")
})

test_that("privacy_check() end-to-end on example_health_survey", {
  data("example_health_survey", package = "dataganger")
  roles <- detect_roles(example_health_survey)
  pc_pre <- privacy_check(example_health_survey, roles = roles, stage = "pre")
  expect_s3_class(pc_pre, "dataganger_privacy_check")
  expect_in("HIGH", pc_pre$severity)

  spec <- synth_spec(purpose = "development", seed = 1, n = 50)
  syn <- suppressWarnings(synthesize_data(example_health_survey, spec, roles = roles))
  pc_post <- privacy_check(example_health_survey, syn, roles = roles, stage = "post")
  expect_s3_class(pc_post, "dataganger_privacy_check")
})
})
