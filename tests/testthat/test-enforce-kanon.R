
test_that("enforce_kanon removes direct identifiers from output", {
  syn <- data.frame(
    id  = sprintf("P%03d", 1:20),
    sex = rep(c("F", "M"), 10),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("id", "sex"),
    disclosure_role = c("direct", "quasi"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_false("id" %in% names(out))
})

test_that("enforce_kanon does not remove a direct-identifier column explicitly set to pass_through", {
  syn <- data.frame(
    id  = sprintf("P%03d", 1:20),
    sex = rep(c("F", "M"), 10),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("id", "sex"),
    disclosure_role = c("direct", "quasi"),
    simulation = c("pass_through", "synthesize"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_true("id" %in% names(out))
})

test_that("enforce_kanon does not remove a direct-identifier column explicitly set to scramble", {
  syn <- data.frame(
    id  = sprintf("P%03d", 1:20),
    sex = rep(c("F", "M"), 10),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("id", "sex"),
    disclosure_role = c("direct", "quasi"),
    simulation = c("scramble", "synthesize"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_true("id" %in% names(out))
})

test_that("enforce_kanon still removes a direct identifier when simulation is the untouched default", {
  syn <- data.frame(
    id  = sprintf("P%03d", 1:20),
    sex = rep(c("F", "M"), 10),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("id", "sex"),
    disclosure_role = c("direct", "quasi"),
    simulation = c("synthesize", "synthesize"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_false("id" %in% names(out))
})

test_that("enforce_kanon still removes a direct identifier explicitly set to drop", {
  syn <- data.frame(
    id  = sprintf("P%03d", 1:20),
    sex = rep(c("F", "M"), 10),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("id", "sex"),
    disclosure_role = c("direct", "quasi"),
    simulation = c("drop", "synthesize"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_false("id" %in% names(out))
})

test_that("enforce_kanon leaves output with no QI cell smaller than k", {
  syn <- data.frame(
    cat = c(rep("A", 30), rep("B", 30), rep("C", 2)),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = "cat", disclosure_role = "quasi", stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  tab <- table(out$cat[!is.na(out$cat)])
  expect_true(
    all(tab >= 5),
    info = paste("Observed cell counts:", paste(tab, collapse = ", "))
  )
})

test_that("merge_rarest_level suppresses rather than inventing a category", {
  out <- merge_rarest_level(c("common", "common", "rare"))

  expect_equal(out, c("common", "common", NA_character_))
  expect_false(
    any(out %in% "(other)", na.rm = TRUE),
    info = paste("Output values:", paste(unique(out), collapse = ", "))
  )
})

test_that("enforce_kanon suppresses residual cells that cannot reach k", {
  # The single below-k cell ("uniqueX", 1 row) gets blanked; that 1-row NA
  # bucket is itself below k, so the absorption pass pads it -- and the only
  # cell left to absorb from is the entire "common" cell (19 rows), so this
  # one small residual ends up suppressing the whole column. Suppression
  # works at whole-cell granularity (cells can't be split without breaking
  # their internal uniformity), so this is expected, not a feasibility
  # failure: the upfront backstop only looks at the *initial* below-k count
  # (1/20 = 5%), which is well under the default cap.
  syn <- data.frame(
    code = c(rep("common", 19), "uniqueX"), stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = "code", disclosure_role = "quasi", stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  info <- attr(out, "kanon")
  expect_false(isTRUE(info$infeasible))
  expect_true(info$suppressed_cells >= 1)
  res <- assess_kanonymity(out, info$qi_cols, k = 5)
  expect_true(is.na(res$smallest_cell) || res$smallest_cell >= 5)
  # The actual row-level suppression volume is tracked separately from
  # suppressed_cells (a count of distinct QI combinations) so callers can
  # see when whole-cell absorption ends up suppressing far more rows than
  # the number that were originally below k.
  expect_equal(info$suppressed_rows, 20L)
  expect_equal(info$suppressed_row_frac, 1)
})

test_that("enforce_kanon backs off (no suppression) when k is infeasible", {
  # 10 all-unique codes cannot reach k = 5; suppressing them would blank the
  # whole column. The feasibility backstop keeps the data and flags infeasible.
  syn <- data.frame(
    code = sprintf("X%02d", 1:10), stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = "code", disclosure_role = "quasi", stringsAsFactors = FALSE
  )
  out <- suppressWarnings(enforce_kanon(syn, roles = roles, k = 5))
  info <- attr(out, "kanon")
  expect_true(isTRUE(info$infeasible))
  expect_equal(info$suppressed_cells, 0L)
  expect_false(
    any(is.na(out$code)),
    info = paste("Missing code rows:", paste(which(is.na(out$code)), collapse = ", "))
  )
  expect_setequal(out$code, syn$code)
})

test_that("enforce_kanon NA bucket is padded to k when initial suppression creates a small bucket", {
  # Regression: when only a few rows are blanked they collapse into a single
  # NA bucket that itself violates k. The fix absorbs more rows until NA >= k.
  set.seed(7)
  syn <- data.frame(
    sex  = sample(c("F", "M"), 120, replace = TRUE),
    city = sample(c("Toronto", "Montreal"), 120, replace = TRUE),
    rare = c(rep("common", 118), "uniqA", "uniqB"),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("sex", "city", "rare"),
    disclosure_role = c("quasi", "quasi", "quasi"),
    stringsAsFactors = FALSE
  )
  out <- enforce_kanon(syn, roles = roles, k = 8)
  info <- attr(out, "kanon")
  expect_false(is.null(info))
  res <- assess_kanonymity(out, info$qi_cols, k = 8)
  # Every non-NA cell AND the NA bucket must be >= 8
  expect_true(is.na(res$smallest_cell) || res$smallest_cell >= 8,
    info = sprintf("smallest_cell = %s", res$smallest_cell))
  # Most of the data survives here (unlike the single-dominant-cell case
  # above) -- suppressed_row_frac should reflect that modest cost.
  expect_true(info$suppressed_row_frac > 0 && info$suppressed_row_frac < 0.5)
})

test_that("enforce_kanon ignores NA (unselected) disclosure roles safely", {
  syn <- data.frame(
    g = rep(c("a", "b"), 50),
    m = rnorm(100),
    stringsAsFactors = FALSE
  )
  roles <- tibble::tibble(
    variable = c("g", "m"),
    disclosure_role = c(NA_character_, NA_character_)
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_equal(nrow(out), 100L)
  expect_false(anyNA(out$g))
  expect_identical(attr(out, "kanon")$qi_cols, character(0))
})

test_that("enforce_kanon handles a mix of NA and explicit roles", {
  syn <- data.frame(
    id = sprintf("P%03d", 1:100),
    zip = rep(c("A", "B", "C", "D"), 25),
    other = rep("x", 100),
    stringsAsFactors = FALSE
  )
  roles <- tibble::tibble(
    variable = c("id", "zip", "other"),
    disclosure_role = c("direct", "quasi", NA_character_)
  )
  out <- enforce_kanon(syn, roles = roles, k = 5)
  expect_false("id" %in% names(out))      # direct dropped
  expect_true(
    all(c("zip", "other") %in% names(out)),
    info = paste("Output columns:", paste(names(out), collapse = ", "))
  )
})

test_that("enforce_kanon unions identifying sensitive columns into the QI set", {
  syn <- data.frame(
    zip = rep(c("100", "200"), each = 6),
    religion = c(rep("A", 11), "B"),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = c("zip", "religion"),
    disclosure_role = c("quasi", "sensitive"),
    recommended_role = c("categorical candidate", "categorical candidate"),
    stringsAsFactors = FALSE
  )
  res <- enforce_kanon(syn, roles, k = 5)
  expect_true("religion" %in% attr(res, "kanon")$qi_cols)
})

test_that("synthesize_data emits k-anonymous output over quasi-identifiers", {
  set.seed(42)
  df <- data.frame(
    patient_id = sprintf("P%04d", 1:200),
    sex  = sample(c("F", "M"), 200, TRUE),
    band = sample(c("a", "b", "c"), 200, TRUE),
    rare = c(rep("common", 198), "uniqueX", "uniqueY"),
    val  = rnorm(200),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable == "rare"] <- "quasi"
  spec <- synth_spec(purpose = "demo", k_anon = 5)

  syn <- synthesize_data(df, spec = spec, roles = roles)

  # patient_id is an alphanumeric ID with the default "scramble" simulation,
  # so it is kept (scrambled) rather than dropped. A scramble reorders each
  # value's characters, so it will not reproduce the original column
  # verbatim (an occasional single-row coincidence is possible since some
  # values contain repeated digits, but the whole vector will not match).
  expect_true("patient_id" %in% names(syn))
  expect_false(identical(syn$patient_id, df$patient_id))
  info <- attr(syn, "kanon")
  expect_false(is.null(info))
  if (length(info$qi_cols)) {
    res <- assess_kanonymity(syn, info$qi_cols, k = 5)
    expect_true(is.na(res$smallest_cell) || res$smallest_cell >= 5)
  }
})

# Regression for the 100%-NA bug: with conservative disclosure defaults, a
# CUSUM-shaped frame (low-cardinality dimensions + a count measure) auto-marks
# ZERO columns as quasi, so enforce_kanon performs no suppression and the
# output stays fully populated. The old over-quasi defaults blanked ~99% to NA.
test_that("conservative defaults keep CUSUM-shaped output populated (no silent NA blanking)", {
  set.seed(7)
  n <- 300
  df <- data.frame(
    Region    = sample(c("N", "S", "E", "W"), n, TRUE),
    Year      = sample(2015:2020, n, TRUE),
    Month     = sample(1:12, n, TRUE),
    Sex       = sample(c("F", "M"), n, TRUE),
    AgeGroup  = sample(c("0-18", "19-64", "65+"), n, TRUE),
    CaseCount = sample(1:50, n, TRUE),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(sum(roles$disclosure_role %in% "quasi"), 0L)
  out <- enforce_kanon(df, roles = roles, k = 5)
  expect_equal(mean(is.na(out$Region)), 0)
  expect_equal(mean(is.na(out$CaseCount)), 0)
})
