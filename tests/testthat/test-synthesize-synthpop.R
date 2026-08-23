test_that("spec_to_synthpop_args() maps n, seed, exclusions, and smoothing", {
  df <- data.frame(
    record_id = paste0("ID-", 1:25),
    notes = sprintf("this is long free text value number %02d", 1:25),
    # continuous (non-integer) but not all-distinct, so it is not flagged an
    # ID candidate (distinct_ratio < 0.95) and survives to the smoothing step
    score = rep(c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 10.1, 11.2, 12.3),
                length.out = 25),
    bounded = rep(1:5, length.out = 25),
    group = rep(letters[1:5], length.out = 25),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 10L, seed = 42L)
  args <- spec_to_synthpop_args(spec, roles, df)

  expect_equal(args$k, 10L)
  expect_equal(args$seed, 42L)
  expect_false("record_id" %in% names(args$data))
  expect_false("notes" %in% names(args$data))
  expect_true("score" %in% names(args$smoothing))
  expect_equal(args$smoothing[["score"]], "density")
  expect_false("bounded" %in% names(args$smoothing))
})

test_that("spec_to_synthpop_args() omits smoothing for pure-integer data", {
  df <- data.frame(x = 1:25, y = rep(1:5, length.out = 25))
  spec <- synth_spec(purpose = "demo")
  args <- spec_to_synthpop_args(spec, roles = NULL, data = df)
  expect_null(args$smoothing)
})

test_that("synthesize_synthpop() returns a tibble with same columns", {
  skip_if_no_synthpop()
  df   <- data.frame(x = 1:20, y = letters[rep(1:4, 5)], stringsAsFactors = FALSE)
  spec <- synth_spec(purpose = "demo", seed = 1L)
  syn  <- synthesize_synthpop(df, spec)
  expect_s3_class(syn, "tbl_df")
  expect_named(syn, names(df))
})

test_that("synthesize_synthpop() respects n rows via spec$n", {
  skip_if_no_synthpop()
  df   <- data.frame(x = 1:30, y = rnorm(30))
  spec <- synth_spec(purpose = "demo", n = 10L, seed = 1L)
  syn  <- synthesize_synthpop(df, spec)
  expect_equal(nrow(syn), 10L)
})

test_that("synthesize_synthpop() excludes ID candidate columns", {
  skip_if_no_synthpop()
  df <- data.frame(
    record_id = paste0("ID-", 1:25),
    score     = rep(1:5, each = 5),
    group     = rep(letters[1:5], each = 5),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec  <- synth_spec(purpose = "demo", seed = 1L)
  syn   <- synthesize_synthpop(df, spec, roles = roles)
  expect_false("record_id" %in% names(syn))
  expect_true("score" %in% names(syn))
  expect_true("group" %in% names(syn))
})

test_that("synthesize_synthpop() seed produces reproducible output", {
  skip_if_no_synthpop()
  df   <- data.frame(x = rnorm(20), y = rnorm(20))
  spec <- synth_spec(purpose = "demo", seed = 42L)
  syn1 <- synthesize_synthpop(df, spec)
  syn2 <- synthesize_synthpop(df, spec)
  expect_equal(syn1$x, syn2$x)
})

test_that("seeded scramble stays deterministic on the synthpop path", {
  skip_if_no_synthpop()
  df <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, 10:39),
    x = rnorm(30),
    y = rnorm(30),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", engine = "synthpop", seed = 42L)

  syn1 <- synthesize_data(df, spec, roles = roles)
  stats::runif(128)
  syn2 <- synthesize_data(df, spec, roles = roles)

  expect_identical(syn1$order_id, syn2$order_id)
})

test_that("synthesize_synthpop() aborts when all columns are excluded", {
  skip_if_no_synthpop()
  df    <- data.frame(id = paste0("X-", 1:25), stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  spec  <- synth_spec(purpose = "demo")
  expect_error(synthesize_synthpop(df, spec, roles = roles), "No synthesizable columns")
})

test_that("synthpop_bridge_cols() identifies high-cardinality char columns", {
  withr::local_locale(c(LC_TIME = "C"))
  df <- data.frame(
    date_str = format(as.Date("2020-01-01") + 1:50, "%b %e, %Y"), # date role
    big_cat  = sprintf("cat_%03d", rep(1:30, length.out = 50)),     # 30 distinct, letter+digit shape -> alphanumeric ID
    small_cat = rep(letters[1:5], each = 10),                        # 5 distinct, OK
    score     = rnorm(50),
    stringsAsFactors = FALSE
  )
  roles  <- detect_roles(df)
  bridge <- synthpop_bridge_cols(roles, df)
  expect_true("date_str"  %in% bridge)
  # big_cat's consistent letter+digit shape now gets recommended_role
  # "alphanumeric ID", so it is truly excluded (handled by
  # apply_simulation_treatment's scramble) rather than bridged.
  expect_false("big_cat"  %in% bridge)
  expect_true("big_cat"   %in% synthpop_role_excluded_cols(roles))
  expect_false("small_cat" %in% bridge)
  expect_false("score"     %in% bridge)
})

test_that("synthesize_synthpop() stitches bridge columns back into original order", {
  skip_if_no_synthpop()
  df <- data.frame(
    id      = paste0("ID-", 1:25),                            # excluded (ID)
    d_str   = format(as.Date("2020-01-01") + 1:25, "%Y-%m-%d"), # bridge
    score   = rep(1:5, each = 5),
    group   = rep(letters[1:5], each = 5),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec  <- synth_spec(purpose = "demo", n = 10L, seed = 1L)
  syn   <- synthesize_synthpop(df, spec, roles = roles)
  # id excluded, d_str (date bridge) stitched back, score + group in synthpop
  expect_false("id"    %in% names(syn))
  expect_true( "d_str" %in% names(syn))
  expect_true( "score" %in% names(syn))
  expect_true( "group" %in% names(syn))
  # column order mirrors original (minus excluded id)
  expect_equal(names(syn), c("d_str", "score", "group"))
})

# Regression: Bug 5 -- CUSUM-shaped data (char-stored date + high-cardinality
# char predictor) used to hang synthpop's CART. Verify completion < 30 s.
test_that("synthesize_data() with high-cardinality char date column completes without hang", {
  skip_if_no_synthpop()
  withr::local_locale(c(LC_TIME = "C"))
  set.seed(42)
  n <- 500L
  df <- data.frame(
    case_id   = sprintf("CASE-%05d", 1:n),         # ID -> excluded
    date_str  = format(seq.Date(as.Date("2019-01-01"), by = "day",
                                length.out = n), "%b %e, %Y"),  # date bridge
    month     = rep(month.abb, length.out = n),    # 12 distinct char
    region    = rep(LETTERS[1:6], length.out = n), # 6 distinct char
    count     = sample(1:10, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec  <- suppressWarnings(synth_spec("development", n = 50L, seed = 1L))
  t0    <- proc.time()[["elapsed"]]
  syn   <- synthesize_data(df, spec, roles = roles)
  elapsed <- proc.time()[["elapsed"]] - t0
  expect_lt(elapsed, 30, label = "synthesis should complete in under 30 seconds")
  expect_s3_class(syn, "dataganger_synthetic")
  expect_equal(nrow(syn), 50L)
})

test_that("synthesize_data() derives roles for synthpop so high-cardinality IDs don't stall it", {
  # Regression: with roles = NULL, an ID / free-text column was passed to
  # synthpop, whose sequential CART grinds forever on a high-cardinality
  # categorical. synthesize_data() must derive roles and exclude such columns.
  skip_if_no_synthpop()
  df <- data.frame(
    rec_id = sprintf("R%04d", 1:60),   # 60 unique -> ID candidate, would stall CART
    age    = round(rnorm(60, 50, 10)),
    grp    = factor(sample(c("a", "b", "c"), 60, TRUE)),
    stringsAsFactors = FALSE
  )
  spec <- suppressWarnings(synth_spec("development", seed = 1L))
  syn <- synthesize_data(df, spec)          # no roles passed
  expect_equal(attr(syn, "engine"), "synthpop")
  expect_s3_class(syn, "dataganger_synthetic")
  expect_equal(nrow(syn), 60L)
})

test_that("labelled columns with a rare level survive the synthpop path", {
  skip_if_no_synthpop()

  # Regression: synthpop's sequential CART calls t() on the response, and
  # t.haven_labelled() does not exist. Under label_strategy = "preserve" the
  # column previously reached syn() still classed as haven_labelled and the
  # call hard-errored with "`t.haven_labelled()` not supported".
  #
  # The distribution matters: a balanced labelled column did not trigger it,
  # so the fixture below is deliberately skewed with a level seen twice in
  # 300 rows. That is the normal shape of a coded survey or clinical variable,
  # and "analytics" defaults to label_strategy = "preserve", so this was
  # reachable with default settings on real data.
  n <- 300L
  df <- data.frame(
    low = rep(c("A", "B", "C"), length.out = n),
    num = seq_len(n),
    stringsAsFactors = FALSE
  )
  df$coded <- haven::labelled(
    c(rep(1, 290L), rep(2, 8L), 3, 3),
    labels = c(Alpha = 1, Bravo = 2, Charlie = 3)
  )

  roles <- detect_roles(df)
  roles$user_role[roles$variable %in% c("low", "coded")] <- "categorical"
  roles$user_role[roles$variable == "num"] <- "numeric"
  roles$label_strategy[roles$variable %in% c("low", "coded")] <- "preserve"

  for (purpose in c("development", "analytics")) {
    spec <- synth_spec(
      purpose = purpose, seed = 1L,
      acknowledge_risk = identical(purpose, "analytics")
    )
    syn <- suppressWarnings(synthesize_data(df, spec, roles = roles))

    expect_equal(nrow(syn), n, info = purpose)

    observed <- sort(unique(as.character(syn$coded)))
    expect_setequal(observed, c("Alpha", "Bravo", "Charlie"))
  }
})
