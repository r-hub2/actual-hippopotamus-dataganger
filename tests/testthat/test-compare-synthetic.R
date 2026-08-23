# Tests for compare_synthetic() -- [3.1]-[3.5]

test_that("compare_synthetic() returns dataganger_comparison", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  spec <- synth_spec(purpose = "demo")
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_s3_class(cmp, "dataganger_comparison")
  expect_named(cmp, c("dataset", "numeric", "categorical", "relationship", "interaction",
                      "privacy_flags", "meta"))
})

test_that("compare_synthetic() includes relationship interactions", {
  set.seed(201)
  original <- data.frame(
    predictor = stats::rnorm(120),
    outcome = stats::rnorm(120),
    group = factor(rep(c("a", "b"), 60))
  )
  synthetic <- original

  interaction <- compare_synthetic(original, synthetic)$interaction

  expect_named(interaction, c(
    "predictor", "outcome", "family", "effect_label", "estimate",
    "null_value", "p_value", "n_terms", "note"
  ))
  expect_equal(nrow(interaction), choose(3, 2))
  expect_identical(interaction$predictor[[1]], "predictor")
  expect_identical(interaction$outcome[[1]], "outcome")
})

test_that("compare_synthetic() dataset-level metrics", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  spec <- synth_spec(purpose = "demo", n = 20)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  ds <- cmp$dataset
  expect_equal(ds$original[ds$metric == "nrow"], 10)
  expect_equal(ds$synthetic[ds$metric == "nrow"], 20)
  expect_equal(ds$original[ds$metric == "ncol"], 2)
  expect_true(ds$value[ds$metric == "type_match_pct"] > 0)
})

test_that("compare_synthetic() numeric comparison", {
  df <- data.frame(a = rnorm(50, 10, 2), b = rnorm(50, 5, 1))
  spec <- synth_spec(purpose = "demo", n = 100, seed = 1)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  num <- cmp$numeric
  expect_true(nrow(num) >= 1)
  expect_true("std_diff" %in% names(num))
  expect_true("mean_orig" %in% names(num))
  expect_true("mean_syn" %in% names(num))
})

test_that("compare_synthetic() standardized diff is computed correctly", {
  df <- data.frame(x = c(1:4, 5))
  spec <- synth_spec(purpose = "demo", n = 5, seed = 1)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_true(!is.na(cmp$numeric$std_diff[1]))
})

test_that("compare_synthetic() categorical comparison", {
  df <- data.frame(f = factor(rep(c("a", "b", "c"), each = 5)))
  spec <- synth_spec(purpose = "demo", n = 30)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  cat <- cmp$categorical
  expect_true(nrow(cat) >= 1)
  expect_true("tvd" %in% names(cat))
  expect_true("n_levels_orig" %in% names(cat))
  expect_true("n_levels_syn" %in% names(cat))
})

test_that("compare_synthetic() TVD is between 0 and 1", {
  df <- data.frame(f = factor(rep(c("x", "y"), each = 10)))
  spec <- synth_spec(purpose = "demo", n = 50)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_true(cmp$categorical$tvd[1] >= 0)
  expect_true(cmp$categorical$tvd[1] <= 1)
})

test_that("compare_synthetic() relationship with 2+ numeric columns", {
  df <- data.frame(a = 1:20, b = 20:1, c = rnorm(20))
  spec <- synth_spec(purpose = "demo", n = 20)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_true(nrow(cmp$relationship) >= 1)
  expect_true("cor_orig" %in% names(cmp$relationship))
  expect_true("cor_syn" %in% names(cmp$relationship))
  expect_true("cor_diff" %in% names(cmp$relationship))
})

test_that("compare_synthetic() relationship with <2 numeric columns is empty", {
  df <- data.frame(x = letters[1:10], y = factor(rep("a", 10)))
  spec <- synth_spec(purpose = "demo", n = 10)
  syn <- synthesize_data(df, spec)
  expect_message(
    cmp <- compare_synthetic(df, syn),
    "Not enough numeric"
  )
  expect_equal(nrow(cmp$relationship), 0)
})

test_that("compare_synthetic() handles all-NA numeric column", {
  df <- data.frame(x = rep(NA_real_, 10), y = 1:10)
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_true(nrow(cmp$numeric) >= 1)
  expect_true(is.na(cmp$numeric$std_diff[cmp$numeric$variable == "x"]))
})

test_that("compare_synthetic() handles no numeric columns", {
  df <- data.frame(x = letters[1:5], y = factor(letters[1:5]))
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_equal(nrow(cmp$numeric), 0)
})

test_that("compare_synthetic() handles no categorical columns", {
  df <- data.frame(x = 1:5, y = 6:10)
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_equal(nrow(cmp$categorical), 0)
})

test_that("compare_numeric emits sd_ratio, median_std_diff, and test p-values", {
  set.seed(1)
  orig <- data.frame(x = rnorm(200, 10, 2))
  syn  <- data.frame(x = rnorm(200, 10, 2))
  cn <- compare_numeric(orig, syn)

  expect_true(
    all(c("sd_ratio", "median_std_diff",
          "mean_p", "sd_p", "median_p") %in% names(cn)),
    info = paste("Numeric comparison columns:", paste(names(cn), collapse = ", "))
  )
  expect_equal(cn$sd_ratio, cn$sd_syn / cn$sd_orig)
  expect_equal(cn$median_std_diff,
               (cn$median_syn - cn$median_orig) / cn$iqr_orig)
  expect_gt(cn$mean_p, 0.05)
  expect_gt(cn$sd_p, 0.05)
  expect_gt(cn$median_p, 0.05)

  syn2 <- data.frame(x = rnorm(200, 14, 2))
  cn2 <- compare_numeric(orig, syn2)
  expect_lt(cn2$mean_p, 0.05)

  cn3 <- compare_numeric(data.frame(x = rep(5, 3)), data.frame(x = rep(5, 3)))
  expect_true(is.na(cn3$sd_ratio) || is.finite(cn3$sd_ratio))
  expect_true(is.na(cn3$mean_p))
})

test_that("compare_synthetic() rejects non-data-frame", {
  expect_error(
    compare_synthetic("not a df", data.frame(x = 1:3)),
    "must be a data frame"
  )
})

test_that("compare_synthetic() print method works", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  spec <- synth_spec(purpose = "demo")
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_no_error(print(cmp))
})

test_that("compare_synthetic() meta includes generation time", {
  df <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo")
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_s3_class(cmp$meta$generated_at, "POSIXct")
  expect_equal(cmp$meta$nrow_orig, 5)
  expect_equal(cmp$meta$ncol_orig, 1)
})

test_that("compare_synthetic() categorical comparison for character columns", {
  df <- data.frame(txt = c("hello", "world", "hello", "foo", "bar"))
  spec <- synth_spec(purpose = "demo", n = 20, merge_rare = FALSE)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_true(nrow(cmp$categorical) >= 1)
})

test_that("plot_comparison() errors if ggplot2 missing", {
  skip_if(
    requireNamespace("ggplot2", quietly = TRUE),
    "ggplot2 is installed"
  )
  df <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo")
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  expect_error(plot_comparison(cmp))
})

test_that("plot_comparison() returns plots when ggplot2 available", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(x = rnorm(20), f = factor(rep(c("a", "b"), 10)))
  spec <- synth_spec(purpose = "demo", n = 20, seed = 1)
  syn <- synthesize_data(df, spec)
  cmp <- compare_synthetic(df, syn)
  p <- plot_comparison(cmp)
  expect_type(p, "list")
  expect_true(!is.null(p$numeric))
  expect_true(!is.null(p$categorical))
})

test_that("compare_synthetic() works with toy dataset", {
  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "development", seed = 1)
  syn <- synthesize_data(example_health_survey, spec)
  cmp <- compare_synthetic(example_health_survey, syn)
  expect_s3_class(cmp, "dataganger_comparison")
  expect_true(nrow(cmp$numeric) > 0)
  expect_true(nrow(cmp$categorical) > 0)
})

test_that("post-synthesis comparison survives generic column renaming", {
  original <- data.frame(
    age = rep(20:29, each = 4),
    group = rep(c("a", "b"), 20),
    score = seq_len(40),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(original)
  roles$identifies <- "none"
  roles$sensitive <- FALSE
  roles$disclosure_role <- "none"

  spec_preserve <- synth_spec(purpose = "development", seed = 101, n = 40, name_strategy = "preserve")
  spec_generic <- synth_spec(purpose = "development", seed = 101, n = 40, name_strategy = "generic")
  syn_preserve <- synthesize_data(original, spec_preserve, roles = roles)
  syn_generic <- synthesize_data(original, spec_generic, roles = roles)

  flags_preserve <- privacy_check(original, syn_preserve, roles = roles, stage = "post", spec = spec_preserve)
  flags_generic <- privacy_check(original, syn_generic, roles = roles, stage = "post", spec = spec_generic)
  cmp_preserve <- compare_synthetic(original, syn_preserve, roles = roles)
  cmp_generic <- compare_synthetic(original, syn_generic, roles = roles)

  expect_equal(flags_generic$variable, flags_preserve$variable)
  expect_equal(flags_generic$flag, flags_preserve$flag)
  expect_equal(cmp_generic$numeric$variable, cmp_preserve$numeric$variable)
  expect_gt(nrow(cmp_generic$numeric), 0)
  expect_equal(
    exact_row_match_count(original, dg_original_names(syn_generic)),
    exact_row_match_count(original, syn_preserve)
  )
})
