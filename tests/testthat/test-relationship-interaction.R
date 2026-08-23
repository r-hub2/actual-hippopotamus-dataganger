test_that("continuous relationships report preserved and modified correlations", {
  set.seed(101)
  x <- seq(-2, 2, length.out = 300)
  noise_orig <- stats::rnorm(length(x), sd = 0.25)
  noise_synth <- stats::rnorm(length(x), sd = 0.25)

  preserved <- relationship_interaction(
    x, 2 * x + noise_orig, x, 2 * x + noise_synth,
    "numeric", "numeric"
  )
  expect_identical(preserved$family, "continuous")
  expect_identical(preserved$effect_label, "Difference in correlation")
  expect_equal(preserved$estimate, 0, tolerance = 0.08)
  expect_gt(preserved$p_value, 0.1)

  modified <- relationship_interaction(
    x, 2 * x + noise_orig, x, -2 * x + noise_synth,
    "numeric", "numeric"
  )
  expect_identical(modified$family, "continuous")
  expect_equal(modified$estimate, -2, tolerance = 0.08)
  expect_lt(modified$p_value, 1e-10)
})

test_that("continuous relationship comparison is invariant to swapping variables", {
  set.seed(108)
  n <- 400
  x_orig <- stats::rnorm(n)
  y_orig <- 0.7 * x_orig + stats::rnorm(n, sd = 0.6)
  x_synth <- stats::rnorm(n)
  y_synth <- -0.3 * x_synth + stats::rnorm(n, sd = 0.9)

  forward <- relationship_interaction(
    x_orig, y_orig, x_synth, y_synth, "numeric", "numeric"
  )
  reverse <- relationship_interaction(
    y_orig, x_orig, y_synth, x_synth, "numeric", "numeric"
  )

  expect_equal(reverse$estimate, forward$estimate, tolerance = 1e-12)
  expect_equal(reverse$p_value, forward$p_value, tolerance = 1e-12)
})

test_that("binary relationships return an interaction odds ratio", {
  set.seed(102)
  n <- 2500
  x_orig <- stats::rnorm(n)
  x_synth <- stats::rnorm(n)
  make_binary <- function(x, slope) {
    stats::rbinom(length(x), 1, stats::plogis(-0.2 + slope * x))
  }

  y_preserved <- make_binary(x_orig, 1)
  preserved <- relationship_interaction(
    x_orig, y_preserved,
    x_orig, y_preserved,
    "numeric", "categorical"
  )
  expect_identical(preserved$family, "binary")
  expect_identical(preserved$effect_label, "Odds ratio")
  expect_equal(preserved$estimate, 1, tolerance = 0.2)
  expect_gt(preserved$p_value, 0.05)

  set.seed(103)
  modified <- relationship_interaction(
    x_orig, make_binary(x_orig, 1),
    x_synth, make_binary(x_synth, -1),
    "numeric", "categorical"
  )
  expect_identical(modified$family, "binary")
  expect_gt(abs(log(modified$estimate)), 1)
  expect_lt(modified$p_value, 1e-10)
})

test_that("count relationships return a slope ratio", {
  set.seed(104)
  n <- 1800
  x_orig <- factor(rep(c("a", "b"), length.out = n))
  x_synth <- factor(rep(c("a", "b"), length.out = n))
  y_orig <- stats::rpois(n, exp(1 + 0.7 * (x_orig == "b")))
  y_synth <- stats::rpois(n, exp(1 + 0.7 * (x_synth == "b")))

  result <- relationship_interaction(
    x_orig, y_orig, x_synth, y_synth, "categorical", "numeric"
  )
  expect_identical(result$family, "count")
  expect_identical(result$effect_label, "Slope ratio")
  expect_equal(result$estimate, 1, tolerance = 0.2)
  expect_gt(result$p_value, 0.05)
})

test_that("multi-level predictors use a joint interaction test", {
  set.seed(105)
  x_orig <- factor(rep(c("a", "b", "c"), each = 80))
  x_synth <- factor(rep(c("a", "b", "c"), each = 80))
  means <- c(a = 0, b = 1, c = 2)
  y_orig <- unname(means[as.character(x_orig)]) + stats::rnorm(240)
  y_synth <- unname(means[as.character(x_synth)]) + stats::rnorm(240)

  result <- relationship_interaction(
    x_orig, y_orig, x_synth, y_synth, "categorical", "numeric"
  )
  expect_identical(result$effect_label, "Joint interaction")
  expect_true(is.na(result$estimate))
  expect_gt(result$n_terms, 1L)
  expect_true(is.finite(result$p_value))
})

test_that("multi-level outcomes use a multinomial joint interaction test", {
  skip_if_not_installed("nnet")
  set.seed(106)
  n <- 500
  x_orig <- stats::rnorm(n)
  x_synth <- stats::rnorm(n)
  sample_y <- function(x) {
    score <- cbind(0, 0.5 + 0.7 * x, -0.3 - 0.5 * x)
    prob <- exp(score) / rowSums(exp(score))
    factor(apply(prob, 1, function(p) sample(c("a", "b", "c"), 1, prob = p)))
  }
  result <- relationship_interaction(
    x_orig, sample_y(x_orig), x_synth, sample_y(x_synth),
    "numeric", "categorical"
  )
  expect_identical(result$family, "multinomial")
  expect_identical(result$effect_label, "Joint interaction")
  expect_true(is.na(result$estimate))
  expect_gt(result$n_terms, 1L)
  expect_true(is.finite(result$p_value))
})

test_that("degenerate relationships return notes instead of errors", {
  no_variation <- relationship_interaction(
    rep(1, 20), 1:20, rep(1, 20), 1:20, "numeric", "numeric"
  )
  expect_true(is.na(no_variation$estimate))
  expect_true(is.na(no_variation$p_value))
  expect_true(nzchar(no_variation$note))

  too_few <- relationship_interaction(
    1:2, 1:2, 1:2, 1:2, "numeric", "numeric"
  )
  expect_true(is.na(too_few$estimate))
  expect_true(is.na(too_few$p_value))
  expect_true(nzchar(too_few$note))
})

test_that("labelled original columns compare with plain synthetic columns", {
  skip_if_not_installed("haven")

  # The arms must carry a real x -> y association. A perfectly balanced design
  # leaves the binomial likelihood flat, so the fit is floating-point noise and
  # whether the p-value comes back finite depends on the BLAS (CRAN ATLAS).
  set.seed(169)
  n <- 300
  codes <- c(low = 10, middle = 20, high = 30)
  prob_yes <- c(low = 0.25, middle = 0.5, high = 0.75)
  draw_arm <- function() {
    level <- sample(names(codes), n, replace = TRUE)
    list(level = level, yes = stats::runif(n) < prob_yes[level])
  }
  orig <- draw_arm()
  synth <- draw_arm()

  x_orig <- haven::labelled(unname(codes[orig$level]), labels = codes)
  y_orig <- haven::labelled(as.numeric(orig$yes), labels = c(no = 0, yes = 1))
  x_synth <- synth$level
  y_synth <- ifelse(synth$yes, "yes", "no")

  expect_no_error(
    result <- relationship_interaction(
      x_orig, y_orig, x_synth, y_synth,
      "categorical", "categorical"
    )
  )
  expect_identical(result$family, "binary")
  expect_identical(result$note, "")
  expect_gte(result$n_terms, 1L)
  expect_true(is.finite(result$p_value))
})

test_that("non-numeric synthetic labels fall back to categorical comparison", {
  skip_if_not_installed("haven")

  n <- 200
  x_orig <- stats::rnorm(n)
  x_synth <- stats::rnorm(n)
  y_orig <- haven::labelled(
    rep(c(0, 1), length.out = n),
    labels = c(no = 0, yes = 1)
  )
  y_synth <- rep(c("no", "yes"), length.out = n)

  expect_no_warning(
    result <- relationship_interaction(
      x_orig, y_orig, x_synth, y_synth,
      "numeric", "numeric"
    )
  )
  expect_identical(result$family, "binary")
  expect_true(is.finite(result$p_value))
})

test_that("relationship comparison detects modified and preserved relationships", {
  set.seed(107)
  x <- seq(-2, 2, length.out = 300)
  original <- data.frame(x = x, y = 2 * x + stats::rnorm(300, sd = 0.25))
  preserved <- data.frame(x = x, y = 2 * x + stats::rnorm(300, sd = 0.25))
  modified <- data.frame(x = x, y = -2 * x + stats::rnorm(300, sd = 0.25))

  preserved_result <- compare_relationship_interaction(original, preserved)
  modified_result <- compare_relationship_interaction(original, modified)

  expect_gt(preserved_result$p_value[[1]], 0.1)
  expect_lt(modified_result$p_value[[1]], 1e-10)
  expect_identical(modified_result$predictor[[1]], "x")
  expect_identical(modified_result$outcome[[1]], "y")
})

test_that("relationship comparison excludes non-comparable roles", {
  original <- data.frame(id = 1:20, text = rep(c("long note a", "long note b"), 10))
  roles <- detect_roles(original)
  roles$user_role <- c("identifier", "free_text")

  result <- compare_relationship_interaction(original, original, roles)

  expect_equal(nrow(result), 0)
  expect_named(result, c(
    "predictor", "outcome", "family", "effect_label", "estimate",
    "null_value", "p_value", "n_terms", "note"
  ))
})
