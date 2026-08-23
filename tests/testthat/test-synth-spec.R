
# Tests for synth_spec() -- [2.1]-[2.4]

test_that("synth_spec() returns dataganger_spec for each valid purpose", {
  purposes <- c("demo", "development", "analytics")
  for (p in purposes) {
    s <- if (p == "analytics") {
      synth_spec(purpose = p, acknowledge_risk = TRUE)
    } else {
      synth_spec(purpose = p)
    }
    expect_s3_class(s, "dataganger_spec")
  }
})

test_that("synth_spec() maps presets correctly", {
  s <- synth_spec(purpose = "demo")
  expect_equal(s$level, "marginal")
  expect_equal(s$preserve_correlations, "none")
  expect_equal(s$coarsen_dates, TRUE)
  expect_equal(s$name_strategy, "preserve")
  expect_equal(s$free_text_strategy, "categorical")
  expect_equal(s$merge_rare, FALSE)
  expect_equal(s$label_strategy, "mask_rare")

  s <- synth_spec(purpose = "development")
  expect_equal(s$level, "marginal")
  expect_equal(s$preserve_correlations, "moderate")
  expect_equal(s$coarsen_dates, FALSE)
  expect_equal(s$name_strategy, "preserve")
  expect_equal(s$merge_rare, FALSE)
  expect_equal(s$label_strategy, "mask_rare")

  s <- synth_spec(purpose = "analytics", acknowledge_risk = TRUE)
  expect_equal(s$level, "hifi")
  expect_equal(s$preserve_correlations, "high")
  expect_equal(s$free_text_strategy, "categorical")
  expect_equal(s$merge_rare, FALSE)
  expect_equal(s$label_strategy, "preserve")
})

test_that("synth_spec() rejects invalid purpose", {
  expect_error(
    synth_spec(purpose = "bogus"),
    "Invalid purpose"
  )
})

test_that("synth_spec() rejects acknowledge_risk = FALSE for analytics", {
  expect_error(
    synth_spec(purpose = "analytics"),
    "acknowledge_risk"
  )
})

test_that("synth_spec() accepts acknowledge_risk = TRUE for analytics", {
  expect_no_error(
    synth_spec(purpose = "analytics", acknowledge_risk = TRUE)
  )
})


test_that("synth_spec() rejects non-positive n", {
  expect_error(
    synth_spec(purpose = "demo", n = -5),
    "must be > 0"
  )
  expect_error(
    synth_spec(purpose = "demo", n = 0),
    "must be > 0"
  )
})

test_that("synth_spec() rejects rare_level_min_n <= 1", {
  expect_error(
    synth_spec(purpose = "demo", rare_level_min_n = 1),
    "must be > 1"
  )
  expect_error(
    synth_spec(purpose = "demo", rare_level_min_n = 0),
    "must be > 1"
  )
})

test_that("synth_spec() rejects invalid level", {
  expect_error(
    synth_spec(purpose = "demo", level = "super_hifi"),
    "Invalid level"
  )
})

test_that("synth_spec() rejects invalid name_strategy", {
  expect_error(
    synth_spec(purpose = "demo", name_strategy = "encrypt"),
    "Invalid name_strategy"
  )
})

test_that("synth_spec() rejects an invalid label_strategy", {
  expect_error(
    synth_spec(purpose = "demo", label_strategy = "merge_rare"),
    "Invalid label_strategy"
  )
})

test_that("synthpop label masking preserves rare-level slots", {
  data <- data.frame(
    category = factor(c(rep("common", 8L), "beta rare", "alpha rare")),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(data)
  roles$user_role[roles$variable == "category"] <- "categorical"
  spec <- synth_spec(purpose = "development", rare_level_min_n = 3L)

  masked <- synthpop_mask_rare_inputs(data, spec, roles)

  expect_false(
    any(c("alpha rare", "beta rare") %in% masked$category),
    info = paste("Masked categories:", paste(unique(masked$category), collapse = ", "))
  )
  expect_setequal(
    grep("^Other category [0-9]+$", masked$category, value = TRUE),
    c("Other category 1", "Other category 2")
  )
  expect_equal(length(unique(masked$category)), length(unique(data$category)))
})

test_that("synth_spec() accepts exact missingness", {
  expect_silent(
    synth_spec(purpose = "demo", preserve_missingness = "exact")
  )
})

test_that("synth_spec() user overrides take precedence", {
  s <- synth_spec(purpose = "demo", n = 500, seed = 123,
                  name_strategy = "generic")
  expect_equal(s$n, 500)
  expect_equal(s$seed, 123)
  expect_equal(s$name_strategy, "generic")
  # Demo default level should still hold
  expect_equal(s$level, "marginal")
})

test_that("synth_spec() accepts engine = \"auto\" without recording an explicit engine", {
  s <- synth_spec(purpose = "development", engine = "auto")
  expect_null(s[["engine", exact = TRUE]])
})

test_that("synth_spec() print method works", {
  s <- synth_spec(purpose = "demo")
  expect_no_error(print(s))
})

test_that("synth_spec() records purpose", {
  s <- synth_spec(purpose = "development")
  expect_equal(s$purpose, "development")
})

test_that("synth_spec() accepts all 3 purposes without extra args", {
  for (p in c("demo", "development")) {
    expect_no_error(synth_spec(purpose = p))
  }
  expect_no_error(synth_spec(purpose = "analytics", acknowledge_risk = TRUE))
})

test_that("synth_spec carries k_anon with a default of 5 and validates it", {
  spec <- synth_spec(purpose = "demo")
  expect_equal(spec$k_anon, 5)

  spec2 <- synth_spec(purpose = "demo", k_anon = 10)
  expect_equal(spec2$k_anon, 10)

  expect_error(synth_spec(purpose = "demo", k_anon = 1), "k_anon")
})


test_that("synth_spec() requires a single non-missing purpose string", {
  expect_error(synth_spec(purpose = NULL), "single non-missing character string")
  expect_error(synth_spec(purpose = NA_character_), "single non-missing character string")
  expect_error(synth_spec(purpose = c("demo", "development")), "single non-missing character string")
  expect_error(synth_spec(purpose = ""), "single non-missing character string")
})
