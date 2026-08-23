test_that("dg_identifies_option_meta returns the three identifies options in order", {
  m <- dg_identifies_option_meta()
  expect_equal(vapply(m, `[[`, "", "value"), c("none", "combination", "direct"))
  expect_match(m[[2]]$label, "combination")
  expect_match(m[[3]]$label, "identifies a person")
})

test_that("axes project to legacy disclosure_role", {
  expect_equal(dg_axes_to_role("direct", FALSE), "direct")
  expect_equal(dg_axes_to_role("combination", TRUE), "quasi")
  expect_equal(dg_axes_to_role("none", TRUE), "sensitive")
  expect_equal(dg_axes_to_role("none", FALSE), "none")
  expect_true(is.na(dg_axes_to_role(NA_character_, FALSE)))
})

test_that("legacy disclosure_role back-fills axes", {
  expect_equal(
    dg_role_to_axes("quasi"),
    list(identifies = "combination", sensitive = FALSE)
  )
  expect_equal(
    dg_role_to_axes("sensitive"),
    list(identifies = "none", sensitive = TRUE)
  )
  expect_equal(
    dg_role_to_axes("direct"),
    list(identifies = "direct", sensitive = FALSE)
  )
  expect_equal(
    dg_role_to_axes("none"),
    list(identifies = "none", sensitive = FALSE)
  )
  expect_equal(
    dg_role_to_axes(NA_character_),
    list(identifies = NA_character_, sensitive = FALSE)
  )
})

test_that("derived action keys off identifies only", {
  expect_equal(dg_derived_action_axes("direct", TRUE), "drop")
  expect_equal(dg_derived_action_axes("combination", FALSE), "synthesize")
  expect_equal(dg_derived_action_axes(NA_character_, FALSE), "synthesize")
})

test_that("treatment text reflects both axes", {
  expect_match(dg_treatment_text_axes("direct", FALSE), "Removed")
  expect_match(dg_treatment_text_axes("combination", FALSE), "Coarsened")
  expect_match(dg_treatment_text_axes("combination", TRUE), "grouped with k-anonymity so no rare combination survives")
  expect_match(dg_treatment_text_axes("none", TRUE), "attribute-level protection is not yet applied")
  expect_match(dg_treatment_text_axes("none", FALSE), "observed distribution with noise")
  expect_match(dg_treatment_text_axes(NA_character_, FALSE), "needs an answer")
})

test_that("dg_kanon_columns unions quasi with identifying-sensitive", {
  roles <- data.frame(
    variable = c("zip", "religion", "income", "name"),
    disclosure_role = c("quasi", "sensitive", "sensitive", "direct"),
    recommended_role = c("categorical candidate", "categorical candidate", "numeric", "free text"),
    stringsAsFactors = FALSE
  )
  out <- dg_kanon_columns(roles)
  expect_true(
    all(c("zip", "religion") %in% out),
    info = paste("Selected columns:", paste(out, collapse = ", "))
  )
  expect_false("income" %in% out)
  expect_false("name" %in% out)
})

test_that("dg_kanon_columns unions combination and discrete sensitive", {
  roles <- data.frame(
    variable = c("age", "income", "diag", "bmi"),
    recommended_role = c("numeric", "numeric", "categorical candidate", "numeric"),
    identifies = c("combination", "combination", "none", "none"),
    sensitive = c(FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  qi <- dg_kanon_columns(roles)
  expect_true(
    all(c("age", "income", "diag") %in% qi),
    info = paste("Quasi-identifier columns:", paste(qi, collapse = ", "))
  )
  expect_false("bmi" %in% qi)
})

test_that("dg_kanon_columns falls back to disclosure_role when axes absent", {
  roles <- data.frame(
    variable = c("age", "x"),
    class = c("numeric", "numeric"),
    disclosure_role = c("quasi", "none"),
    stringsAsFactors = FALSE
  )
  expect_equal(dg_kanon_columns(roles), "age")
})

test_that("dg_kanon_columns is empty/NA-safe", {
  expect_equal(dg_kanon_columns(NULL), character(0))
  roles <- data.frame(
    variable = "x",
    disclosure_role = NA_character_,
    class = "numeric",
    stringsAsFactors = FALSE
  )
  expect_equal(dg_kanon_columns(roles), character(0))
})

test_that("dg_suggest_disclosure maps detected class to a protective suggestion or unset", {
  expect_equal(dg_suggest_disclosure("alphanumeric ID"), "direct")
  expect_equal(dg_suggest_disclosure("free text"), "direct")
  expect_equal(dg_suggest_disclosure("date"), "quasi")
  expect_equal(dg_suggest_disclosure("numeric"), "none")
  expect_equal(dg_suggest_disclosure("logical"), "none")
  expect_true(is.na(dg_suggest_disclosure("categorical candidate")))
  expect_true(is.na(dg_suggest_disclosure("unknown")))
})

test_that("dg_seed_disclosure seeds protective suggestions, leaves ambiguous unset", {
  roles <- data.frame(
    variable = c("id", "dob", "bp", "arm"),
    class = c("alphanumeric ID", "date", "numeric", "categorical candidate"),
    disclosure_role = rep("", 4),
    stringsAsFactors = FALSE
  )
  out <- dg_seed_disclosure(roles)
  expect_equal(out$identifies, c("direct", "combination", "none", ""))
  expect_equal(out$disclosure_role, c("direct", "quasi", "none", NA))
})


test_that("dg_decision_recap_table builds the generate recap rows", {
  roles <- tibble::tibble(
    variable = c("name", "zip", "bp"),
    recommended_role = c("alphanumeric ID", "categorical candidate", "numeric"),
    user_role = c(NA_character_, "date", NA_character_),
    class = c("alphanumeric ID", "categorical candidate", "numeric"),
    identifies = c("direct", "combination", "none"),
    sensitive = c(FALSE, TRUE, FALSE),
    simulation = c("drop", "pass_through", "synthesize")
  )

  out <- dg_decision_recap_table(roles)

  expect_equal(out$variable, c("name", "zip", "bp"))
  expect_equal(out$points_to_person, c("Yes \u2014 it identifies a person on its own", "Only in combination with other columns", "No \u2014 not a person-level identifier"))
  expect_equal(out$sensitive, c("No", "Yes", "No"))
  expect_equal(out$action, c("Drop", "Pass through", "Synthesize"))
  expect_match(out$what_we_do[[1]], "Removed")
  expect_match(out$what_we_do[[2]], "grouped with k-anonymity so no rare combination survives")
  expect_match(out$what_we_do[[3]], "observed distribution with noise")
  expect_equal(out$type, c("alphanumeric_id", "date", "numeric"))
})

test_that("dg_decision_recap_table is robust to missing columns", {
  roles <- data.frame(
    variable = c("x", "y"),
    recommended_role = c(NA_character_, NA_character_),
    class = c("numeric", "date"),
    stringsAsFactors = FALSE
  )

  out <- dg_decision_recap_table(roles)

  expect_equal(out$points_to_person, c("\u2014", "\u2014"))
  expect_equal(out$sensitive, c("No", "No"))
  expect_equal(out$action, c("Synthesize", "Synthesize"))
  expect_match(out$what_we_do[[1]], "needs an answer")
  expect_match(out$type[[1]], "numeric")
  expect_match(out$type[[2]], "date")
})


test_that("roles_ready_for_generation only requires answered eligible columns", {
  expect_false(roles_ready_for_generation(NULL))

  roles <- data.frame(
    variable = c("id", "zip", "notes"),
    identifies = c("direct", "combination", ""),
    sensitive = c(FALSE, FALSE, FALSE),
    simulation = c("drop", "synthesize", "drop"),
    stringsAsFactors = FALSE
  )
  expect_true(roles_ready_for_generation(roles))

  roles$simulation[3] <- "synthesize"
  expect_false(roles_ready_for_generation(roles))
  expect_equal(roles_generation_pending(roles), 3L)
})

test_that("dg_kanon_columns includes sensitive named categorical columns from detected roles", {
  df <- data.frame(
    diagnosis = rep(c("flu", "cold", "asthma"), each = 4),
    score = seq_len(12),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)

  expect_true("diagnosis" %in% dg_kanon_columns(roles))
})

test_that("dg_max_comparable_levels scales with n between the floor and cap", {
  # Small n: floor applies even though n/rare_level_min_n would be smaller.
  expect_equal(dg_max_comparable_levels(10), 5L)
  # Mid-range: scales as n / rare_level_min_n.
  expect_equal(dg_max_comparable_levels(100), 20L)
  # Large n: capped so the chart stays legible.
  expect_equal(dg_max_comparable_levels(10000), 30L)
})

test_that("dg_max_comparable_levels respects custom rare_level_min_n/floor/cap", {
  expect_equal(dg_max_comparable_levels(1000, rare_level_min_n = 10), 30L)
  expect_equal(dg_max_comparable_levels(1000, rare_level_min_n = 10, cap_levels = 50L), 50L)
  expect_equal(dg_max_comparable_levels(1, floor_levels = 3L), 3L)
})

test_that("dg_max_comparable_levels is NA/zero-safe", {
  expect_equal(dg_max_comparable_levels(NA), 5L)
  expect_equal(dg_max_comparable_levels(0), 5L)
  expect_equal(dg_max_comparable_levels(NULL), 5L)
})

test_that("dg_suggest_disclosure returns quasi for postal code", {
  expect_equal(dataganger:::dg_suggest_disclosure("postal code"), "quasi")
})

test_that("dg_kanon_columns includes postal code columns", {
  roles <- data.frame(
    variable = c("postal_code", "name", "age"),
    identifies = c("combination", NA, NA),
    sensitive = c(NA, NA, NA),
    recommended_role = c("postal code", "categorical candidate", "numeric"),
    disclosure_role = c("quasi", "none", "none"),
    stringsAsFactors = FALSE
  )
  cols <- dataganger:::dg_kanon_columns(roles)
  expect_true("postal_code" %in% cols)
})
