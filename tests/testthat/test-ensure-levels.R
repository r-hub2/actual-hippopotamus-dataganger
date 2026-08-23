test_that("ensure_levels_present restores categorical levels to k copies", {
  original <- data.frame(
    group = factor(c(rep("common", 8), "rare_a", "rare_b")),
    stringsAsFactors = FALSE
  )
  syn <- data.frame(
    group = factor(rep("common", 10), levels = levels(original$group)),
    stringsAsFactors = FALSE
  )
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    label_strategy = "preserve",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 3, seed = 71)

  out <- ensure_levels_present(syn, original, roles, spec)

  expect_equal(nrow(out), nrow(syn))
  expect_type(out$group, "character")
  expect_setequal(
    sort(unique(out$group)),
    sort(unique(as.character(original$group)))
  )
  expect_true(
    all(table(out$group) >= 3),
    info = paste("Group counts:", paste(table(out$group), collapse = ", "))
  )
  expect_setequal(unique(as.character(out$group)), unique(as.character(original$group)))
})

test_that("ensure_levels_present is deterministic under the spec seed", {
  original <- data.frame(group = factor(rep(c("a", "b", "c"), each = 4)))
  syn <- data.frame(group = factor(rep("a", 12), levels = levels(original$group)))
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "development", k_anon = 3, seed = 19)

  first <- ensure_levels_present(syn, original, roles, spec)
  second <- ensure_levels_present(syn, original, roles, spec)

  expect_identical(first, second)
})

test_that("ensure_levels_present preserves haven labels and storage", {
  labels <- c(No = 1, Yes = 2, Unknown = 3)
  original <- data.frame(
    response = haven::labelled(c(1, 1, 2, 2, 3, 3, 1, 2, 3), labels = labels)
  )
  syn <- data.frame(
    response = haven::labelled(rep(1, 9), labels = labels)
  )
  roles <- data.frame(
    variable = "response",
    class = "haven_labelled",
    recommended_role = "label_check",
    user_role = NA_character_,
    simulation = "synthesize",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 3, seed = 23)

  out <- ensure_levels_present(syn, original, roles, spec)

  expect_type(out$response, "character")
  expect_setequal(unique(out$response), c("No", "Yes", "Unknown"))
  expect_true(
    all(table(out$response) >= 3),
    info = paste("Response counts:", paste(table(out$response), collapse = ", "))
  )
})

test_that("ensure_levels_present reconstructs labelled storage from text output", {
  labels <- c(No = 1, Yes = 2, Unknown = 3)
  original <- data.frame(
    response = haven::labelled(c(1, 1, 2, 2, 3, 3), labels = labels)
  )
  syn <- data.frame(response = rep("No", 9), stringsAsFactors = FALSE)
  roles <- data.frame(
    variable = "response",
    class = "haven_labelled",
    recommended_role = "label_check",
    user_role = NA_character_,
    simulation = "synthesize",
    label_strategy = "preserve",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 2, seed = 23)

  out <- ensure_levels_present(syn, original, roles, spec)

  # Synthesis of a haven-labelled input now intentionally returns display-label
  # character output, so level restoration must keep that output contract.
  expect_type(out$response, "character")
  expect_equal(nrow(out), nrow(syn))
  expect_setequal(
    unique(out$response),
    c("No", "Yes", "Unknown")
  )
})

test_that("ensure_levels_present restores declared masked placeholders", {
  original <- data.frame(
    group = factor(c(rep("common", 8), "rare_a", "rare_b")),
    stringsAsFactors = FALSE
  )
  syn <- data.frame(group = rep("common", 10), stringsAsFactors = FALSE)
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    label_strategy = "mask_rare",
    stringsAsFactors = FALSE
  )
  spec <- list(
    purpose = "demo",
    k_anon = 2,
    seed = 29,
    rare_level_min_n = 5
  )

  out <- ensure_levels_present(syn, original, roles, spec)

  expect_setequal(
    unique(out$group),
    c("common", "Other category 1", "Other category 2")
  )
  expect_true(
    all(table(out$group) >= 2),
    info = paste("Group counts:", paste(table(out$group), collapse = ", "))
  )
  expect_false(
    any(out$group %in% c("rare_a", "rare_b")),
    info = paste("Output groups:", paste(unique(out$group), collapse = ", "))
  )
})

test_that("ensure_levels_present warns and degrades when n is too small", {
  original <- data.frame(group = factor(c("a", "b", "c")))
  syn <- data.frame(
    group = factor(rep("a", 4), levels = levels(original$group))
  )
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 2, seed = 31)

  expect_warning(
    out <- ensure_levels_present(syn, original, roles, spec),
    "group.*3.*k = 2.*minimum n = 6"
  )

  expect_equal(nrow(out), 4L)
  expect_setequal(unique(as.character(out$group)), c("a", "b", "c"))
})

test_that("ensure_levels_present never sacrifices a last remaining level", {
  original <- data.frame(group = factor(c("a", "b", "c")))
  syn <- data.frame(
    group = factor(c("a", "b"), levels = levels(original$group))
  )
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 2, seed = 37)

  out <- suppressWarnings(ensure_levels_present(syn, original, roles, spec))

  expect_setequal(unique(as.character(out$group)), c("a", "b"))
  expect_false("c" %in% out$group)
})

test_that("ensure_levels_present does not consume missing-value rows", {
  original <- data.frame(group = factor(c(NA, "a", NA, "b", "c")))
  syn <- data.frame(
    group = factor(c(NA, "a", NA, "a", "a"), levels = levels(original$group))
  )
  roles <- data.frame(
    variable = "group",
    class = "factor",
    recommended_role = "categorical candidate",
    user_role = NA_character_,
    simulation = "synthesize",
    stringsAsFactors = FALSE
  )
  spec <- list(purpose = "demo", k_anon = 2, seed = 39)

  out <- suppressWarnings(ensure_levels_present(syn, original, roles, spec))

  expect_equal(which(is.na(out$group)), which(is.na(syn$group)))
})

test_that("ensure_levels_present is off for analytics and non-synthesis actions", {
  original <- data.frame(
    analytics = factor(c("a", "b", "c")),
    kept = factor(c("x", "y", "z"))
  )
  syn <- data.frame(
    analytics = factor(rep("a", 6), levels = levels(original$analytics)),
    kept = factor(rep("x", 6), levels = levels(original$kept))
  )
  roles <- data.frame(
    variable = c("analytics", "kept"),
    class = c("factor", "factor"),
    recommended_role = c("categorical candidate", "categorical candidate"),
    user_role = c(NA_character_, NA_character_),
    simulation = c("synthesize", "pass_through"),
    stringsAsFactors = FALSE
  )

  analytics <- ensure_levels_present(
    syn,
    original,
    roles,
    list(purpose = "analytics", k_anon = 2, seed = 41)
  )
  demo <- ensure_levels_present(
    syn,
    original,
    roles,
    list(purpose = "demo", k_anon = 2, seed = 41)
  )

  expect_identical(analytics, syn)
  expect_identical(demo$kept, syn$kept)
  expect_setequal(unique(as.character(demo$analytics)), c("a", "b", "c"))
})
