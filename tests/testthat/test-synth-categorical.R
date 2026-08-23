# synth_categorical() is the shared resampling path for categorical *and*
# free-text columns (synth_free_text() delegates to it), so its rare-level
# handling is a disclosure control, not just a fidelity knob.
#
# merge_rare defaults to FALSE, matching every purpose preset. The tests below
# that exercise the ".other" merge therefore request merge_rare = TRUE
# explicitly: the merge is still supported, it is just no longer the default.

test_that("rare levels are never drawn into the synthetic output", {
  set.seed(1)
  x <- c(rep("Hypertension", 40), rep("Diabetes", 20),
         "Wilson disease", "Fabry disease")
  syn <- synth_categorical(x, n = 500, rare_level_min_n = 5, merge_rare = TRUE)

  expect_false("Wilson disease" %in% syn)
  expect_false("Fabry disease" %in% syn)
  expect_true(".other" %in% syn)
  # Common values are resampled verbatim -- this is resampling, not generation.
  expect_true(
    all(c("Hypertension", "Diabetes") %in% syn),
    info = paste("Synthetic categories:", paste(unique(syn), collapse = ", "))
  )
})

test_that("categorical values preserve the merged rare slot", {
  set.seed(1)
  x <- c(rep("Hypertension", 40), rep("Diabetes", 20),
         "Wilson disease", "Fabry disease")
  syn <- synth_categorical(x, n = 62, rare_level_min_n = 5, merge_rare = TRUE)

  expect_type(syn, "character")
  expect_setequal(
    sort(unique(syn)),
    c("Hypertension", "Diabetes", ".other")
  )
})

test_that("unused declared levels are not emitted as character values", {
  set.seed(1)
  # A factor input can declare a level with zero rows ("c" below). Ingest
  # converts to character, which drops the declaration on purpose: a category
  # with no rows in the source gets no rows in the synthetic copy, and is
  # never invented as a value.
  x <- factor(c(rep("a", 20), rep("b", 20)), levels = c("a", "b", "c"))
  syn <- synth_categorical(
    as.character(x), n = 40, rare_level_min_n = 5
  )

  expect_type(syn, "character")
  expect_false("c" %in% sort(unique(syn)))
  expect_setequal(sort(unique(syn)), c("a", "b"))
})

test_that("merge_rare = FALSE resamples rare values verbatim", {
  set.seed(1)
  x <- c(rep("common", 60), "singleton")
  syn <- synth_categorical(x, n = 4000, merge_rare = FALSE)

  # This is the analytics preset's behaviour: with merging off, a value seen
  # once in the source is eligible for the sampling pool and does appear.
  expect_true("singleton" %in% syn)
  expect_false(".other" %in% syn)
})

test_that("categorical sampling includes every pool level", {
  x <- c(rep("common", 100), "rare", "uncommon")

  for (seed in c(1, 10, 100)) {
    set.seed(seed)
    syn <- synth_categorical(x, n = 20, merge_rare = FALSE)
    expect_true(
      all(unique(x) %in% syn),
      info = paste("Seed", seed, "synthetic categories:",
                   paste(unique(syn), collapse = ", "))
    )
  }
})

test_that("categorical sampling includes every observed level", {
  x <- c(rep("common", 100), "rare", "uncommon")
  set.seed(1)
  syn <- synth_categorical(x, n = 20, merge_rare = FALSE)

  expect_type(syn, "character")
  expect_true(
    all(sort(unique(x)) %in% syn),
    info = paste("Synthetic categories:", paste(unique(syn), collapse = ", "))
  )
})

test_that("categorical sampling warns when the pool exceeds output size", {
  x <- c("a", "b", "c")

  expect_warning(
    syn <- synth_categorical(x, n = 2, merge_rare = FALSE),
    "Cannot guarantee categorical level presence"
  )
  expect_length(syn, 2)
})

test_that("categorical sampling is deterministic under a fixed seed", {
  x <- c(rep("common", 100), "rare", "uncommon")
  set.seed(42)
  first <- synth_categorical(x, n = 20, merge_rare = FALSE)
  set.seed(42)
  second <- synth_categorical(x, n = 20, merge_rare = FALSE)

  expect_identical(first, second)
})

test_that("rare_level_min_n sets the threshold", {
  set.seed(1)
  x <- c(rep("a", 30), rep("b", 6))
  # b (n = 6) survives a threshold of 5 but not one of 10.
  expect_true("b" %in% synth_categorical(x, n = 300, rare_level_min_n = 5,
    merge_rare = TRUE))
  expect_false("b" %in% synth_categorical(x, n = 300, rare_level_min_n = 10,
    merge_rare = TRUE))
})

test_that("free text routes through the same rare-level control", {
  set.seed(1)
  notes <- c(rep("no concerns", 40),
             "left-handed pilot from Sudbury with Fabry disease")
  syn <- synth_free_text(notes, n = 500, strategy = "categorical",
                         merge_rare = TRUE)

  expect_false("left-handed pilot from Sudbury with Fabry disease" %in% syn)
})

test_that("mask_rare replaces each rare categorical label without merging levels", {
  x <- c(rep("common", 12), rep("less common", 8),
         rep("beta rare", 3), rep("alpha rare", 2))

  set.seed(42)
  syn <- synth_categorical(
    x, n = 100, rare_level_min_n = 5,
    label_strategy = "mask_rare"
  )

  expect_false(
    any(c("alpha rare", "beta rare") %in% syn),
    info = paste("Synthetic categories:", paste(unique(syn), collapse = ", "))
  )
  placeholders <- grep("^Other category [0-9]+$", syn, value = TRUE)
  expect_setequal(unique(placeholders), c("Other category 1", "Other category 2"))
  expect_equal(length(unique(syn)), length(unique(x)))
  expect_true(
    all(c("common", "less common") %in% syn),
    info = paste("Synthetic categories:", paste(unique(syn), collapse = ", "))
  )

  set.seed(42)
  repeated <- synth_categorical(
    x, n = 100, rare_level_min_n = 5,
    label_strategy = "mask_rare"
  )
  expect_identical(syn, repeated)
})

test_that("mask_rare overrides rare merging and preserves distinct values", {
  x <- c(rep("common", 12), "alpha rare", "beta rare")
  set.seed(1)
  syn <- synth_categorical(
    x, n = 40, rare_level_min_n = 5,
    merge_rare = TRUE, label_strategy = "mask_rare"
  )

  expect_type(syn, "character")
  syn_values <- sort(unique(syn))
  expect_false(
    any(c("alpha rare", "beta rare") %in% syn_values),
    info = paste("Synthetic categories:", paste(syn_values, collapse = ", "))
  )
  expect_setequal(
    grep("^Other category [0-9]+$", syn_values, value = TRUE),
    c("Other category 1", "Other category 2")
  )
  expect_false(".other" %in% syn)
  expect_equal(length(syn_values), length(sort(unique(x))))
})

test_that("free text categorical synthesis honours mask_rare", {
  notes <- c(rep("no concerns", 10), "rare note one", "rare note two")
  set.seed(1)
  syn <- synth_free_text(
    notes, n = 30, strategy = "categorical",
    rare_level_min_n = 5, label_strategy = "mask_rare"
  )

  expect_false(
    any(c("rare note one", "rare note two") %in% syn),
    info = paste("Synthetic notes:", paste(unique(syn), collapse = " | "))
  )
  expect_setequal(
    grep("^Other category [0-9]+$", syn, value = TRUE),
    c("Other category 1", "Other category 2")
  )
})

test_that("unknown label_strategy aborts", {
  expect_error(
    synth_categorical(c("common", "rare"), n = 10, label_strategy = "unknown"),
    "Unknown label strategy"
  )
})

test_that("an all-NA column yields all NA", {
  output <- synth_categorical(c(NA, NA), n = 5)
  expect_true(
    all(is.na(output)),
    info = paste("All-NA synthesis output:", paste(output, collapse = ", "))
  )
})
