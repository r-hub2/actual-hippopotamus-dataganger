local({

# Tests for synthesize_data() - [2.9]-[2.14]

synthesize_data <- dataganger::synthesize_data
detect_roles <- dataganger::detect_roles
synth_spec <- dataganger::synth_spec

# ---- Schema synthesis ----

test_that("synthesize_data() schema level returns typed placeholder rows", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  spec <- synth_spec(purpose = "demo", level = "schema", name_strategy = "preserve")
  syn <- synthesize_data(df, spec)
  expect_s3_class(syn, "dataganger_synthetic")
  expect_s3_class(syn, "tbl_df")
  expect_equal(nrow(syn), nrow(df))
  expect_equal(ncol(syn), 2)
  expect_named(syn, c("x", "y"))
})

test_that("synthesize_data() schema preserves types", {
  df <- data.frame(
    num  = 1:3,
    chr  = letters[1:3],
    fac  = factor(c("a", "b", "c")),
    lgl  = c(TRUE, FALSE, TRUE),
    dt   = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    stringsAsFactors = FALSE
  )
  spec <- synth_spec(purpose = "demo", level = "schema", name_strategy = "preserve")
  syn <- synthesize_data(df, spec)
  expect_type(syn$num, "double")
  expect_type(syn$chr, "character")
  expect_type(syn$fac, "character")
  expect_type(syn$lgl, "logical")
  expect_s3_class(syn$dt, "Date")
})

test_that("synthesize_data() schema with haven_labelled", {
  df <- tibble::tibble(
    status = haven::labelled(c(1, 2, 1), labels = c(A = 1, B = 2))
  )
  spec <- synth_spec(purpose = "demo", level = "schema", name_strategy = "preserve")
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), nrow(df))
  expect_equal(ncol(syn), 1)
  expect_type(syn$status, "character")
})

# ---- Marginal synthesis ----

test_that("synthesize_data() marginal returns correct dimensions", {
  df <- data.frame(x = 1:10, y = letters[1:10])
  spec <- synth_spec(purpose = "demo", n = 20)
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), 20)
  expect_equal(ncol(syn), 2)
  expect_s3_class(syn, "dataganger_synthetic")
})

test_that("synthesize_data() marginal numeric column", {
  set.seed(1)
  df <- data.frame(val = rnorm(100, mean = 50, sd = 10))
  spec <- synth_spec(purpose = "demo", n = 200)
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), 200)
  expect_type(syn$val, "double")
  lower <- min(df$val, na.rm = TRUE) - 1e-8
  upper <- max(df$val, na.rm = TRUE) + 1e-8
  expect_equal(syn$val[syn$val < lower], numeric(), info = "Values below source range")
  expect_equal(syn$val[syn$val > upper], numeric(), info = "Values above source range")
})

test_that("synthesize_data() jitters zero-IQR numeric columns with nonconstant outliers", {
  original <- data.frame(
    salary = c(rep(100, 55), 150, 200, 250, 300)
  )
  roles <- detect_roles(original)
  roles$identifies <- "none"
  roles$sensitive <- FALSE
  roles$disclosure_role <- "none"
  # Pin the internal engine: synth_numeric jitter is internal-engine behavior,
  # and the single-column fixture cannot be synthesized by synthpop at all.
  spec <- synth_spec(purpose = "development", seed = 22, n = nrow(original),
                     engine = "internal")

  syn <- synthesize_data(original, spec, roles = roles)

  exact_share <- mean(syn$salary %in% original$salary)
  non_modal_originals <- setdiff(unique(original$salary), 100)
  expect_lt(exact_share, 1)
  expect_false(
    all(non_modal_originals %in% syn$salary),
    info = paste("Synthetic salary values:", paste(unique(syn$salary), collapse = ", "))
  )
})

test_that("synthesize_data() marginal factor column", {
  df <- data.frame(group = factor(rep(c("A", "B", "C"), each = 10)))
  spec <- synth_spec(purpose = "demo", n = 50)
  syn <- synthesize_data(df, spec)
  expect_type(syn$group, "character")
  expect_equal(
    setdiff(unique(as.character(syn$group)), c("A", "B", "C")),
    character(), info = paste("Synthetic groups:", paste(unique(syn$group), collapse = ", "))
  )
})

test_that("synthesize_data() marginal Date column", {
  df <- data.frame(
    dt = as.Date(c("2023-01-15", "2023-06-20", "2023-12-01"))
  )
  spec <- synth_spec(purpose = "demo", n = 10)
  syn <- synthesize_data(df, spec)
  expect_s3_class(syn$dt, "Date")
})

test_that("synthesize_data() marginal logical column", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE, TRUE))
  spec <- synth_spec(purpose = "demo", n = 50)
  syn <- synthesize_data(df, spec)
  expect_type(syn$flag, "logical")
})

test_that("synthesize_data() marginal haven_labelled column", {
  df <- tibble::tibble(
    status = haven::labelled(
      c(1, 2, 1, 2, 1),
      labels = c(Active = 1, Inactive = 2),
      label = "Status"
    )
  )
  spec <- synth_spec(purpose = "demo", n = 20, merge_rare = FALSE)
  syn <- synthesize_data(df, spec)
  expect_type(syn$status, "character")
  # mask_rare replaced the old merge_rare default, so rare labels may be
  # represented by distinct neutral placeholders.
  # Report the offending values rather than a bare FALSE.
  from_source <- is.na(syn$status) |
    syn$status %in% c("Active", "Inactive") |
    grepl("^Other category [0-9]+$", syn$status)
  expect_equal(
    unique(syn$status[!from_source]), character(),
    info = paste(
      "values that are neither a source level, NA, nor a rare-category",
      "replacement label"
    )
  )
})

test_that("synthesize_data() marginal POSIXct column", {
  df <- data.frame(
    ts = as.POSIXct(c("2024-01-01 12:00:00", "2024-06-15 08:30:00")),
    stringsAsFactors = FALSE
  )
  spec <- synth_spec(purpose = "demo", n = 10)
  syn <- synthesize_data(df, spec)
  expect_s3_class(syn$ts, "POSIXct")
})

test_that("synthesize_data() preserves missingness at approximate rate", {
  df <- data.frame(
    val = c(rnorm(80), rep(NA_real_, 20))
  )
  spec <- synth_spec(purpose = "demo", n = 1000,
                     preserve_missingness = "approx")
  syn <- synthesize_data(df, spec)
  na_rate <- sum(is.na(syn$val)) / nrow(syn)
  expect_true(na_rate > 0.05 && na_rate < 0.35)
})

test_that("synthesize_data() missingness = none produces no NAs", {
  df <- data.frame(val = c(rnorm(5), NA))
  spec <- synth_spec(purpose = "demo", n = 50,
                     preserve_missingness = "none")
  syn <- synthesize_data(df, spec)
  expect_equal(sum(is.na(syn$val)), 0)
})

# ---- Engine checks ----

test_that("synthesize_data() errors when synthpop is not installed and engine = 'synthpop'", {
  skip_if(requireNamespace("synthpop", quietly = TRUE), "synthpop is installed")
  df   <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo")
  expect_error(
    synthesize_data(df, spec, engine = "synthpop"),
    "synthpop"
  )
})

test_that("synthesize_data() accepts engine = 'marginal' as alias for internal", {
  df   <- data.frame(x = 1:10, y = rnorm(10))
  spec <- synth_spec(purpose = "demo")
  syn  <- synthesize_data(df, spec, engine = "marginal")
  expect_s3_class(syn, "dataganger_synthetic")
})

test_that("synthesize_data() derives synthpop when installed", {
  skip_if_no_synthpop()
  df <- data.frame(
    x = rnorm(30),
    y = rep(letters[1:3], length.out = 30),
    stringsAsFactors = FALSE
  )
  spec <- suppressWarnings(synth_spec(purpose = "development", seed = 1L))
  syn <- synthesize_data(df, spec)
  expect_equal(attr(syn, "engine"), "synthpop")
})

test_that("synthesize_data() falls back when derived synthpop is unavailable", {
  skip_if(requireNamespace("synthpop", quietly = TRUE), "synthpop is installed")
  df <- data.frame(x = 1:20, y = rep(letters[1:4], each = 5))
  spec <- suppressWarnings(synth_spec(purpose = "development", seed = 1L))
  expect_warning(
    syn <- synthesize_data(df, spec),
    "Install .*synthpop.*full-fidelity"
  )
  expect_s3_class(syn, "dataganger_synthetic")
  expect_equal(attr(syn, "engine"), "internal")
})

test_that("synthesize_data() routes objectives to expected engines", {
  df <- data.frame(x = 1:20, y = rep(letters[1:4], each = 5))

  teaching <- synth_spec(purpose = "demo", seed = 1L)
  syn_teaching <- synthesize_data(df, teaching)
  expect_equal(attr(syn_teaching, "engine"), "internal")

  model <- suppressWarnings(synth_spec(purpose = "development", seed = 1L))
  hifi <- synth_spec(purpose = "analytics", seed = 1L, acknowledge_risk = TRUE)

  if (!requireNamespace("synthpop", quietly = TRUE)) {
    # synthpop truly unavailable: derived synthpop warns and falls back
    expect_warning(expect_equal(attr(synthesize_data(df, model), "engine"), "internal"), "synthpop")
    expect_warning(expect_equal(attr(synthesize_data(df, hifi), "engine"), "internal"), "synthpop")
  } else if (isTRUE(getOption("dataganger.disable_synthpop", FALSE))) {
    # synthpop installed but disabled via option: silent route to internal
    expect_equal(attr(synthesize_data(df, model), "engine"), "internal")
    expect_equal(attr(synthesize_data(df, hifi), "engine"), "internal")
  } else {
    expect_equal(attr(synthesize_data(df, model), "engine"), "synthpop")
    expect_equal(attr(synthesize_data(df, hifi), "engine"), "synthpop")
  }
})

test_that("synthesize_data() explicit internal overrides a synthpop-implying spec", {
  df <- data.frame(x = 1:5)
  spec <- suppressWarnings(synth_spec(purpose = "development"))
  syn <- synthesize_data(df, spec, engine = "internal")
  expect_equal(attr(syn, "engine"), "internal")
})

test_that("synthesize_data() explicit auto remains objective-derived", {
  df <- data.frame(x = 1:20, y = rep(letters[1:4], each = 5))
  withr::local_options(list(dataganger.disable_synthpop = FALSE))
  demo_spec <- synth_spec(purpose = "demo", engine = "auto", seed = 1L)
  expect_equal(attr(synthesize_data(df, demo_spec), "engine"), "internal")
  expect_equal(attr(synthesize_data(df, demo_spec, engine = "auto"), "engine"), "internal")
  dev_spec <- suppressWarnings(synth_spec(purpose = "development", engine = "auto", seed = 1L))
  if (requireNamespace("synthpop", quietly = TRUE)) {
    expect_equal(attr(synthesize_data(df, dev_spec), "engine"), "synthpop")
  } else {
    expect_warning(expect_equal(attr(synthesize_data(df, dev_spec), "engine"), "internal"), "synthpop")
  }
})

test_that("synthesize_data() disable_synthpop is honored under auto", {
  skip_if_no_synthpop()
  df <- data.frame(x = 1:20, y = rep(letters[1:4], each = 5))
  withr::local_options(list(dataganger.disable_synthpop = TRUE))
  spec <- suppressWarnings(synth_spec(purpose = "development", engine = "auto", seed = 1L))
  syn <- synthesize_data(df, spec)
  expect_equal(attr(syn, "engine"), "internal")
})

# ---- Seed reproducibility ----

test_that("synthesize_data() seed produces identical output", {
  df <- data.frame(x = rnorm(50), y = letters[1:50])
  spec <- synth_spec(purpose = "demo", n = 30, seed = 42)
  syn1 <- synthesize_data(df, spec)
  syn2 <- synthesize_data(df, spec)
  expect_equal(syn1$x, syn2$x)
  expect_equal(syn1$y, syn2$y)
})

test_that("synthesize_data() seed isolation does not mutate global RNG", {
  df <- data.frame(x = 1:10)
  set.seed(999)
  before <- .Random.seed
  spec <- synth_spec(purpose = "demo", n = 5, seed = 123)
  syn <- synthesize_data(df, spec)
  after <- .Random.seed
  expect_equal(before, after)
})

# ---- Edge cases ----

test_that("synthesize_data() all-NA numeric column", {
  df <- data.frame(x = rep(NA_real_, 10))
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), 5)
  expect_equal(which(!is.na(syn$x)), integer(), info = "Non-missing row indices in syn$x")
})

test_that("synthesize_data() all-NA character column", {
  df <- data.frame(x = rep(NA_character_, 10))
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  expect_equal(which(!is.na(syn$x)), integer(), info = "Non-missing row indices in syn$x")
})

test_that("synthesize_data() all-NA haven_labelled column", {
  df <- tibble::tibble(
    x = haven::labelled(
      rep(NA_real_, 5),
      labels = c(A = 1, B = 2)
    )
  )
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  expect_equal(which(!is.na(syn$x)), integer(), info = "Non-missing row indices in syn$x")
  expect_type(syn$x, "character")
})

test_that("synthesize_data() 1-level factor does not error", {
  df <- data.frame(f = factor(rep("only", 10)))
  spec <- synth_spec(purpose = "demo", n = 20)
  expect_no_error(syn <- synthesize_data(df, spec))
  expect_type(syn$f, "character")
})

test_that("synthesize_data() 0-row input schema works", {
  df <- data.frame(x = numeric(0), y = character(0))
  spec <- synth_spec(purpose = "demo", level = "schema")
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), 0)
  expect_equal(ncol(syn), 2)
})

test_that("synth_spec() rejects n = 0 for public API", {
  expect_error(
    synth_spec(purpose = "demo", n = 0),
    "must be > 0"
  )
})

test_that("synthesize_data() 1-row input does not error", {
  df <- data.frame(x = 42, y = "hello")
  spec <- synth_spec(purpose = "demo", n = 10)
  expect_no_error(syn <- synthesize_data(df, spec))
  expect_equal(nrow(syn), 10)
})

test_that("synthesize_data() 100% missing column handled", {
  df <- data.frame(
    good = 1:10,
    all_na = rep(NA_real_, 10)
  )
  spec <- synth_spec(purpose = "demo", n = 5)
  syn <- synthesize_data(df, spec)
  expect_equal(which(!is.na(syn$all_na)), integer(),
               info = "Non-missing row indices in syn$all_na")
  expect_false(all(is.na(syn$good)), info = "syn$good was entirely missing")
})

# ---- Name strategies ----

test_that("synthesize_data() name_strategy 'generic' renames columns", {
  df <- data.frame(patient_name = 1:5, age_years = 21:25)
  spec <- synth_spec(purpose = "demo", n = 5, name_strategy = "generic")
  syn <- synthesize_data(df, spec)
  expect_named(syn, c("col_1", "col_2"))
  expect_equal(attr(syn, "spec")$name_map[["patient_name"]], "col_1")
  expect_equal(attr(syn, "spec")$name_map[["age_years"]], "col_2")
})

test_that("synthesize_data() name_strategy 'dictionary_only' renames and stores map", {
  df <- data.frame(patient_name = 1:5, age_years = 21:25)
  spec <- synth_spec(purpose = "demo", n = 5,
                     name_strategy = "dictionary_only")
  syn <- synthesize_data(df, spec)
  expect_named(syn, c("col_1", "col_2"))
  expect_true(!is.null(attr(syn, "spec")$name_map))
  expect_equal(attr(syn, "spec")$name_map[["patient_name"]], "col_1")
  expect_equal(attr(syn, "spec")$name_map[["age_years"]], "col_2")
})

test_that("synthesize_data() name_strategy 'preserve' keeps original names", {
  df <- data.frame(patient_name = 1:5, age_years = 21:25)
  spec <- synth_spec(purpose = "demo", n = 5, name_strategy = "preserve")
  syn <- synthesize_data(df, spec)
  expect_named(syn, c("patient_name", "age_years"))
})

# ---- Attributes ----

test_that("synthesize_data() returns correct attributes", {
  df <- data.frame(x = 1:10)
  spec <- synth_spec(purpose = "demo", n = 5, seed = 7)
  syn <- synthesize_data(df, spec)
  expect_true(!is.null(attr(syn, "spec")))
  expect_equal(attr(syn, "original_dims"), list(nrow = 10, ncol = 1))
  expect_equal(attr(syn, "seed_used"), 7)
  expect_s3_class(attr(syn, "generated_at"), "POSIXct")
})

# ---- Input validation ----

test_that("synthesize_data() rejects non-data-frame", {
  expect_error(
    synthesize_data("not data", synth_spec(purpose = "demo")),
    "must be a data frame"
  )
})

test_that("synthesize_data() rejects non-spec", {
  expect_error(
    synthesize_data(data.frame(x = 1:3), "not a spec"),
    "dataganger_spec"
  )
})

test_that("synthesize_data() roles are optional", {
  df <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo", n = 10)
  expect_no_error(synthesize_data(df, spec, roles = NULL))
})

test_that("synthesize_data() marginal with character column", {
  df <- data.frame(txt = rep(c("hello", "world", "foo", "bar"), each = 3))
  spec <- synth_spec(purpose = "demo", n = 20, merge_rare = FALSE)
  syn <- synthesize_data(df, spec)
  expect_type(syn$txt, "character")
  # mask_rare replaced the old merge_rare behavior, so placeholders are now
  # valid output values for labels below the rarity threshold.
  # Report the offending values rather than a bare FALSE.
  from_source <- is.na(syn$txt) |
    syn$txt %in% c("hello", "world", "foo", "bar") |
    grepl("^Other category [0-9]+$", syn$txt)
  expect_equal(
    unique(syn$txt[!from_source]), character(),
    info = paste(
      "values that are neither a source level, NA, nor a rare-category",
      "replacement label"
    )
  )
})

test_that("synthesize_data() default n equals nrow(original)", {
  df <- data.frame(x = 1:5)
  spec <- synth_spec(purpose = "demo")  # n = NULL
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), 5)
})

test_that("synthesize_data() free text dropped when strategy is drop", {
  long_strings <- sprintf("very_long_text_for_free_text_test_%040d", 1:20)
  df <- data.frame(notes = long_strings, x = 1:20)
  spec <- synth_spec(purpose = "demo", n = 10,
                     free_text_strategy = "drop")
  syn <- synthesize_data(df, spec)
  expect_equal(which(!is.na(syn$notes)), integer(),
               info = "Non-missing row indices in redacted notes")
  expect_false(all(is.na(syn$x)), info = "Non-redacted syn$x was entirely missing")
})

test_that("free_text_strategy defaults to categorical, not drop", {
  spec <- synth_spec(purpose = "demo")
  expect_equal(spec$free_text_strategy, "categorical")
})

test_that("synthesize_data() free text is synthesized as categorical by default", {
  set.seed(1)
  # Almost every note is unique; a handful repeat often enough to clear
  # rare_level_min_n (default 5) and should survive rare-level collapsing.
  common_note <- "Routine follow-up, no concerns to report at this visit."
  unique_notes <- sprintf(
    "Patient reports symptom variant number %d during today's visit.", 1:100
  )
  notes <- c(rep(common_note, 10), unique_notes)
  df <- data.frame(notes = notes, x = seq_along(notes))
  spec <- synth_spec(purpose = "demo", n = 200)

  syn <- synthesize_data(df, spec)

  expect_type(syn$notes, "character")
  expect_false(all(is.na(syn$notes)), info = "Synthesized notes were entirely missing")
  # Every near-unique note is masked by the new mask_rare behavior; none
  # reappear verbatim.
  expect_equal(intersect(unique_notes, syn$notes), character(),
               info = "Original unique notes found in synthetic output")
  # The note repeated often enough (>= rare_level_min_n) is allowed to recur.
  expect_true(common_note %in% syn$notes)
  # Report the offending values rather than a bare FALSE.
  from_source <- is.na(syn$notes) |
    syn$notes == common_note |
    grepl("^Other category [0-9]+$", syn$notes)
  expect_equal(
    unique(syn$notes[!from_source]), character(),
    info = paste(
      "values that are neither a source level, NA, nor a rare-category",
      "replacement label"
    )
  )
})

test_that("synthesize_data() marginal mixed types all work", {
  df <- data.frame(
    n = rnorm(10),
    c = letters[1:10],
    f = factor(rep(c("x", "y"), 5)),
    l = rep(c(TRUE, FALSE), 5),
    d = as.Date("2024-01-01") + 0:9,
    stringsAsFactors = FALSE
  )
  spec <- synth_spec(purpose = "demo", n = 30)
  expect_no_error(syn <- synthesize_data(df, spec))
  expect_equal(nrow(syn), 30)
  expect_equal(ncol(syn), 5)
})

test_that("simulation treatment passes through and drops columns", {
  df <- data.frame(
    group = rep(c("A", "B", "C"), length.out = 30),
    x = 1:30,
    omit = letters[seq_len(30)],
    stringsAsFactors = FALSE
  )
  roles <- dataganger:::dg_sync_roles_axes(detect_roles(df))
  roles$simulation[roles$variable == "group"] <- "pass_through"
  roles$simulation[roles$variable == "omit"] <- "drop"
  roles$identifies[roles$variable == "patient_id"] <- "direct"
  roles$identifies[roles$variable %in% c("age_band", "region")] <- "combination"
  spec <- synth_spec(purpose = "demo")

  syn <- synthesize_data(df, spec, roles = roles)

  expect_identical(syn$group, df$group)
  expect_false("omit" %in% names(syn))
  expect_true("x" %in% names(syn))
})

test_that("pass-through treatment falls back gracefully when row count differs", {
  df <- data.frame(grp = rep(c("a", "b", "c"), 10L), x = 1:30)
  roles <- detect_roles(df)
  roles$simulation[roles$variable == "grp"] <- "pass_through"
  spec <- synth_spec(purpose = "demo", n = 10)

  # Should warn but NOT abort; synthesis completes at the requested row count.
  expect_warning(
    syn <- synthesize_data(df, spec, roles = roles),
    "Pass-through columns"
  )
  expect_equal(nrow(syn), 10L)
  expect_true("x" %in% names(syn))
})

test_that("simulation treatment scrambles an alphanumeric ID column", {
  set.seed(1)
  df <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, sample(1:99, 30, TRUE)),
    x = 1:30,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$simulation[roles$variable == "order_id"], "scramble")
  spec <- synth_spec(purpose = "demo", engine = "internal")

  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_true("order_id" %in% names(syn))
  expect_equal(intersect(syn$order_id, df$order_id), character(),
               info = "Original order IDs found in synthetic output")
  expect_match(syn$order_id, "^..-....-..$")
})

test_that("seeded scramble is deterministic regardless of ambient RNG state", {
  df <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, 10:39),
    x = 1:30,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$simulation[roles$variable == "order_id"], "scramble")
  spec <- synth_spec(purpose = "demo", engine = "internal", seed = 42)

  syn1 <- synthesize_data(df, spec, roles = roles, engine = "internal")
  stats::runif(128)
  syn2 <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_identical(syn1$order_id, syn2$order_id)
})

test_that("scramble treatment falls back gracefully when row count differs", {
  set.seed(1)
  df <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, sample(1:99, 30, TRUE)),
    x = 1:30,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 10, engine = "internal")

  expect_warning(
    syn <- synthesize_data(df, spec, roles = roles, engine = "internal"),
    "Scrambled columns"
  )
  expect_equal(nrow(syn), 10L)
  expect_true("x" %in% names(syn))
})

test_that("name_strategy maps only output columns after drop treatment", {
  df <- data.frame(keep = 1:10, omit = 11:20)
  roles <- detect_roles(df)
  roles$simulation[roles$variable == "omit"] <- "drop"
  spec <- synth_spec(purpose = "demo", name_strategy = "generic")

  syn <- synthesize_data(df, spec, roles = roles)
  nm <- attr(syn, "spec")$name_map

  expect_named(syn, "col_1")
  expect_equal(nm, c(keep = "col_1"))
  expect_false("omit" %in% names(nm))
})


test_that("synthesize_data() handles roles missing one column", {
  df <- data.frame(id = 1:20, x = rep(1:5, 4), y = rep(letters[1:2], 10), stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  roles <- roles[roles$variable != "y", , drop = FALSE]
  spec <- synth_spec(purpose = "demo", n = 10)
  # id is an alphanumeric ID (default simulation "scramble"); the row-count
  # change (20 -> 10) makes scramble fall back to plain synthesis with a
  # warning, which is expected and irrelevant to this test.
  expect_no_error(syn <- suppressWarnings(synthesize_data(df, spec, roles = roles)))
  expect_s3_class(syn, "dataganger_synthetic")
})
# ---- Phase 2.1 fix tests ----

# Fix 1 - remove_ids
test_that("remove_ids masks ID columns with NA", {
  # Use x with low cardinality so it's not also flagged as ID candidate
  df <- data.frame(
    id = 1:50,
    x  = rep(1:5, 10)
  )
  roles <- detect_roles(df)
  roles$disclosure_role[roles$variable == "x"] <- "none"
  spec <- synth_spec(purpose = "demo", n = 10)
  spec$remove_ids <- TRUE
  # "id" is an alphanumeric ID with a default "scramble" simulation, which
  # exempts it from enforce_kanon's drop -- but remove_ids already masked its
  # values to NA in synthesize_marginal, so it stays present and fully NA
  # rather than being dropped as a column. The row-count change (50 -> 10)
  # also makes scramble fall back to plain synthesis with a warning, which
  # is expected and irrelevant here since the column is already NA-masked.
  syn <- suppressWarnings(synthesize_data(df, spec, roles = roles))
  expect_equal(which(!is.na(syn$id)), integer(), info = "Non-missing row indices in masked ID")
  expect_false(all(is.na(syn$x)), info = "Non-ID syn$x was entirely missing")
})

# Fix 2 - haven::labelled() in schema synthesis
test_that("schema synthesis handles haven_labelled column without error", {
  df <- tibble::tibble(
    status = haven::labelled(c(1, 2, 1), labels = c(A = 1, B = 2)),
    x      = 1:3
  )
  spec <- synth_spec(purpose = "demo", level = "schema", name_strategy = "preserve")
  syn <- synthesize_data(df, spec)
  expect_equal(nrow(syn), nrow(df))
  expect_equal(ncol(syn), 2)
  expect_type(syn$status, "character")
})

# Fix 3 - factor levels preserved for rare levels
test_that("categorical synthesis preserves rare level slots", {
  df <- data.frame(
    f = factor(c(rep("common", 199), "rare"))
  )
  spec <- synth_spec(purpose = "demo", n = 10)
  syn <- synthesize_data(df, spec)
  # mask_rare replaced the old merge_rare behavior: preserve the level slot
  # count while withholding the literal rare label.
  syn_values <- sort(unique(syn$f[!is.na(syn$f)]))
  source_values <- sort(unique(df$f[!is.na(df$f)]))
  expect_equal(length(syn_values), length(source_values))
  expect_match(paste(syn_values, collapse = "\n"),
               "(^|\n)Other category [0-9]+($|\n)")
  expect_false("rare" %in% syn_values)
})

# Fix 4 - name_map stored inside spec attribute
test_that("name_strategy dictionary_only stores name_map in spec attr", {
  df <- data.frame(patient_name = 1:5, age_years = 21:25)
  spec <- synth_spec(purpose = "demo", n = 5,
                     name_strategy = "dictionary_only")
  syn <- synthesize_data(df, spec)
  nm <- attr(syn, "spec")$name_map
  expect_true(!is.null(nm))
  expect_type(nm, "character")
  expect_equal(nm[["patient_name"]], "col_1")
  expect_equal(nm[["age_years"]], "col_2")
})

# F2 - ".other" sentinel does not collide with real "other"
test_that("rare-merge uses .other sentinel not other", {
  df <- data.frame(
    f = factor(c(rep("other", 100), rep("x", 3), rep("y", 2)))
  )
  spec <- synth_spec(purpose = "demo", n = 50, merge_rare = TRUE,
                     rare_level_min_n = 5)
  syn <- synthesize_data(df, spec)
  # "other" was common so should survive; "x" and "y" merge to ".other"
  expect_true("other" %in% sort(unique(syn$f)))
})

# Safer_external end-to-end pipeline test
test_that("demo schema pipeline completes on example_health_survey", {
  skip_if_not_installed("dataganger")
  data("example_health_survey", package = "dataganger")
  spec <- synth_spec(purpose = "demo", level = "schema")
  syn <- synthesize_data(example_health_survey, spec)
  expect_equal(nrow(syn), nrow(example_health_survey))
  expect_equal(ncol(syn), ncol(example_health_survey))
})


test_that("synthesize_data() generic naming still drops direct identifiers before renaming", {
  df <- data.frame(
    patient_id = sprintf("id%02d", 1:20),
    age_band = rep(c("20s", "30s", "40s", "50s"), each = 5),
    region = rep(c("north", "south"), each = 10),
    stringsAsFactors = FALSE
  )
  roles <- dataganger:::dg_sync_roles_axes(detect_roles(df))
  roles$disclosure_role[roles$variable == "patient_id"] <- "direct"
  roles$disclosure_role[roles$variable %in% c("age_band", "region")] <- "quasi"
  roles$identifies[roles$variable == "patient_id"] <- "direct"
  roles$identifies[roles$variable %in% c("age_band", "region")] <- "combination"
  # patient_id defaults to simulation = "scramble" as an alphanumeric ID,
  # which is an explicit keep-decision exempting it from the drop below.
  # Force an explicit "drop" decision to test that identifiers are still
  # dropped before renaming when that decision is made.
  roles$simulation[roles$variable == "patient_id"] <- "drop"
  spec <- synth_spec(purpose = "demo", n = 20, seed = 11, name_strategy = "generic")

  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")
  nm <- attr(syn, "spec")$name_map
  kanon <- attr(syn, "kanon")

  expect_false("patient_id" %in% names(nm))
  expect_named(syn, c("col_1", "col_2"))
  expect_equal(unname(nm[c("age_band", "region")]), c("col_1", "col_2"))
  expect_true(length(kanon$qi_cols) > 0L)
  expect_no_match(names(syn), "patient", ignore.case = TRUE)
})

# ---- Character-stored date/time synthesis ----

test_that("date format detection requires 90 percent round-trip fidelity", {
  valid <- rep("01/01/2020", 9)
  non_round_trip <- "1/1/2020"

  accepted <- detect_date_format(c(valid, non_round_trip))
  rejected <- detect_date_format(c(valid[seq_len(8)], non_round_trip, "1/2/2020"))

  expect_equal(
    accepted$format, "%m/%d/%Y",
    info = paste("Expected 9/10 round trips to be accepted; input:",
                 paste(c(valid, non_round_trip), collapse = " | "))
  )
  expect_null(
    rejected,
    info = paste("Expected 8/10 round trips to be rejected; input:",
                 paste(c(valid[seq_len(8)], non_round_trip, "1/2/2020"), collapse = " | "))
  )

  invalid_tail <- c(rep("01/01/2020", 200), rep("1/1/2020", 30))
  expect_null(
    detect_date_format(invalid_tail),
    info = "Full-column validation accepted a format with less than 90 percent fidelity"
  )
})

test_that("unknown period suffixes fail closed in detection and role assignment", {
  values <- rep(c("01/01/2020 01:15 FOO", "01/02/2020 11:45 BAR"), 5)
  roles <- detect_roles(data.frame(visit = values, stringsAsFactors = FALSE))

  expect_null(
    detect_date_format(values),
    info = paste("Unknown suffixes were accepted:", paste(unique(values), collapse = " | "))
  )
  expect_false(
    identical(roles$recommended_role[[1]], "date"),
    info = paste("Unknown suffixes were assigned the date role:",
                 paste(unique(values), collapse = " | "))
  )
})

test_that("unsupported translated period markers fail closed in the C locale", {
  withr::local_locale(c(LC_TIME = "C"))
  values <- rep(c(
    "01/01/2020 01:15 \u5348\u524d",
    "01/01/2020 01:15 \u5348\u5f8c"
  ), 5)
  df <- data.frame(visit = values, stringsAsFactors = FALSE)
  roles <- detect_roles(df)

  expect_null(
    detect_date_format(values),
    info = paste("Unsupported translated markers were accepted:",
                 paste(unique(values), collapse = " | "))
  )
  expect_false(
    identical(roles$recommended_role[[1L]], "date"),
    info = paste("Unsupported translated markers received the date role:",
                 paste(unique(values), collapse = " | "))
  )

  spec <- synth_spec(
    purpose = "demo", n = 10, seed = 905L,
    coarsen_dates = FALSE, engine = "internal"
  )
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")
  expect_equal(
    sort(unique(syn$visit)), sort(unique(values)),
    info = paste("Translated markers were stripped or reinterpreted:",
                 paste(unique(syn$visit), collapse = " | "))
  )
})

test_that("an invalid character date role cannot fall through to categorical synthesis", {
  values <- rep(c("01/01/2020 01:15 FOO", "01/02/2020 11:45 BAR"), 5)
  df <- data.frame(visit = values, stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  roles$recommended_role[[1L]] <- "date"
  spec <- synth_spec(
    purpose = "demo", n = 10, seed = 904L,
    coarsen_dates = FALSE, engine = "internal"
  )

  expect_error(
    synthesize_marginal(df, spec, roles = roles),
    "date role but no validated date/time format",
    info = paste("Invalid date-role values:", paste(unique(values), collapse = " | "))
  )
})

test_that("English month-name forms synthesize portably and deterministically", {
  forms <- list(
    abbreviated = c("Jan 8, 2019", "Feb 9, 2019", "Mar 10, 2019",
                    "Apr 11, 2019", "May 12, 2019"),
    full = c("January 8, 2019", "February 9, 2019", "March 10, 2019",
             "April 11, 2019", "May 12, 2019"),
    dotted = c("Jan. 8, 2019", "Feb. 9, 2019", "Mar. 10, 2019",
               "Apr. 11, 2019", "May. 12, 2019"),
    lowercase = c("january 8, 2019", "february 9, 2019", "march 10, 2019",
                  "april 11, 2019", "may 12, 2019")
  )

  for (form_name in names(forms)) {
    values <- rep(forms[[form_name]], 20)
    df <- data.frame(visit = values, stringsAsFactors = FALSE)
    roles <- detect_roles(df)
    role <- roles$recommended_role[roles$variable == "visit"]
    date_info <- parse_date_like_character(values)

    expect_equal(role, "date", info = paste("month form:", form_name))
    expect_false(is.null(date_info), info = paste("month form:", form_name))

    roles$identifies[roles$variable == "visit"] <- "none"
    roles <- dg_sync_roles_axes(roles)
    spec <- synth_spec(
      purpose = "demo", n = 100, seed = 902L,
      coarsen_dates = FALSE, engine = "internal"
    )
    first <- synthesize_data(df, spec, roles = roles, engine = "internal")
    second <- synthesize_data(df, spec, roles = roles, engine = "internal")
    reparsed <- parse_date_like_character(first$visit)

    expect_identical(first$visit, second$visit, info = paste("month form:", form_name))
    expect_false(is.null(reparsed), info = paste(
      "Synthesized month text did not parse for", form_name, ":",
      paste(utils::head(first$visit, 10), collapse = " | ")
    ))
    expect_match(first$visit, "^[A-Za-z]+[.]? [0-9]{1,2}, 2019$",
                 info = paste("month form:", form_name))
  }
})

test_that("unsupported accented month text fails closed in the C locale", {
  withr::local_locale(c(LC_TIME = "C"))
  values <- rep(c("M\u00e4rz 8, 2019", "M\u00e4rz 9, 2019", "M\u00e4rz 10, 2019",
                  "M\u00e4rz 11, 2019", "M\u00e4rz 12, 2019"), 20)
  df <- data.frame(visit = values, stringsAsFactors = FALSE)
  roles <- detect_roles(df)

  expect_null(
    parse_date_like_character(values),
    info = paste("Unsupported accented month text parsed under",
                 Sys.getlocale("LC_TIME"), ":", paste(unique(values), collapse = " | "))
  )
  expect_false(
    identical(roles$recommended_role[[1]], "date"),
    info = paste("Unsupported accented month text received date role under",
                 Sys.getlocale("LC_TIME"), ":", paste(unique(values), collapse = " | "))
  )

  spec <- synth_spec(
    purpose = "demo", n = 100, seed = 903L,
    coarsen_dates = FALSE, engine = "internal"
  )
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")
  expect_equal(
    sort(unique(syn$visit)), sort(unique(values)),
    info = "Fail-closed accented month text should remain on the non-date path"
  )
})

test_that("native non-ASCII period markers require a successful round trip", {
  am <- format(as.POSIXct("2020-01-01 01:15:00", tz = "UTC"), "%p", tz = "UTC")
  pm <- format(as.POSIXct("2020-01-01 13:15:00", tz = "UTC"), "%p", tz = "UTC")
  native_markers <- c(am, pm)
  skip_if(any(!nzchar(native_markers)) || all(grepl("^[AaPp][Mm]$", native_markers)),
          "Active LC_TIME has no non-ASCII native period markers")

  values <- c(paste("01/01/2020 01:15", am), paste("01/01/2020 01:15", pm))
  detected <- detect_date_format(values)
  expect_false(
    is.null(detected),
    info = paste("Native period values did not round trip under",
                 Sys.getlocale("LC_TIME"), ":", paste(values, collapse = " | "))
  )

  unknown <- sub(paste0(" ", pm, "$"), " UNKNOWN", values, fixed = FALSE)
  expect_null(
    detect_date_format(unknown),
    info = paste("Non-round-tripping period text was accepted:",
                 paste(unknown, collapse = " | "))
  )
})

test_that("character-stored ISO date strings are synthesized as dates, not resampled verbatim", {
  set.seed(1)
  df <- data.frame(
    event_date = format(as.Date("2020-01-01") + sample(0:364, 100, TRUE), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$recommended_role[roles$variable == "event_date"], "date")
  # date-role columns default to being a quasi-identifier (like a native
  # Date column would), which would otherwise coarsen this to month-level
  # via enforce_kanon regardless of coarsen_dates -- opt out so this test
  # isolates marginal-stage format-preserving synthesis.
  roles$identifies[roles$variable == "event_date"] <- "none"
  roles <- dg_sync_roles_axes(roles)

  spec <- synth_spec(purpose = "demo", n = 100, seed = 2, coarsen_dates = FALSE)
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  # Format is preserved (still "YYYY-MM-DD" strings, not a Date object or a
  # different pattern).
  expect_type(syn$event_date, "character")
  expect_match(syn$event_date, "^\\d{4}-\\d{2}-\\d{2}$")
  # Values fall within the observed range rather than being copied verbatim.
  parsed <- as.Date(syn$event_date)
  outside <- parsed[parsed < as.Date("2020-01-01") | parsed > as.Date("2020-12-30")]
  expect_equal(outside, as.Date(character()), info = "Synthetic dates outside source range")
  # Not simply the original column reshuffled.
  expect_false(identical(sort(syn$event_date), sort(df$event_date)))
})

test_that("character-stored date+time strings preserve both the date range and the time-of-day format", {
  set.seed(3)
  df <- data.frame(
    visit = sprintf(
      "%s %02d:%02d %s",
      format(as.Date("2020-01-01") + sample(0:29, 100, TRUE), "%m/%d/%Y"),
      sample(1:12, 100, TRUE), sample(0:59, 100, TRUE),
      sample(c("AM", "PM"), 100, TRUE)
    ),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$recommended_role[roles$variable == "visit"], "date")
  # Opt out of the quasi-identifier default (see comment in the ISO-date
  # test above) so this test isolates marginal-stage format preservation.
  roles$identifies[roles$variable == "visit"] <- "none"
  roles <- dg_sync_roles_axes(roles)

  spec <- synth_spec(purpose = "demo", n = 100, seed = 4, coarsen_dates = FALSE)
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  sample_values <- paste(utils::head(stats::na.omit(syn$visit), 10), collapse = ", ")
  detected <- detect_date_format(syn$visit)
  expect_equal(
    detected$format, "%m/%d/%Y %I:%M %p",
    info = paste("Detected format mismatch. Sample syn$visit values:", sample_values)
  )
  # Parse through the package's locale-independent 12-hour reader; base
  # strptime() would delegate "%p" to the C library and turn this assertion
  # into a test of the host locale.
  parsed <- dg_strptime_period(trimws(syn$visit), detected$format, tz = "UTC")
  expect_true(
    all(!is.na(parsed)),
    info = paste("Some synthesized visit values did not parse. Sample values:", sample_values)
  )
  parsed_dates <- as.Date(parsed)
  outside <- parsed_dates[
    parsed_dates < as.Date("2020-01-01") | parsed_dates > as.Date("2020-01-30")
  ]
  expect_equal(outside, as.Date(character()), info = "Synthetic datetimes outside source range")
  # The time-of-day component varies rather than collapsing to midnight
  # (which is what blanket coarsen-to-day would otherwise do).
  times <- format(parsed, "%H:%M", tz = "UTC")
  expect_gt(length(unique(times)), 1L)
})

# The C library's "%p" is locale- and OS-dependent -- on macOS under
# LC_TIME=en_GB it matches nothing when parsing and formats to an empty
# string. Rather than hunt for such a locale (most CI images install none),
# these tests pin the structural property that makes the behavior uniform:
# dataganger reads and writes the AM/PM token itself and never passes "%p"
# down to strptime() or format().
test_that("12-hour parsing and formatting never delegate %p to the platform", {
  # Violations are recorded rather than thrown: detect_date_format() wraps its
  # parse and format calls in tryCatch(), which would swallow a stop() here
  # and turn a clear failure into an obscure one.
  seen <- new.env(parent = emptyenv())
  seen$delegated <- character()
  assign("dg_note_period_delegation", function(where, fmt) {
    seen$delegated <- c(seen$delegated, paste0(where, ": ", fmt))
  }, envir = globalenv())
  on.exit(rm("dg_note_period_delegation", envir = globalenv()), add = TRUE)

  suppressMessages(trace(base::strptime, print = FALSE, tracer = quote({
    if (any(grepl("%p", format, fixed = TRUE))) {
      get("dg_note_period_delegation", envir = globalenv())("strptime", format)
    }
  })))
  on.exit(suppressMessages(untrace(base::strptime)), add = TRUE)
  suppressMessages(trace(base::format.POSIXct, print = FALSE, tracer = quote({
    if (!missing(format) && any(grepl("%p", format, fixed = TRUE))) {
      get("dg_note_period_delegation", envir = globalenv())("format", format)
    }
  })))
  on.exit(suppressMessages(untrace(base::format.POSIXct)), add = TRUE)

  df <- data.frame(
    visit = rep(c("01/01/2020 01:15 AM", "01/01/2020 01:15 PM"), 50),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$identifies[roles$variable == "visit"] <- "none"
  roles <- dg_sync_roles_axes(roles)
  spec <- synth_spec(purpose = "demo", n = 100, seed = 5, coarsen_dates = FALSE)

  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_equal(
    seen$delegated, character(),
    info = "%p was handed to the C library, whose output is locale-dependent"
  )
  expect_match(
    syn$visit, " (AM|PM)$",
    info = paste0(
      "sample syn$visit values: ",
      paste(utils::head(syn$visit, 10), collapse = ", ")
    )
  )
})

test_that("a 12-hour column is detected and parsed without a locale %p", {
  values <- c(
    "01/01/2020 12:15 AM", "01/01/2020 12:15 PM",
    "01/01/2020 01:15 PM", "01/02/2020 11:45 AM"
  )
  detected <- detect_date_format(values)
  expect_equal(detected$format, "%m/%d/%Y %I:%M %p")

  parsed <- dg_strptime_period(values, detected$format, tz = "UTC")
  expect_equal(
    format(parsed, "%Y-%m-%d %H:%M", tz = "UTC"),
    c("2020-01-01 00:15", "2020-01-01 12:15",
      "2020-01-01 13:15", "2020-01-02 11:45")
  )
  # A value with no period marker does not match a 12-hour format.
  expect_true(is.na(dg_strptime_period(
    "01/01/2020 01:15", "%m/%d/%Y %I:%M %p", tz = "UTC"
  )))
  # 24-hour columns are untouched by the AM/PM path.
  expect_equal(
    detect_date_format(c("01/01/2020 13:15", "01/02/2020 09:05"))$format,
    "%m/%d/%Y %H:%M"
  )
})

test_that("synthetic 12-hour output keeps the source column's marker case", {
  df <- data.frame(
    visit = rep(c("01/01/2020 11:15 am", "01/01/2020 09:45 pm"), 50),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$identifies[roles$variable == "visit"] <- "none"
  roles <- dg_sync_roles_axes(roles)
  spec <- synth_spec(purpose = "demo", n = 100, seed = 5, coarsen_dates = FALSE)

  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_match(
    syn$visit, " (am|pm)$",
    info = paste0(
      "sample syn$visit values: ",
      paste(utils::head(syn$visit, 10), collapse = ", ")
    )
  )
})

test_that("character-stored 12-hour times survive a contrasting LC_TIME", {
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old_locale)), add = TRUE)

  locale_candidates <- c(
    "en_DK.UTF-8", "en_DK.utf8", "en_GB.UTF-8", "en_GB.utf8"
  )
  locale_used <- NULL
  for (candidate in locale_candidates) {
    activated <- suppressWarnings(Sys.setlocale("LC_TIME", candidate))
    if (!is.na(activated) && nzchar(activated) &&
        !identical(format(as.POSIXct("2020-01-01 13:00:00", tz = "UTC"),
                          "%p", tz = "UTC"), "PM")) {
      locale_used <- activated
      break
    }
  }
  if (is.null(locale_used)) {
    skip("No installed LC_TIME locale with a non-uppercase or empty %p marker")
  }

  df <- data.frame(
    visit = rep(c("01/01/2020 01:15 AM", "01/01/2020 01:15 PM"), 50),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$identifies[roles$variable == "visit"] <- "none"
  roles <- dg_sync_roles_axes(roles)
  spec <- synth_spec(
    purpose = "demo", n = 100, seed = 5, coarsen_dates = FALSE
  )

  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")
  sample_values <- paste(utils::head(syn$visit, 10), collapse = ", ")

  expect_match(
    syn$visit, " (AM|PM)$",
    info = paste0(
      "LC_TIME=", locale_used,
      "; sample syn$visit values: ", sample_values
    )
  )
})

test_that("a bare time-of-day column (no date part) is synthesized and stays time-only", {
  set.seed(5)
  df <- data.frame(
    check_in = sprintf("%02d:%02d", sample(6:20, 100, TRUE), sample(0:59, 100, TRUE)),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_equal(roles$recommended_role[roles$variable == "check_in"], "date")
  # Opt out of the quasi-identifier default (see comment in the ISO-date
  # test above) so this test isolates marginal-stage format preservation.
  roles$identifies[roles$variable == "check_in"] <- "none"
  roles <- dg_sync_roles_axes(roles)

  spec <- synth_spec(purpose = "demo", n = 100, seed = 6, coarsen_dates = FALSE)
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_match(syn$check_in, "^\\d{2}:\\d{2}$")
  # No date leaked into the output.
  expect_no_match(syn$check_in, "[-/]")
  hours <- as.integer(substr(syn$check_in, 1, 2))
  expect_equal(hours[hours < 6 | hours > 20], integer(),
               info = paste("Synthetic hours:", paste(hours, collapse = ", ")))
})

test_that("character-stored dates preserve the original NA rate", {
  set.seed(7)
  x <- format(as.Date("2020-01-01") + sample(0:364, 200, TRUE), "%m/%d/%Y")
  x[sample(seq_along(x), 40)] <- NA
  df <- data.frame(sched = x, stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  # Opt out of the quasi-identifier default (see comment in the ISO-date
  # test above) so this test isolates marginal-stage format preservation.
  roles$identifies[roles$variable == "sched"] <- "none"
  roles <- dg_sync_roles_axes(roles)

  spec <- synth_spec(purpose = "demo", n = 200, seed = 8, coarsen_dates = FALSE)
  syn <- synthesize_data(df, spec, roles = roles, engine = "internal")

  expect_in(TRUE, is.na(syn$sched))
  expect_match(stats::na.omit(syn$sched), "^\\d{2}/\\d{2}/\\d{4}$")
})

test_that("synthesize_data generates postal codes for detected postal columns", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9",
                    "R3C 4A5", "S4P 3B2", "N2L 3G1", "P1A 1B2", "L1A 2B3"),
    x = 1:10,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 20, seed = 42, engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles)
  expect_equal(nrow(syn), 20)
  expect_true("postal_code" %in% names(syn))
  reg <- dataganger:::dg_postal_format_registry()
  expect_match(syn$postal_code[!is.na(syn$postal_code)], reg$CA$regex)
})

test_that("synthesize_data resamples postal codes when strategy is resample", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9",
                    "R3C 4A5", "S4P 3B2", "N2L 3G1", "P1A 1B2", "L1A 2B3"),
    x = 1:10,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$postal_strategy[roles$variable == "postal_code"] <- "resample"
  spec <- synth_spec(purpose = "demo", n = 20, seed = 42, engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles)
  observed <- unique(df$postal_code)
  unexpected <- setdiff(syn$postal_code[!is.na(syn$postal_code)], observed)
  expect_equal(unexpected, character(), info = "Unexpected resampled postal codes")
})

test_that("synthesize_data postal code seeded determinism", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9",
                    "R3C 4A5", "S4P 3B2", "N2L 3G1", "P1A 1B2", "L1A 2B3"),
    x = 1:10,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  spec <- synth_spec(purpose = "demo", n = 15, seed = 99, engine = "internal")
  syn1 <- synthesize_data(df, spec, roles = roles)
  syn2 <- synthesize_data(df, spec, roles = roles)
  expect_identical(syn1$postal_code, syn2$postal_code)
})

test_that("synthesize_data per-column postal strategy", {
  df <- data.frame(
    ca_postal = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9",
                  "R3C 4A5", "S4P 3B2", "N2L 3G1", "P1A 1B2", "L1A 2B3"),
    us_zip = c("10001", "90210", "60601", "30301", "85001",
               "20001", "33101", "48201", "55401", "63101"),
    x = 1:10,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$postal_strategy[roles$variable == "ca_postal"] <- "generate"
  roles$postal_strategy[roles$variable == "us_zip"] <- "resample"
  roles$postal_country[roles$variable == "ca_postal"] <- "CA"
  roles$postal_country[roles$variable == "us_zip"] <- "US"
  spec <- synth_spec(purpose = "demo", n = 20, seed = 42, engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles)
  reg <- dataganger:::dg_postal_format_registry()
  expect_match(syn$ca_postal[!is.na(syn$ca_postal)], reg$CA$regex)
  unexpected <- setdiff(syn$us_zip[!is.na(syn$us_zip)], unique(df$us_zip))
  expect_equal(unexpected, character(), info = "Unexpected resampled US ZIP codes")
})

test_that("synthesize_data postal falls through to character when format unknown", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9",
                    "R3C 4A5", "S4P 3B2", "N2L 3G1", "P1A 1B2", "L1A 2B3"),
    x = 1:10,
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$postal_country[roles$variable == "postal_code"] <- "ZZ"
  spec <- synth_spec(purpose = "demo", n = 20, seed = 42, engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles)
  expect_equal(nrow(syn), 20)
  expect_true("postal_code" %in% names(syn))
})
})
