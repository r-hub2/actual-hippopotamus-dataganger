test_that("export_synthetic() requires explicit path", {
  syn <- tibble::tibble(x = 1:3)
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 1)
  class(syn) <- c("dataganger_synthetic", class(syn))

  expect_error(
    export_synthetic(syn),
    "path"
  )
})

test_that("export_synthetic() writes the full bundle file set", {
  tmp <- withr::local_tempdir()
  data("example_health_survey", package = "dataganger")

  roles <- detect_roles(example_health_survey)
  spec <- synth_spec(purpose = "development", seed = 1, n = 40)
  syn <- synthesize_data(example_health_survey, spec, roles = roles)
  cmp <- compare_synthetic(example_health_survey, syn, roles = roles)
  prv <- privacy_check(example_health_survey, syn, roles = roles, stage = "post", spec = spec)

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(
    syn,
    original = example_health_survey,
    comparison = cmp,
    privacy = prv,
    path = out_dir,
    format = "dir"
  )

  expect_setequal(
    list.files(out_dir, recursive = TRUE),
    c(
      "synthetic_data.csv",
      "human/human.md",
      "human/comparison_report.html",
      "agent/recipe.yaml",
      "agent/AGENT.md",
      "agent/manifest.json"
    )
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_equal(manifest$seed, 1)
  expect_true(nzchar(manifest$spec_hash))
  expect_false(file.exists(file.path(out_dir, "agent", "code_readiness_report.json")))

  recipe <- yaml::read_yaml(file.path(out_dir, "agent", "recipe.yaml"))
  expect_equal(recipe$purpose, "development")
  expect_true(is.list(recipe$roles))
  expect_true(length(recipe$roles) > 0L)

  expect_setequal(
    names(manifest$file_sha256),
    c(
      "synthetic_data.csv",
      "human/human.md",
      "human/comparison_report.html",
      "agent/recipe.yaml",
      "agent/AGENT.md"
    )
  )
})

test_that("export_synthetic() writes code readiness JSON when supplied", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = 1:8,
    grp = factor(rep(c("a", "b"), each = 4)),
    score = c(10, 11, 12, 13, 14, 15, 16, 17)
  )
  spec <- synth_spec(purpose = "development", seed = 11)
  roles <- detect_roles(original)
  syn <- synthesize_data(original, spec, roles = roles)
  code_readiness <- check_code_readiness(original, syn, roles = roles)

  out_dir <- file.path(tmp, "bundle-with-readiness")
  export_synthetic(
    syn,
    original = original,
    roles = roles,
    code_readiness = code_readiness,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  readiness_path <- file.path(out_dir, "agent", "code_readiness_report.json")
  expect_true(file.exists(readiness_path))

  readiness <- jsonlite::read_json(readiness_path)
  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_true(is.logical(readiness$summary$ready))
  expect_true("agent/code_readiness_report.json" %in% names(manifest$file_sha256))
  expect_match(human_md, "agent/code_readiness_report.json", fixed = TRUE)
})

test_that("export_synthetic() folds guidance and privacy text into human/human.md", {
  tmp <- withr::local_tempdir()
  data("example_health_survey", package = "dataganger")

  roles <- detect_roles(example_health_survey)
  spec <- synth_spec(purpose = "development", seed = 1, n = 40)
  syn <- synthesize_data(example_health_survey, spec, roles = roles)
  prv <- privacy_check(example_health_survey, syn, roles = roles, stage = "post", spec = spec)

  out_dir <- file.path(tmp, "compact-dir")
  export_synthetic(
    syn,
    original = example_health_survey,
    privacy = prv,
    path = out_dir,
    format = "dir",
    include_dictionary = FALSE,
    compact = TRUE
  )

  listing <- list.files(out_dir, recursive = TRUE)
  expect_false("ai-readme.md" %in% listing)
  expect_false("privacy_report.txt" %in% listing)
  expect_false("README.md" %in% listing)
  expect_true("human/human.md" %in% listing)

  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")
  expect_match(human_md, "## Privacy")
  expect_match(human_md, "## For AI assistants")
  expect_match(human_md, "Exact row matches")
})

test_that("export_synthetic() records infeasible k-anon in human markdown and manifest", {
  tmp <- withr::local_tempdir()
  df <- data.frame(
    qi_a = sprintf("a%03d", 1:100),
    qi_b = sprintf("b%03d", 1:100),
    value = seq_len(100),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  roles$identifies <- "combination"
  roles$sensitive <- FALSE
  roles$disclosure_role <- "quasi"
  roles$simulation <- c("pass_through", "pass_through", "synthesize")
  # Pin the internal engine: this tests k-anon reporting, not synthpop, and
  # the 3-column fixture leaves synthpop too few columns after exclusions.
  spec <- synth_spec(purpose = "development", seed = 9, n = 100, k_anon = 5,
                     engine = "internal")
  result <- run_synthesis_pipeline(df, spec, roles = roles)
  out_dir <- file.path(tmp, "kanon-dir")

  export_synthetic(
    result$synthetic,
    original = df,
    roles = roles,
    comparison = result$comparison,
    privacy = result$privacy,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")
  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = FALSE)

  expect_match(human_md, "k-anonymity: not applied; k=5", fixed = TRUE)
  expect_match(human_md, "acknowledged=no", fixed = TRUE)
  expect_true(isTRUE(manifest$kanon$infeasible))
  expect_false(isTRUE(manifest$kanon$applied))
  expect_false(isTRUE(manifest$kanon$acknowledged))
  expect_length(manifest$blockers, 1L)
  expect_identical(manifest$blockers[[1]]$type, "kanon_not_applied")
})

test_that("export_synthetic() clears the k-anon blocker after explicit acknowledgement", {
  tmp <- withr::local_tempdir()
  syn <- tibble::tibble(age = 1:3, sex = c("F", "M", "F"))
  attr(syn, "spec") <- synth_spec(purpose = "development", seed = 10, engine = "internal")
  attr(syn, "kanon") <- list(
    qi_cols = c("age", "sex"),
    k = 5L,
    smallest_cell = 1L,
    suppressed_cells = 0L,
    infeasible = TRUE
  )
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "ack-bundle")
  export_synthetic(
    syn,
    path = out_dir,
    format = "dir",
    include_report = FALSE,
    kanon_acknowledged = TRUE
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = FALSE)
  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_true(isTRUE(manifest$kanon$acknowledged))
  expect_length(manifest$blockers, 0L)
  expect_match(human_md, "acknowledged=yes", fixed = TRUE)
})

test_that("export_synthetic() records k provenance after the user accepts a suggested k", {
  tmp <- withr::local_tempdir()
  syn <- tibble::tibble(age = c(30, 40, 50), sex = c("F", "M", "F"))
  attr(syn, "spec") <- synth_spec(purpose = "development", seed = 11, engine = "internal")
  attr(syn, "kanon") <- list(
    qi_cols = c("age", "sex"),
    k = 3L,
    smallest_cell = 3L,
    suppressed_cells = 29L,
    infeasible = FALSE,
    k_default = 5L,
    k_provenance = "user_selected_after_infeasible"
  )
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "provenance-bundle")
  export_synthetic(
    syn,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = FALSE)
  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_identical(manifest$kanon$k_default, 5L)
  expect_identical(manifest$kanon$k_provenance, "user_selected_after_infeasible")
  expect_match(
    human_md,
    "k-anonymity: k = 3, user-selected after k = 5 was infeasible",
    fixed = TRUE
  )
})

test_that("export_synthetic() sanitizes spreadsheet-dangerous cells", {
  tmp <- withr::local_tempdir()

  syn <- tibble::tibble(
    text = c("=sum(A1:A2)", "  +oops", "-bad", "@cmd", "safe"),
    grp = factor(c("a", "b", "c", "d", "e"))
  )
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 2)
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(syn, path = out_dir, format = "dir")

  exported <- readr::read_csv(file.path(out_dir, "synthetic_data.csv"), show_col_types = FALSE)
  expect_equal(
    exported$text,
    c("'=sum(A1:A2)", "'  +oops", "'-bad", "'@cmd", "safe")
  )
})

test_that("export_synthetic() warns but succeeds on exact-row matches by default", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = sprintf("id-%02d", 1:20),
    grp = rep(letters[1:4], each = 5)
  )
  syn <- original
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 3)
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "warn-dir")
  expect_warning(
    export_synthetic(syn, original = original, path = out_dir, format = "dir"),
    "exact-row"
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_true(manifest$exact_row_matches > 0)

  human_md <- readLines(file.path(out_dir, "human", "human.md"), warn = FALSE)
  expect_in(sprintf("Exact row matches: %s", manifest$exact_row_matches), human_md)
})

test_that("export_synthetic() errors on exact-row matches when fail_on_exact_match = TRUE", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = sprintf("id-%02d", 1:20),
    grp = rep(letters[1:4], each = 5)
  )
  syn <- original
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 3)
  class(syn) <- c("dataganger_synthetic", class(syn))

  expect_error(
    export_synthetic(
      syn,
      original = original,
      path = file.path(tmp, "bad-dir"),
      format = "dir",
      fail_on_exact_match = TRUE
    ),
    "exact-row"
  )
})

test_that("export_synthetic() refuses to overwrite existing output without flag", {
  tmp <- withr::local_tempdir()

  syn <- tibble::tibble(x = 1:3)
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 4)
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "bundle-dir")
  dir.create(out_dir)

  expect_error(
    export_synthetic(syn, path = out_dir, format = "dir"),
    "already exists"
  )
})

test_that("export_synthetic() writes zip output", {
  tmp <- withr::local_tempdir()
  data("example_health_survey", package = "dataganger")

  roles <- detect_roles(example_health_survey)
  spec <- synth_spec(purpose = "development", seed = 5, n = 30)
  syn <- synthesize_data(example_health_survey, spec, roles = roles)

  zip_path <- file.path(tmp, "bundle.zip")
  export_synthetic(
    syn,
    original = example_health_survey,
    path = zip_path,
    format = "zip"
  )

  expect_true(file.exists(zip_path))
  zip_listing <- utils::unzip(zip_path, list = TRUE)
  expect_setequal(
    zip_listing$Name,
    c(
      "synthetic_data.csv",
      "human/human.md",
      "human/comparison_report.html",
      "agent/recipe.yaml",
      "agent/AGENT.md",
      "agent/manifest.json"
    )
  )
})

test_that("export_synthetic() manifest records synthpop engine and citation", {
  tmp <- withr::local_tempdir()
  syn <- tibble::tibble(x = 1:3)
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 8)
  attr(syn, "engine") <- "synthpop"
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "synthpop-bundle")
  export_synthetic(syn, path = out_dir, format = "dir", include_report = FALSE)

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_equal(manifest$engine, "synthpop")
  expect_match(manifest$synthesis_citation, "Nowok B, Raab GM, Dibben C")
  expect_match(manifest$synthesis_citation, "10.18637/jss.v074.i11", fixed = TRUE)
})

test_that("export_synthetic() does not list dropped variables as NA (NA) in human markdown", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = 1:12,
    keep = rep(c("a", "b", "c"), each = 4),
    note = sprintf("free text %02d", 1:12)
  )
  roles <- detect_roles(original)
  roles$simulation[roles$variable == "note"] <- "drop"
  spec <- synth_spec(purpose = "demo", seed = 9)
  syn <- synthesize_data(original, spec, roles = roles)

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(
    syn,
    original = original,
    roles = roles,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_false(grepl("NA \\(NA\\)", human_md))
  expect_match(human_md, "`note`: dropped", fixed = TRUE)
})

test_that("export_synthetic() omits original_variable when name_strategy is dictionary_only", {
  tmp <- withr::local_tempdir()
  data("example_health_survey", package = "dataganger")

  roles <- detect_roles(example_health_survey)
  spec <- synth_spec(purpose = "demo")
  spec$name_strategy <- "dictionary_only"
  syn <- synthesize_data(example_health_survey, spec, roles = roles)

  out_dir <- file.path(tmp, "dictionary-only-bundle")
  export_synthetic(
    syn,
    original = example_health_survey,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  recipe <- yaml::read_yaml(file.path(out_dir, "agent", "recipe.yaml"))
  recipe_variables <- vapply(recipe$roles, `[[`, character(1), "variable")
  expect_false("original_variable" %in% recipe_variables,
               info = paste("Recipe variables:", paste(recipe_variables, collapse = ", ")))

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_null(manifest$spec$name_map)
})

test_that("dictionary_only export withholds original names from bundled text", {
  tmp <- withr::local_tempdir()
  original <- tibble::tibble(
    patient_zip = rep(sprintf("%05d", 10001:10010), each = 3),
    salary = rep(c(50000, 65000, 72000), 10)
  )
  # Use detect_roles output as-is: patient_zip matches the ID-name pattern and
  # is dropped as a direct identifier, so it never enters the name_map. The
  # dropped-column path must also withhold the original name in recipe.yaml.
  roles <- detect_roles(original)
  spec <- synth_spec(purpose = "demo", seed = 44, name_strategy = "dictionary_only", n = nrow(original))
  syn <- synthesize_data(original, spec, roles = roles)

  out_dir <- file.path(tmp, "dictionary-private")
  export_synthetic(
    syn,
    original = original,
    roles = roles,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  text_files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  text_files <- text_files[!grepl("synthetic_data\\.csv$", text_files)]
  bundled <- paste(vapply(text_files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(grepl("patient_zip|salary", bundled))
})

test_that("export_synthetic() skips report gracefully when report deps are unavailable", {
  tmp <- withr::local_tempdir()
  syn <- tibble::tibble(x = 1:3)
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 9)
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "no-report-bundle")
  withr::local_options(dataganger.can_render_comparison_report = FALSE)
  expect_message(
    export_synthetic(syn, path = out_dir, format = "dir"),
    "skipping comparison report"
  )

  expect_false(file.exists(file.path(out_dir, "human", "comparison_report.html")))
  expect_true(file.exists(file.path(out_dir, "agent", "manifest.json")))
})

test_that("build_reproduction_script() emits a runnable R-only pipeline", {
  original <- tibble::tibble(
    id = sprintf("id-%02d", 1:5),
    city = c("A", "B", "A", "C", "B"),
    score = c(1, 2, 3, NA, 5)
  )
  roles <- detect_roles(original)
  roles$user_role[roles$variable == "city"] <- "categorical"
  roles$disclosure_role[roles$variable == "id"] <- "direct"
  roles$simulation[roles$variable == "id"] <- "drop"
  roles$simulation[roles$variable == "city"] <- "pass_through"

  spec <- synth_spec(
    purpose = "development",
    seed = 42L,
    level = "marginal",
    name_strategy = "preserve",
    k_anon = 7L,
    rare_level_min_n = 3L,
    preserve_missingness = "exact",
    coarsen_dates = FALSE,
    merge_rare = TRUE,
    free_text_strategy = "drop"
  )

  script <- build_reproduction_script(spec = spec, roles = roles, purpose = "development")

  expect_match(script, "read_input\\(")
  expect_match(script, "detect_roles\\(")
  expect_match(script, "synth_spec\\(")
  expect_match(script, "synthesize_data\\(")
  expect_match(script, "seed = 42")
  expect_match(script, "roles\\$user_role")
  expect_match(script, "roles\\$disclosure_role")
  expect_match(script, "roles\\$simulation")
  expect_no_match(script, "import ")
  expect_no_match(script, "```\\{python\\}")
})

test_that("build_reproduction_script() omits original-name keyed overrides when names are withheld", {
  original <- tibble::tibble(patient_zip = c("10001", "10002"), salary = c(10, 20))
  roles <- detect_roles(original)
  roles$user_role[roles$variable == "salary"] <- "numeric"
  spec <- synth_spec(purpose = "demo", seed = 1, name_strategy = "dictionary_only")

  script <- build_reproduction_script(spec, roles, "demo", include_original_names = FALSE)

  expect_match(script, "Column names were withheld", fixed = TRUE)
  expect_no_match(script, "roles\\$user_role")
  expect_no_match(script, "patient_zip|salary")
})

test_that("export_synthetic() writes the same reproduction pipeline into bundle files", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = sprintf("id-%02d", 1:12),
    grp = factor(rep(c("a", "b", "c"), each = 4)),
    score = c(1:11, NA)
  )
  roles <- detect_roles(original)
  roles$disclosure_role[roles$variable == "id"] <- "direct"
  roles$simulation[roles$variable == "id"] <- "drop"
  spec <- synth_spec(purpose = "development", seed = 99L, n = nrow(original))
  syn <- synthesize_data(original, spec, roles = roles)

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(
    syn,
    original = original,
    roles = roles,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  recipe <- paste(readLines(file.path(out_dir, "agent", "recipe.yaml"), warn = FALSE), collapse = "\n")
  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_match(recipe, "purpose: development")
  expect_match(recipe, "seed: 99")
  expect_match(recipe, "roles:")
  expect_match(human_md, "synthesize_data\\(")
  expect_match(human_md, "read_input\\(")
})


test_that("bundle human markdown avoids overclaiming privacy safety", {
  tmp <- withr::local_tempdir()
  syn <- tibble::tibble(x = 1:3)
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 1)
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(syn, path = out_dir, format = "dir", include_report = FALSE)

  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_false(grepl("safe to share", human_md, fixed = TRUE))
  expect_match(human_md, "reduce direct disclosure risk")
})

test_that("manifest booleans and dictionary reflect dropped and pass-through columns", {
  tmp <- withr::local_tempdir()
  original <- tibble::tibble(id = c("P1", "P2", "P3"), note = c("a", "b", "c"), score = c(1, 2, 3))
  syn <- tibble::tibble(id = c("P1", "P2", "P3"), score = c(1.1, 2.1, 3.1))
  spec <- synth_spec(purpose = "development", seed = 1)
  attr(syn, "spec") <- spec
  class(syn) <- c("dataganger_synthetic", class(syn))

  roles <- tibble::tibble(
    variable = c("id", "note", "score"),
    recommended_role = c("alphanumeric ID", "free text", "numeric"),
    user_role = c(NA_character_, NA_character_, NA_character_),
    class = c("alphanumeric ID", "free text", "numeric"),
    identifies = c("direct", "direct", "none"),
    sensitive = c(FALSE, FALSE, FALSE),
    disclosure_role = c("direct", "direct", "none"),
    simulation = c("pass_through", "drop", "synthesize"),
    reason = c("", "", ""),
    disclosure_reason = c("", "", "")
  )

  out_dir <- file.path(tmp, "bundle-dir")
  export_synthetic(
    syn,
    original = original,
    roles = roles,
    path = out_dir,
    format = "dir",
    include_report = FALSE
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  human_md <- paste(readLines(file.path(out_dir, "human", "human.md"), warn = FALSE), collapse = "\n")

  expect_true(isTRUE(manifest$raw_rows_included))
  expect_true(isTRUE(manifest$ids_included))
  expect_false(isTRUE(manifest$free_text_included))
  expect_match(human_md, "`note`: dropped")
})


test_that("export_synthetic() exact-row matches respect the privacy role-map exclusion", {
  tmp <- withr::local_tempdir()

  original <- tibble::tibble(
    id = sprintf("id%02d", 1:20),
    grp = rep(letters[1:4], each = 5),
    score = rep(1:5, times = 4)
  )
  roles <- detect_roles(original)
  syn <- tibble::tibble(
    id = sprintf("syn%02d", 1:20),
    grp = original$grp,
    score = original$score
  )
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 12)
  class(syn) <- c("dataganger_synthetic", class(syn))

  privacy <- privacy_check(original, syn, roles = roles, stage = "post", spec = attr(syn, "spec"))
  out_dir <- file.path(tmp, "role-map-dir")
  export_synthetic(syn, original = original, roles = roles, privacy = privacy, path = out_dir, format = "dir", include_report = FALSE)

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_equal(manifest$exact_row_matches, attr(privacy, "exact_row_matches", exact = TRUE))
  expect_gt(manifest$exact_row_matches, exact_row_match_count(original, syn))
})

test_that("render_kanon_line() surfaces suppressed_row_frac when whole-cell absorption suppressed extra rows", {
  modest <- list(
    qi_cols = "x", k = 5L, smallest_cell = 24L,
    suppressed_cells = 3L, suppressed_rows = 26L, suppressed_row_frac = 0.2166667,
    infeasible = FALSE
  )
  line <- dataganger:::render_kanon_line(modest)
  expect_match(line, "enforced, k=5, smallest cell=24", fixed = TRUE)
  expect_match(line, "22% of QI values suppressed", fixed = TRUE)

  clean <- list(
    qi_cols = "x", k = 5L, smallest_cell = 30L,
    suppressed_cells = 0L, suppressed_rows = 0L, suppressed_row_frac = 0,
    infeasible = FALSE
  )
  clean_line <- dataganger:::render_kanon_line(clean)
  expect_false(grepl("suppressed", clean_line, fixed = TRUE))
})

test_that("export_synthetic() manifest and human report include suppressed_rows fields", {
  tmp <- withr::local_tempdir()
  original <- tibble::tibble(
    id = sprintf("id%02d", 1:20),
    grp = rep(letters[1:4], each = 5),
    score = rep(1:5, times = 4)
  )
  syn <- original
  attr(syn, "spec") <- synth_spec(purpose = "demo", seed = 12)
  attr(syn, "kanon") <- list(
    qi_cols = "grp", k = 5L, smallest_cell = 20L,
    suppressed_cells = 1L, suppressed_rows = 5L, suppressed_row_frac = 0.25,
    infeasible = FALSE
  )
  class(syn) <- c("dataganger_synthetic", class(syn))

  out_dir <- file.path(tmp, "kanon-rows-dir")
  # syn is a verbatim copy of original for simplicity here, which trips the
  # (unrelated) exact-row-match warning; this test only cares about the
  # kanon fields, so that warning is expected and irrelevant.
  suppressWarnings(
    export_synthetic(syn, original = original, path = out_dir, format = "dir", include_report = FALSE)
  )

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_equal(manifest$kanon$suppressed_rows, 5L)
  expect_equal(manifest$kanon$suppressed_row_frac, 0.25)

  human_md <- paste(readLines(file.path(out_dir, "human", "human.md")), collapse = "\n")
  expect_match(human_md, "25% of QI values suppressed", fixed = TRUE)
})

test_that("cli_kanon_summary_line() covers not-applicable, infeasible, and suppressed cases", {
  expect_equal(dataganger:::cli_kanon_summary_line(NULL), "not applicable")
  expect_equal(
    dataganger:::cli_kanon_summary_line(list(applied = FALSE, qi_cols = character(0), k = 5L)),
    "not applicable; no quasi-identifier columns"
  )
  expect_match(
    dataganger:::cli_kanon_summary_line(list(infeasible = TRUE, k = 5L)),
    "not applied \\(k=5 infeasible",
  )
  line <- dataganger:::cli_kanon_summary_line(list(
    applied = TRUE, infeasible = FALSE, k = 5L, smallest_cell = 20L,
    suppressed_row_frac = 1
  ))
  expect_match(line, "enforced, k=5, smallest cell=20", fixed = TRUE)
  expect_match(line, "100% of QI values suppressed", fixed = TRUE)
})

test_that("dataganger inspect prints a K-anonymity summary line", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  bundle_path <- file.path(tmp, "bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123), spec_path)
  expect_identical(
    dataganger_cli(c("synthesize", data_path, "--spec", spec_path, "--out", bundle_path), quit = FALSE),
    0L
  )

  out <- capture.output(code <- dataganger_cli(c("inspect", bundle_path), quit = FALSE))
  expect_identical(code, 0L)
  expect_match(paste(out, collapse = "\n"), "(^|\n)K-anonymity:")
})
