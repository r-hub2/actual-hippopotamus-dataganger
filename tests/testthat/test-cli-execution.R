test_that("profile command writes profile JSON", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path <- file.path(tmp, "profile.json")

  result <- run_cli(c("profile", data_path, "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))

  profile <- jsonlite::read_json(out_path, simplifyVector = TRUE)
  expect_equal(profile$n_rows, 5)
  expect_equal(profile$n_cols, 4)
  expect_true("profile" %in% names(profile))
  expect_true("n_missing" %in% names(profile$profile))
  expect_in("score", profile$profile$variable)
})

test_that("roles command writes roles YAML", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path <- file.path(tmp, "roles.yaml")

  result <- run_cli(c("roles", data_path, "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))

  roles <- yaml::read_yaml(out_path)
  expect_true(is.list(roles))
  role_variables <- vapply(roles, `[[`, character(1), "variable")
  expect_in("id", role_variables)
  expect_in("score", role_variables)
})

test_that("spec command writes synth spec YAML", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "spec.yaml")

  result <- run_cli(c("spec", "--purpose", "development", "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))

  spec <- yaml::read_yaml(out_path)
  expect_equal(spec$purpose, "development")
  expect_equal(spec$level, "marginal")
  expect_equal(spec$name_strategy, "preserve")
  expect_null(spec$engine)
})

test_that("spec command returns processing error for invalid purpose", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "spec.yaml")

  result <- run_cli(c("spec", "--purpose", "not_a_purpose", "--out", out_path))

  expect_identical(result$code, 1L)
  expect_false(file.exists(out_path))
})

test_that("synthesize reports processing error when spec file is missing", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path <- file.path(tmp, "bundle.zip")

  result <- run_cli(c("synthesize", data_path, "--spec", file.path(tmp, "missing.yaml"), "--out", out_path))

  expect_identical(result$code, 1L)
  expect_false(file.exists(out_path))
})

test_that("synthesize command writes the restructured bundle zip", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  out_path <- file.path(tmp, "synthetic_bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123), spec_path)

  result <- run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))

  listing <- utils::unzip(out_path, list = TRUE)
  expect_setequal(
    listing$Name,
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

test_that("inspect command summarizes bundle without original data", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  bundle_path <- file.path(tmp, "synthetic_bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123), spec_path)
  expect_identical(
    dataganger_cli(c("synthesize", data_path, "--spec", spec_path, "--out", bundle_path), quit = FALSE),
    0L
  )

  out <- capture.output(code <- dataganger_cli(c("inspect", bundle_path), quit = FALSE))

  expect_identical(code, 0L)
  inspected <- paste(out, collapse = "\n")
  expect_match(inspected, "Synthetic bundle", fixed = TRUE)
  expect_match(inspected, "Variables:", fixed = TRUE)
  expect_match(inspected, "Privacy", fixed = TRUE)
})

test_that("exec shim prints help through Rscript", {
  testthat::skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")), "CLI shim subprocess is covered by source tests and manual smoke tests")
  shim <- system.file("exec", "dataganger", package = "dataganger")
  if (!nzchar(shim)) {
    shim <- testthat::test_path("..", "..", "exec", "dataganger")
  }
  expect_true(file.exists(shim))

  result <- system2("Rscript", c(shQuote(shim), "--help"), stdout = TRUE, stderr = TRUE)
  expect_match(paste(result, collapse = "\n"), "Usage: dataganger", fixed = TRUE)
})

test_that("make-agent-bundle command writes the restructured bundle zip", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path  <- file.path(tmp, "agent.zip")

  result <- run_cli(c("make-agent-bundle", data_path, "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))
  listing <- utils::unzip(out_path, list = TRUE)$Name
  expect_true("synthetic_data.csv"   %in% listing)
  expect_true("human/human.md" %in% listing)
  expect_true("agent/recipe.yaml" %in% listing)
  expect_true("agent/manifest.json" %in% listing)
})

test_that("make-agent-bundle exits 2 when --out is missing", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)

  result <- run_cli(c("make-agent-bundle", data_path))
  expect_identical(result$code, 2L)
})

test_that("make-agent-bundle uses development as default purpose", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path  <- file.path(tmp, "agent.zip")

  result <- run_cli(c("make-agent-bundle", data_path, "--out", out_path))
  expect_identical(result$code, 0L)

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$purpose, "development")
})

test_that("export-diagnostic command writes valid diagnostic JSON", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  out_path  <- file.path(tmp, "diag.json")

  result <- run_cli(c("export-diagnostic", data_path, "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))
  diag <- jsonlite::read_json(out_path)
  expect_equal(diag$source, "dataganger")
  expect_type(diag$dataset$n_rows_bucket, "character")
  expect_true(length(diag$columns) > 0L)
})

test_that("export-diagnostic exits 2 when --out is missing", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)

  result <- run_cli(c("export-diagnostic", data_path))
  expect_identical(result$code, 2L)
})


test_that("synthesize --engine internal works (explicit flag)", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  out_path  <- file.path(tmp, "bundle.zip")

  spec <- synth_spec(purpose = "demo")
  yaml::write_yaml(unclass(spec), spec_path)

  result <- run_cli(c("synthesize", data_path,
                      "--spec", spec_path,
                      "--out", out_path,
                      "--engine", "internal"))
  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))
})

test_that("synthesize routes development to synthpop and records provenance (no --engine)", {
  skip_if_no_synthpop()
  tmp       <- withr::local_tempdir()
  data_path <- file.path(tmp, "data.csv")
  spec_path <- file.path(tmp, "spec.yaml")
  out_path  <- file.path(tmp, "bundle.zip")

  set.seed(11)
  n <- 60
  x <- rnorm(n)
  readr::write_csv(
    tibble::tibble(
      x = x,
      lab_value = round(2 * x + rnorm(n, sd = 0.3), 2),  # distinctive numeric -> kept
      arm = rep(c("A", "B"), length.out = n)
    ),
    data_path
  )
  # development presets preserve_correlations = "moderate" -> synthpop engine
  yaml::write_yaml(list(purpose = "development", n = n, seed = 7L), spec_path)

  # small synthetic data can trip the exact-row-match privacy warning; not under test here
  result <- suppressWarnings(
    run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path))
  )
  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)

  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$engine, "synthpop")
  expect_match(manifest$synthesis_citation, "synthpop", ignore.case = TRUE)

  syn <- readr::read_csv(file.path(extract_dir, "synthetic_data.csv"), show_col_types = FALSE)
  expect_true("lab_value" %in% names(syn))  # distinctive numeric survived end-to-end
})

test_that("apply_disclosure_overrides sets per-column disclosure roles", {
  roles <- detect_roles(data.frame(
    a = rep(c("x", "y"), 25), b = rnorm(50), stringsAsFactors = FALSE
  ))
  out <- apply_disclosure_overrides(roles, list(a = "quasi", b = "none"))
  dr <- stats::setNames(out$disclosure_role, out$variable)
  expect_equal(dr[["a"]], "quasi")
  expect_equal(dr[["b"]], "none")
})

test_that("apply_disclosure_overrides rejects unknown column or value", {
  roles <- detect_roles(data.frame(a = 1:50))
  expect_error(apply_disclosure_overrides(roles, list(zzz = "quasi")), "unknown column")
  expect_error(apply_disclosure_overrides(roles, list(a = "bogus")), "must be one of")
})

test_that("cli_read_spec_yaml carries a disclosure_roles map", {
  tmp <- tempfile(fileext = ".yaml")
  writeLines(c(
    "purpose: development",
    "disclosure_roles:",
    "  age: quasi",
    "  diagnosis: sensitive"
  ), tmp)
  spec <- cli_read_spec_yaml(tmp)
  expect_equal(attr(spec, "disclosure_roles"), list(age = "quasi", diagnosis = "sensitive"))
})

test_that("cli_read_spec_yaml and cli_read_roles_yaml read a combined recipe", {
  df <- data.frame(age = 1:5, token = letters[1:5], stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".yaml")
  writeLines(c(
    "purpose: development",
    "seed: 11",
    "roles:",
    "  - variable: age",
    "    identifies: none",
    "    sensitive: true",
    "  - variable: token",
    "    identifies: direct",
    "    simulation: drop"
  ), tmp)

  spec <- cli_read_spec_yaml(tmp)
  roles <- cli_read_roles_yaml(tmp, df)

  expect_equal(spec$purpose, "development")
  expect_equal(spec$seed, 11)
  expect_equal(roles$simulation[roles$variable == "token"], "drop")
  expect_true(roles$sensitive[roles$variable == "age"])
})


test_that("synthesize with YAML engine auto keeps demo on internal", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  out_path <- file.path(tmp, "bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123, engine = "auto"), spec_path)
  result <- suppressWarnings(run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path)))
  expect_identical(result$code, 0L)
  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$engine, "internal")
})

test_that("synthesize with YAML engine auto uses synthpop for development when available", {
  skip_if_no_synthpop()
  withr::local_options(list(dataganger.disable_synthpop = FALSE))
  tmp <- withr::local_tempdir()
  data_path <- file.path(tmp, "data.csv")
  spec_path <- file.path(tmp, "spec.yaml")
  out_path <- file.path(tmp, "bundle.zip")
  set.seed(11)
  n <- 60
  x <- rnorm(n)
  readr::write_csv(tibble::tibble(x = x, lab_value = round(2 * x + rnorm(n, sd = 0.3), 2), arm = rep(c("A", "B"), length.out = n)), data_path)
  yaml::write_yaml(list(purpose = "development", n = n, seed = 7L, engine = "auto"), spec_path)
  result <- suppressWarnings(run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path)))
  expect_identical(result$code, 0L)
  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$engine, "synthpop")
})

test_that("synthesize records internal engine and no synthpop citation for demo", {
  tmp       <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  out_path  <- file.path(tmp, "bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123), spec_path)

  result <- suppressWarnings(
    run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path))
  )
  expect_identical(result$code, 0L)

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$engine, "internal")
  expect_null(manifest$synthesis_citation)
})


test_that("spec command supports analytics acknowledgement", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "analytics-spec.yaml")

  result <- run_cli(c(
    "spec", "--purpose", "analytics", "--acknowledge-risk", "true", "--out", out_path
  ))

  expect_identical(result$code, 0L)
  spec <- yaml::read_yaml(out_path)
  expect_equal(spec$purpose, "analytics")
  expect_true(isTRUE(spec$acknowledged_risk))
  expect_null(spec$engine)
})

test_that("cli_read_spec_yaml accepts engine and acknowledgement fields", {
  tmp <- tempfile(fileext = ".yaml")
  writeLines(c(
    "purpose: analytics",
    "acknowledge_risk: true",
    "engine: internal"
  ), tmp)
  spec <- cli_read_spec_yaml(tmp)
  expect_true(isTRUE(spec$acknowledged_risk))
  expect_equal(spec$engine, "internal")
})

test_that("cli_read_spec_yaml normalizes engine auto to objective-derived", {
  tmp <- tempfile(fileext = ".yaml")
  writeLines(c(
    "purpose: development",
    "engine: auto"
  ), tmp)
  spec <- cli_read_spec_yaml(tmp)
  expect_null(spec[["engine", exact = TRUE]])
})

test_that("yaml engine is honored unless CLI --engine overrides it", {
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  spec_path <- file.path(tmp, "spec.yaml")
  out_path <- file.path(tmp, "bundle.zip")
  yaml::write_yaml(list(purpose = "demo", n = 5, seed = 123, engine = "synthpop"), spec_path)

  result <- suppressWarnings(
    run_cli(c("synthesize", data_path, "--spec", spec_path, "--out", out_path, "--engine", "internal"))
  )
  expect_identical(result$code, 0L)

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out_path, exdir = extract_dir)
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  expect_equal(manifest$engine, "internal")
})

test_that("synthesize --recipe reads combined spec and roles", {
  skip_if_no_synthpop()
  tmp <- withr::local_tempdir()
  data_path <- file.path(tmp, "d.csv")
  recipe_path <- file.path(tmp, "recipe.yaml")
  out_path <- file.path(tmp, "bundle.zip")

  df <- data.frame(
    age = sample(20:80, 60, TRUE),
    token = sprintf("T%04d", 1:60),
    grp = rep(c("a", "b"), length.out = 60),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, data_path)
  writeLines(c(
    "purpose: development",
    "n: 60",
    "seed: 7",
    "roles:",
    "  - variable: token",
    "    identifies: direct",
    "    simulation: drop"
  ), recipe_path)

  res <- suppressWarnings(run_cli(c(
    "synthesize", data_path, "--recipe", recipe_path, "--out", out_path
  )))
  expect_identical(res$code, 0L)

  ex <- file.path(tmp, "ex")
  dir.create(ex)
  utils::unzip(out_path, exdir = ex)
  syn <- readr::read_csv(file.path(ex, "synthetic_data.csv"), show_col_types = FALSE)
  expect_false("token" %in% names(syn))
})

test_that("synthesize --roles scrambles a direct alphanumeric-ID column instead of leaking it", {
  # A column marked `direct` that is also an alphanumeric ID gets the
  # scramble default (`dg_sync_roles_axes`), and that explicit keep-decision
  # takes precedence over the direct-ID drop in `enforce_kanon`. The column
  # therefore survives, but its values are fully replaced -- no original
  # identifier value leaks. (Dropping a direct ID is now Action-only: the
  # user selects the "drop" simulation, exercised elsewhere.)
  skip_if_no_synthpop()
  tmp <- withr::local_tempdir()
  dp <- file.path(tmp, "d.csv")
  rp <- file.path(tmp, "r.yaml")
  sp <- file.path(tmp, "s.yaml")
  op <- file.path(tmp, "b.zip")
  df <- data.frame(
    age = sample(20:80, 60, TRUE),
    token = sprintf("T%04d", 1:60),
    grp = rep(c("a", "b"), length.out = 60),
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, dp)

  roles <- detect_roles(df)
  roles$identifies[roles$variable == "token"] <- "direct"
  roles$identifies[roles$variable == "age"] <- "none"
  roles <- dg_sync_roles_axes(roles)
  expect_identical(
    roles$simulation[roles$variable == "token"], "scramble"
  )
  cli_write_yaml(roles_to_yaml_list(roles), rp)
  yaml::write_yaml(list(purpose = "development", n = 60, seed = 7L), sp)

  res <- suppressWarnings(run_cli(c(
    "synthesize", dp, "--spec", sp,
    "--roles", rp, "--out", op
  )))
  expect_identical(res$code, 0L)
  ex <- file.path(tmp, "ex")
  dir.create(ex)
  utils::unzip(op, exdir = ex)
  syn <- readr::read_csv(file.path(ex, "synthetic_data.csv"), show_col_types = FALSE)
  # Column is kept (scrambled), but none of the original tokens survive.
  expect_true("token" %in% names(syn))
  expect_length(intersect(df$token, syn$token), 0L)
})

test_that("synthesize accepts a relative --out path", {
  # Regression: every other CLI test builds paths from withr::local_tempdir(),
  # which is absolute, so nothing covered the form a user actually types:
  #   dataganger synthesize data.csv --spec spec.yaml --out bundle.zip
  #
  # Two distinct failures hid behind that gap. rmarkdown::render() resolves a
  # relative output_file against the template directory and does not restore
  # the working directory afterwards; and zip::zip() switches to `root` before
  # forcing its lazily-evaluated `files` argument, so paths normalized inline
  # resolved against the wrong directory. Both are invisible with an absolute
  # path and fatal with a relative one.
  tmp <- withr::local_tempdir()
  data_path <- cli_fixture_csv(tmp)
  withr::local_dir(tmp)

  spec_result <- run_cli(c("spec", "--purpose", "demo", "--out", "spec.yaml"))
  expect_identical(spec_result$code, 0L)

  result <- run_cli(c(
    "synthesize", basename(data_path),
    "--spec", "spec.yaml",
    "--out", "bundle.zip"
  ))

  expect_identical(result$code, 0L)
  expect_true(file.exists("bundle.zip"))

  entries <- utils::unzip("bundle.zip", list = TRUE)$Name
  expect_in(
    c("agent/manifest.json", "agent/AGENT.md", "human/human.md",
      "synthetic_data.csv"),
    entries
  )

  # The staging directory is created alongside the output; it must not survive.
  expect_length(list.files(".", pattern = "^\\.bundle-", all.files = TRUE), 0L)
})
