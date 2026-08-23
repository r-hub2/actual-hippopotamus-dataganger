test_that("make_agent_bundle() produces a valid zip with the restructured layout", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "agent.zip")

  make_agent_bundle(
    file    = testthat::test_path("fixtures", "tiny.csv"),
    out     = out,
    purpose = "development",
    seed    = 42L
  )

  expect_true(file.exists(out))
  listing <- utils::unzip(out, list = TRUE)$Name
  expect_true("synthetic_data.csv"   %in% listing)
  expect_true("human/human.md"       %in% listing)
  expect_true("agent/recipe.yaml"    %in% listing)
  expect_true("agent/AGENT.md"       %in% listing)
  expect_true("agent/manifest.json"  %in% listing)
  expect_true("agent/code_readiness_report.json" %in% listing)
  expect_false("comparison_report.html" %in% listing)
})

test_that("make_agent_bundle() writes recipe, manifest, and code readiness metadata", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "agent.zip")

  make_agent_bundle(
    file    = testthat::test_path("fixtures", "tiny.csv"),
    out     = out,
    purpose = "demo",
    seed    = 1L
  )

  extract_dir <- file.path(tmp, "extracted")
  dir.create(extract_dir)
  utils::unzip(out, exdir = extract_dir)
  recipe <- yaml::read_yaml(file.path(extract_dir, "agent", "recipe.yaml"))
  manifest <- jsonlite::read_json(file.path(extract_dir, "agent", "manifest.json"))
  code_readiness <- jsonlite::read_json(file.path(extract_dir, "agent", "code_readiness_report.json"))

  expect_equal(recipe$purpose, "demo")
  expect_true(is.list(recipe$roles))
  expect_equal(manifest$source, "dataganger")
  expect_equal(manifest$purpose, "demo")
  expect_equal(manifest$engine, "internal")
  expect_null(manifest$synthesis_citation)
  expect_true(is.logical(code_readiness$summary$ready))
})

test_that("make_agent_bundle() aborts when out parent directory does not exist", {
  expect_error(
    make_agent_bundle(
      file = testthat::test_path("fixtures", "tiny.csv"),
      out  = "/nonexistent_dir_xyz/bundle.zip"
    ),
    "Parent directory does not exist"
  )
})

test_that("make_agent_bundle() aborts when out exists and overwrite = FALSE", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "agent.zip")
  file.create(out)

  expect_error(
    make_agent_bundle(
      file = testthat::test_path("fixtures", "tiny.csv"),
      out  = out
    ),
    "already exists"
  )
})

test_that("make_agent_bundle() overwrites when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "agent.zip")

  make_agent_bundle(
    file = testthat::test_path("fixtures", "tiny.csv"),
    out  = out,
    seed = 1L
  )

  expect_no_error(
    make_agent_bundle(
      file      = testthat::test_path("fixtures", "tiny.csv"),
      out       = out,
      seed      = 2L,
      overwrite = TRUE
    )
  )
  expect_true(file.exists(out))
})

test_that("make_agent_bundle() passes ... to read_input (encoding arg)", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "agent.zip")

  # encoding = "UTF-8" is valid and should not error
  expect_no_error(
    make_agent_bundle(
      file     = testthat::test_path("fixtures", "tiny.csv"),
      out      = out,
      seed     = 1L,
      encoding = "UTF-8"
    )
  )
})
