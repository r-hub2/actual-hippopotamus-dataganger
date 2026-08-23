local({
local_no_network_traps <- function() {
  # testthat's mocked bindings do not reliably bind these base/namespace
  # functions in this setup, so trap them directly at the namespace level.
  boom <- quote(stop("network access attempted", call. = FALSE))

  trace("url", tracer = boom, where = asNamespace("base"), print = FALSE)
  trace("socketConnection", tracer = boom, where = asNamespace("base"), print = FALSE)
  trace("download.file", tracer = boom, where = asNamespace("utils"), print = FALSE)

  withr::defer(untrace("url", where = asNamespace("base")))
  withr::defer(untrace("socketConnection", where = asNamespace("base")))
  withr::defer(untrace("download.file", where = asNamespace("utils")))

  invisible(NULL)
}

test_that("the full pipeline and app UI construction make no network calls", {
  local_no_network_traps()

  df <- data.frame(
    age = sample(20:80, 40, replace = TRUE),
    grp = sample(c("a", "b"), 40, replace = TRUE),
    stringsAsFactors = FALSE
  )
  roles <- dg_sync_roles_axes(detect_roles(df))
  spec <- synth_spec("development", seed = 1L, engine = "internal")
  syn <- synthesize_data(df, spec, roles = roles)

  expect_s3_class(compare_synthetic(df, syn, roles = roles), "dataganger_comparison")

  out_dir <- file.path(withr::local_tempdir(), "bundle")
  expect_no_error(suppressWarnings(
    export_synthetic(
      syn,
      original = df,
      roles = roles,
      path = out_dir,
      format = "dir",
      include_report = FALSE
    )
  ))
  expect_true(file.exists(file.path(out_dir, "human", "human.md")))
  expect_true(file.exists(file.path(out_dir, "agent", "manifest.json")))

  app_env <- new.env(parent = globalenv())
  # Resolve via system.file so this works both from source (load_all) and from
  # the installed package under R CMD check.
  app_path <- system.file("app", "app.R", package = "dataganger")
  skip_if(!nzchar(app_path) || !file.exists(app_path), "app.R not found")
  expect_no_error(sys.source(app_path, envir = app_env))
  expect_true(exists("ui", envir = app_env, inherits = FALSE))
  expect_true(exists("server", envir = app_env, inherits = FALSE))
})

test_that("package source contains no network primitives", {
  # Source-level guard: only meaningful when the R/ source tree is present
  # (dev / test_local / the from-source CI jobs). Installed-package test runs
  # under R CMD check have no R/ sources, so skip honestly rather than pass
  # vacuously.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(r_dir), "R/ source not available (installed package)")
  app_dir <- testthat::test_path("..", "..", "inst", "app")
  files <- c(
    list.files(
      r_dir,
      pattern = "\\.[Rr]$",
      full.names = TRUE
    ),
    list.files(
      app_dir,
      pattern = "\\.[Rr]$",
      full.names = TRUE
    )
  )
  pattern <- paste(
    "\\burl\\(",
    "download\\.file",
    "socketConnection",
    "\\bhttr\\b",
    "\\bcurl\\b",
    "\\bRCurl\\b",
    "GET\\(",
    "POST\\(",
    "nsl\\(",
    "browseURL\\(",
    "font_google",
    "font_link",
    "fonts\\.googleapis",
    sep = "|"
  )

  matches <- unlist(lapply(files, function(path) {
    lines <- readLines(path, warn = FALSE)
    hit <- grep(pattern, lines, perl = TRUE)
    if (!length(hit)) return(character())
    sprintf("%s:%d:%s", basename(path), hit, lines[hit])
  }), use.names = FALSE)

  expect_true(length(matches) == 0, info = paste(matches, collapse = "\n"))
})
})
