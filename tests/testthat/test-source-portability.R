local({
test_that("R sources parse without warnings in the C locale", {
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(source_dir), "Package source directory is unavailable")
  source_files <- list.files(source_dir, pattern = "[.]R$", full.names = TRUE)

  withr::local_locale(c(LC_CTYPE = "C"))
  for (source_file in source_files) {
    parse_warnings <- character()
    withCallingHandlers(
      parse(file = source_file),
      warning = function(condition) {
        parse_warnings <<- c(parse_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    )
    expect_equal(parse_warnings, character(), info = source_file)
  }
})
})
