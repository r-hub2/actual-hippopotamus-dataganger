local({

source_slice <- function(lines, location) {
  first <- location$line1[[1L]]
  last <- location$line2[[1L]]
  if (first == last) {
    return(substr(lines[[first]], location$col1[[1L]], location$col2[[1L]]))
  }
  pieces <- c(
    substr(lines[[first]], location$col1[[1L]], nchar(lines[[first]])),
    if (last > first + 1L) lines[seq.int(first + 1L, last - 1L)],
    substr(lines[[last]], 1L, location$col2[[1L]])
  )
  paste(pieces, collapse = "\n")
}

opaque_expectation_violations <- function(paths) {
  violations <- list()
  for (path in paths) {
    parsed <- parse(file = path, keep.source = TRUE)
    parse_data <- getParseData(parsed)
    lines <- readLines(path, warn = FALSE)
    expectation_symbols <- parse_data[
      parse_data$token == "SYMBOL_FUNCTION_CALL" &
        parse_data$text %in% c("expect_true", "expect_false"),
      , drop = FALSE
    ]
    for (row in seq_len(nrow(expectation_symbols))) {
      symbol <- expectation_symbols[row, , drop = FALSE]
      function_expr <- parse_data[parse_data$id == symbol$parent, , drop = FALSE]
      call_expr <- parse_data[parse_data$id == function_expr$parent, , drop = FALSE]
      call_text <- source_slice(lines, call_expr)
      call <- tryCatch(parse(text = call_text)[[1L]], error = function(e) NULL)
      if (is.null(call) || length(call) < 2L) next

      arguments <- as.list(call)[-1L]
      argument_names <- names(arguments)
      has_info <- !is.null(argument_names) && "info" %in% argument_names
      tested <- arguments[[1L]]
      is_aggregate <- is.call(tested) &&
        as.character(tested[[1L]]) %in% c("all", "any")
      if (is_aggregate && !has_info) {
        expression <- paste(deparse(call, width.cutoff = 500L), collapse = " ")
        violations[[length(violations) + 1L]] <- data.frame(
          file = basename(path),
          line = call_expr$line1[[1L]],
          expression = expression,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(violations) == 0L) {
    return(data.frame(
      file = character(), line = integer(), expression = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, violations)
}

test_that("opaque aggregate assertion scanner reports actionable locations", {
  fixture <- tempfile(fileext = ".R")
  writeLines(c(
    "test_that(\"bad fixture\", {",
    "  expect_true(any(grepl(\"needle\", output)))",
    "  testthat::expect_false(all(is.na(output)))",
    "  expect_true(any(output), info = \"inspected output\")",
    "})"
  ), fixture, useBytes = TRUE)

  violations <- opaque_expectation_violations(fixture)

  expect_equal(violations$file, rep(basename(fixture), 2L))
  expect_equal(violations$line, c(2L, 3L))
  expect_match(violations$expression[[1L]], "expect_true", fixed = TRUE)
  expect_match(violations$expression[[2L]], "expect_false", fixed = TRUE)
  expect_match(violations$expression[[1L]], "any(grepl", fixed = TRUE)
  expect_match(violations$expression[[2L]], "all(is.na", fixed = TRUE)
})

test_that("test files contain no opaque aggregate boolean assertions", {
  test_dir <- testthat::test_path()
  test_files <- list.files(test_dir, pattern = "^test.*[.]R$", full.names = TRUE)
  violations <- opaque_expectation_violations(test_files)
  diagnostic <- if (nrow(violations) == 0L) character() else sprintf(
    "%s:%d: %s", violations$file, violations$line, violations$expression
  )

  expect_equal(
    diagnostic, character(),
    info = paste(c("Opaque aggregate boolean assertions:", diagnostic), collapse = "\n")
  )
})

})
