test_that(".build_issue_url() builds a labelled GitHub issue URL", {
  url <- dataganger:::.build_issue_url(
    message = "Generation failed on step 4",
    context = "Shiny app",
    type = "bug"
  )

  expect_match(url, "https://github.com/lennon-li/dataganger/issues/new", fixed = TRUE)
  expect_match(url, "labels=bug", fixed = TRUE)
  expect_match(url, "title=", fixed = TRUE)
  expect_match(url, "body=", fixed = TRUE)
  expect_match(url, "%20", fixed = TRUE)

  decoded <- utils::URLdecode(url)
  expect_match(decoded, "App bug: Generation failed on step 4", fixed = TRUE)
  expect_match(decoded, "**Context:** Shiny app", fixed = TRUE)
  expect_match(decoded, "| dataganger | `", fixed = TRUE)
  expect_match(decoded, "| R | `", fixed = TRUE)
  expect_match(decoded, "| Platform | `", fixed = TRUE)
  expect_match(decoded, "## Steps to reproduce", fixed = TRUE)
  expect_match(decoded, "never include dataset content, column names, file paths, or values", fixed = TRUE)
})

test_that(".build_issue_url() maps feedback and feature types to labels", {
  feedback_url <- dataganger:::.build_issue_url(type = "feedback")
  feature_url <- dataganger:::.build_issue_url(type = "feature")

  expect_match(feedback_url, "labels=feedback", fixed = TRUE)
  expect_match(feature_url, "labels=enhancement", fixed = TRUE)

  expect_match(utils::URLdecode(feedback_url), "Feedback: ", fixed = TRUE)
  expect_match(utils::URLdecode(feature_url), "Feature request: ", fixed = TRUE)
})

test_that(".build_issue_url() handles NULL message and context", {
  expect_no_error({
    url <- dataganger:::.build_issue_url(
      message = NULL,
      context = NULL,
      type = "feedback"
    )
  })

  decoded <- utils::URLdecode(url)
  expect_match(decoded, "<!-- Describe the issue or suggestion here -->", fixed = TRUE)
  expect_false(grepl("\\*\\*Context:\\*\\*", decoded))
})

test_that("report_issue() prints a copyable GitHub issue and never calls browseURL", {
  called <- FALSE
  trace(
    what = "browseURL",
    tracer = quote({
      called <<- TRUE
      stop("browseURL should not be called", call. = FALSE)
    }),
    where = asNamespace("utils"),
    print = FALSE
  )
  on.exit(untrace("browseURL", where = asNamespace("utils")), add = TRUE)

  out <- testthat::capture_output_lines(
    url <- dataganger::report_issue(
      message = "Generation failed on step 4",
      context = "Shiny app",
      type = "bug"
    )
  )

  expect_false(called)
  expect_identical(
    url,
    dataganger:::.build_issue_url(
      message = "Generation failed on step 4",
      context = "Shiny app",
      type = "bug"
    )
  )

  printed <- paste(out, collapse = "\n")
  expect_match(printed, "## App bug", fixed = TRUE)
  expect_match(printed, "| dataganger | `", fixed = TRUE)
  expect_match(printed, "https://github.com/lennon-li/dataganger/issues/new", fixed = TRUE)
})
