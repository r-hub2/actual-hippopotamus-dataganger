cli_fixture_csv <- function(tmp = withr::local_tempdir()) {
  path <- file.path(tmp, "fixture.csv")
  readr::write_csv(
    tibble::tibble(
      id = 1:5,
      group = factor(c("a", "a", "b", "b", "c")),
      score = c(10, NA, 12, 13, 14),
      note = c("alpha", "beta", "gamma", "delta", "epsilon")
    ),
    path
  )
  path
}

# Run dataganger_cli() and capture its exit code, printed output, and messages.
#
# cli signals its alerts as `cliMessage` conditions rather than writing to the
# stderr sink, so `capture.output(type = "message")` only sees them when the
# condition happens to fall through to the sink. That fall-through depends on
# how the package is loaded: it holds under pkgload (testthat::test_local /
# test_file) but NOT under the `load_package = "source"` loading that
# devtools::test() uses, where the capture comes back empty. Catch the
# conditions directly so the capture is identical under every runner.
run_cli <- function(args) {
  messages <- character()
  output <- capture.output(
    code <- withCallingHandlers(
      dataganger_cli(args, quit = FALSE),
      cliMessage = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  )
  list(code = code, output = output, messages = messages)
}
