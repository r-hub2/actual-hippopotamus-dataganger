# Skip a test that requires a real synthpop synthesis.
#
# Skips when synthpop is not installed (as `skip_if_not_installed` would) and
# also when synthpop has been intentionally disabled via
# `options(dataganger.disable_synthpop = TRUE)` -- set on CI by
# `setup-synthpop.R` because a synthpop synthesis can hang unattended.
skip_if_no_synthpop <- function() {
  testthat::skip_if_not_installed("synthpop")
  if (isTRUE(getOption("dataganger.disable_synthpop", FALSE))) {
    testthat::skip("synthpop disabled via options(dataganger.disable_synthpop)")
  }
  invisible(NULL)
}
