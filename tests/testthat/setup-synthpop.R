# On CI, disable synthpop-backed synthesis by default. synthpop is installed
# there (it is in Suggests and CI installs Suggests for the check), but a
# synthpop synthesis can hang unattended, which previously ran every CI job to
# the 6h ceiling. Disabling it steers auto-derived synthesis onto the internal
# engine and makes `skip_if_no_synthpop()` skip the synthpop-specific tests.
#
# Exception: a dedicated CI job sets DATAGANGER_TEST_SYNTHPOP=true to actually
# exercise the synthpop path (with a job timeout as the hang guard). CRAN runs
# the synthpop tests, so CI must too on at least one job -- otherwise synthpop
# regressions stay invisible until CRAN sees them.
if (isTRUE(as.logical(Sys.getenv("CI", "false"))) &&
    !isTRUE(as.logical(Sys.getenv("DATAGANGER_TEST_SYNTHPOP", "false")))) {
  options(dataganger.disable_synthpop = TRUE)
}
