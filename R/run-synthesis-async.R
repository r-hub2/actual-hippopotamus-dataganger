# Background synthesis for the Shiny app (Bug 6 - cancellable generation).
#
# The app runs the full synthesize -> compare -> privacy pipeline in a separate
# R process via callr, so the main Shiny session stays responsive and a Cancel
# button can kill the run. The shared pipeline below is also used by the
# synchronous fallback when callr is unavailable.

#' Run the full synthesis pipeline
#'
#' Synthesizes, compares, and privacy-checks in one call. Used by the Shiny
#' Generation step (in a background process when possible) and directly by the
#' synchronous fallback.
#'
#' @param data A data frame to synthesize from.
#' @param spec A `dataganger_spec`.
#' @param roles Optional `dataganger_roles`.
#'
#' @return A list with `synthetic`, `comparison`, and `privacy`.
#' @keywords internal
#' @noRd
run_synthesis_pipeline <- function(data, spec, roles = NULL) {
  if (!is.null(roles) && !roles_ready_for_generation(roles)) {
    cli::cli_abort(
      "Finish the column privacy questions before running the synthesis pipeline."
    )
  }

  captured_warnings <- character(0)
  pipeline <- withCallingHandlers(
    {
      synthetic <- synthesize_data(data, spec, roles = roles)
      comparison <- compare_synthetic(data, synthetic, roles = roles)
      privacy <- privacy_check(
        data, synthetic,
        roles = roles, stage = "post", spec = spec
      )
      list(synthetic = synthetic, comparison = comparison, privacy = privacy)
    },
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(
    synthetic = pipeline$synthetic,
    comparison = pipeline$comparison,
    privacy = pipeline$privacy,
    warnings = captured_warnings,
    kanon = attr(pipeline$synthetic, "kanon", exact = TRUE)
  )
}

#' Is dataganger running under devtools::load_all (not installed)?
#'
#' The background process loads dataganger from the library, which fails when
#' the package is only dev-loaded (`devtools::load_all`) rather than installed.
#' The Shiny Generation step uses this to fall back to synchronous generation
#' during interactive development instead of spawning a doomed subprocess.
#'
#' @return A single logical.
#' @keywords internal
#' @noRd
synthesis_dev_loaded <- function() {
  # pkgload sets .__DEVTOOLS__ in the namespace when a package is dev-loaded via
  # load_all(). Check for it directly so we have no runtime dependency on pkgload.
  ns <- tryCatch(getNamespace("dataganger"), error = function(e) NULL)
  if (is.null(ns)) return(FALSE)
  exists(".__DEVTOOLS__", envir = ns, inherits = FALSE)
}

#' Launch the synthesis pipeline in a background process
#'
#' Returns a live `callr::r_process` handle. Poll `handle$is_alive()`,
#' collect with `handle$get_result()`, and cancel with `handle$kill()`.
#'
#' @inheritParams run_synthesis_pipeline
#' @return A `callr::r_process`.
#' @keywords internal
#' @noRd
start_synthesis_process <- function(data, spec, roles = NULL) {
  rlang::check_installed("callr", reason = "to run cancellable background synthesis")
  callr::r_bg(
    func = function(data, spec, roles) {
      get("run_synthesis_pipeline", envir = asNamespace("dataganger"))(
        data, spec, roles = roles
      )
    },
    args = list(data = data, spec = spec, roles = roles),
    supervise = TRUE
  )
}
