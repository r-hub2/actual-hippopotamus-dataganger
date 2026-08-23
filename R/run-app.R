#' Launch the DataGangeR Shiny Application
#'
#' Opens the DataGangeR interactive workflow in a local Shiny app. Requires
#' the `shiny`, `bslib`, `DT`, and `plotly` packages.
#'
#' @param max_upload_mb Maximum file upload size in megabytes. Default 50.
#' @param launch Whether to launch the app. Default `interactive()`. Set to
#'   `FALSE` to configure options without blocking (useful for testing).
#' @param port Port to pass to `shiny::runApp()`. Default `NULL`.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(max_upload_mb = 50, launch = interactive(), port = NULL, ...) {
  rlang::check_installed(
    c("shiny", "bslib", "DT", "plotly"),
    reason = "to run the DataGangeR Shiny app"
  )
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  if (launch) {
    .run_shiny_app(
      appDir = system.file("app", package = "dataganger"),
      port = port,
      display.mode = "normal",
      ...
    )
  }
  invisible(NULL)
}

# Internal wrapper so tests can mock without touching shiny's namespace.
.run_shiny_app <- function(...) shiny::runApp(...)
