.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion("dataganger")
  packageStartupMessage(
    "dataganger ", v, "\n",
    "  Start the app: dataganger::run_app()"
  )
}
