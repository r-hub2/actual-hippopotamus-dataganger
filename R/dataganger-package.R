#' DataGangeR uses two synthesis engines. By default the engine is chosen
#' automatically from your objective: demo uses the dependency-free internal
#' engine, development uses synthpop when it is installed so moderate
#' correlations can be preserved, and analytics requires synthpop plus an
#' explicit risk acknowledgement because high-fidelity synthesis may retain
#' sensitive structure. The engine can also be selected explicitly (auto,
#' internal, or synthpop) in both the Shiny app and the CLI. Install
#' synthpop with `install.packages("synthpop")` to enable
#' relationship-preserving synthesis at full fidelity.
#' When synthpop is used, please cite: Nowok B, Raab GM, Dibben C (2016).
#' "synthpop: Bespoke Creation of Synthetic Data in R." *Journal of
#' Statistical Software*, 74(11), 1-26. doi:10.18637/jss.v074.i11
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("variable", "std_diff", "color", "tvd", ".data"))

## usethis namespace: start
## usethis namespace: end
NULL
