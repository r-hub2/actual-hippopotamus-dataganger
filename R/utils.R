# Internal utility functions for dataganger

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ---------------------------------------------------------------------------
# Diagnostic logging
# ---------------------------------------------------------------------------

# Emit a timestamped diagnostic line to the console. Gated on
# getOption("dataganger.verbose", TRUE) so users can silence it. Exists to make
# long synthesis / profiling phases observable when a run appears to hang.
dg_log <- function(...) {
  if (!isTRUE(getOption("dataganger.verbose", TRUE))) {
    return(invisible(NULL))
  }
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  invisible(NULL)
}

# Run `expr`, logging a "<label> ..." line before it starts and a
# "<label> done: N.NNs" line after it returns. The before-line is the point:
# if `expr` hangs, the console shows which phase was entered and never left.
dg_timeit <- function(label, expr) {
  dg_log(label, " ...")
  start <- proc.time()[["elapsed"]]
  res <- force(expr)
  dg_log(sprintf("%s done: %.2fs", label, proc.time()[["elapsed"]] - start))
  res
}

# ---------------------------------------------------------------------------
# Cooperative cancellation
# ---------------------------------------------------------------------------

# Abort the current synthesis with a classed condition when
# getOption("dataganger.cancel") is TRUE. Checked at column boundaries inside
# synthesize_marginal() so a long marginal run can be stopped cleanly. Used by
# headless / CLI callers; the Shiny app instead cancels by killing the
# background process that runs the synthesis (see run_synthesis_async()).
check_cancel <- function() {
  if (isTRUE(getOption("dataganger.cancel", FALSE))) {
    cli::cli_abort("Synthesis cancelled.", class = "dataganger_cancelled")
  }
  invisible(NULL)
}

synthpop_citation <- function() {
  paste(
    "Nowok B, Raab GM, Dibben C (2016).",
    "\"synthpop: Bespoke Creation of Synthetic Data in R.\"",
    "Journal of Statistical Software, 74(11), 1-26.",
    "doi:10.18637/jss.v074.i11"
  )
}

