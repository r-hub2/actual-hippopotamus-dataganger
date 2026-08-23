#' Report a problem or share feedback
#'
#' Prints a pre-filled GitHub issue you can copy into your browser for the
#' `lennon-li/dataganger` repository, with package and R environment details
#' already populated. Use this to report a bug, suggest a feature, or send
#' general feedback without copying session details by hand.
#'
#' @param message Character. A short description of the problem or suggestion.
#'   If `NULL`, a placeholder prompt is used.
#' @param context Character. Optional context about where the issue happened,
#'   such as `"Shiny app"` or `"export_synthetic()"`.
#' @param type Character. One of `"feedback"`, `"bug"`, or `"feature"`.
#'
#' @return Invisibly, the GitHub issue URL that was printed.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   report_issue(
#'     message = "The export step was unclear when I skipped the dictionary.",
#'     context = "Shiny app",
#'     type = "feedback"
#'   )
#' }
report_issue <- function(message = NULL, context = NULL, type = c("feedback", "bug", "feature")) {
  url <- .build_issue_url(message = message, context = context, type = type)
  body <- .issue_body_from_url(url)

  cli::cli_h2("Pre-filled GitHub issue")
  cli::cli_text("GitHub issue URL:")
  cat(url, "\n\n", sep = "")
  cli::cli_text("Issue body:")
  cat(body, "\n", sep = "")

  invisible(url)
}

.issue_body_from_url <- function(url) {
  body <- sub("^.*[?&]body=([^&]+).*$", "\\1", url)
  utils::URLdecode(body)
}

.build_issue_url <- function(message = NULL, context = NULL, type = c("feedback", "bug", "feature")) {
  type <- match.arg(type)

  type_meta <- switch(
    type,
    feedback = list(label = "feedback", title_prefix = "Feedback"),
    bug = list(label = "bug", title_prefix = "App bug"),
    feature = list(label = "enhancement", title_prefix = "Feature request")
  )

  msg <- message %||% "<!-- Describe the issue or suggestion here -->"
  context_line <- if (!is.null(context) && nzchar(context)) {
    paste0("**Context:** ", context, "\n\n")
  } else {
    ""
  }

  pkg_ver <- tryCatch(
    as.character(utils::packageVersion("dataganger")),
    error = function(e) "unknown"
  )
  r_ver <- paste0(R.version$major, ".", R.version$minor)
  platform <- R.version$platform %||% "unknown"
  sys <- Sys.info()
  os_info <- paste(
    stats::na.omit(c(sys[["sysname"]], sys[["release"]])),
    collapse = " "
  )
  if (!nzchar(os_info)) {
    os_info <- "unknown"
  }

  body <- paste0(
    "## ", type_meta$title_prefix, "\n\n",
    "<!-- Privacy note: never include dataset content, column names, file paths, or values. -->\n\n",
    context_line,
    "**Details:**\n\n",
    msg, "\n\n",
    "## Environment\n\n",
    "| Field | Value |\n",
    "|-------|-------|\n",
    "| dataganger | `", pkg_ver, "` |\n",
    "| R | `", r_ver, "` |\n",
    "| Platform | `", platform, "` |\n",
    "| OS | `", os_info, "` |\n\n",
    "## Steps to reproduce\n\n",
    "1. \n",
    "2. \n",
    "3. \n"
  )

  title <- paste0(type_meta$title_prefix, ": ", strtrim(msg, 70))

  paste0(
    "https://github.com/lennon-li/dataganger/issues/new",
    "?labels=", type_meta$label,
    "&title=", utils::URLencode(title, reserved = TRUE),
    "&body=", utils::URLencode(body, reserved = TRUE)
  )
}
