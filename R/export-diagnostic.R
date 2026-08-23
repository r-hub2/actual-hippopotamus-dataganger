#' Export a Lens-compatible diagnostic schema for a dataset
#'
#' Profiles a data frame and writes a \code{diagnostic_view.json} describing
#' column roles, sensitivity, and exposure levels. Does not synthesise data.
#' Intended for agent pre-inspection and Lens ingestion.
#'
#' @param data A data frame to describe.
#' @param path Output path for the JSON file.
#' @param roles Optional; a \code{dataganger_roles} object from
#'   [detect_roles()]. Computed internally if \code{NULL}.
#' @param profile Optional; a \code{dataganger_profile} object from
#'   [profile_data()]. Computed internally if \code{NULL}.
#' @param overwrite Logical. When \code{FALSE} (the default), aborts if
#'   \code{path} already exists.
#'
#' @return Invisibly, the written JSON path.
#' @export
#'
#' @examples
#' dat <- data.frame(age = c(34, 29, 41), grp = c("a", "b", "c"))
#' export_diagnostic_package(dat, path = tempfile(fileext = ".json"))
export_diagnostic_package <- function(data, path, roles = NULL,
                                      profile = NULL, overwrite = FALSE) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }

  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty character string")
  }

  out_parent <- dirname(path)
  if (!dir.exists(out_parent)) {
    cli::cli_abort(c(
      "Parent directory does not exist: {.file {out_parent}}",
      "i" = "Create the directory first or use an existing path."
    ))
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "Output file already exists at {.file {path}}; set {.arg overwrite = TRUE} to replace it"
    )
  }

  if (is.null(profile)) profile <- profile_data(data)
  if (is.null(roles))   roles   <- detect_roles(data, profile = profile)

  col_info <- lapply(seq_len(nrow(roles)), function(i) {
    role  <- roles$recommended_role[i]
    level <- diagnostic_exposure_level(role)
    list(
      name           = roles$variable[i],
      type           = roles$class[i],
      role           = role,
      disclosure_role = if (!is.na(roles$disclosure_role[i])) roles$disclosure_role[i] else "none",
      exposed        = level != "blocked",
      exposure_level = level
    )
  })

  has_free_text <- any(roles$recommended_role == "free text")
  has_ids       <- any(roles$recommended_role == "alphanumeric ID")

  diag <- list(
    source             = "dataganger",
    dataganger_version = as.character(utils::packageVersion("dataganger")),
    generated_at       = format(Sys.time(), usetz = TRUE),
    dataset = list(
      n_rows_bucket = bucket_nrows(nrow(data)),
      n_cols        = length(col_info)
    ),
    columns = col_info,
    blocked = list(
      raw_rows         = TRUE,
      free_text_fields = has_free_text,
      id_fields        = has_ids,
      plots            = TRUE
    )
  )

  jsonlite::write_json(
    diag,
    path       = path,
    auto_unbox = TRUE,
    pretty     = TRUE,
    null       = "null"
  )

  invisible(path)
}

diagnostic_exposure_level <- function(role) {
  switch(role,
    "alphanumeric ID" = "blocked",
    "free text"    = "blocked",
    "date"         = "coarsened",
    "schema_only"
  )
}
