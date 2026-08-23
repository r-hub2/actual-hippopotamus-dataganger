#' Read a data file into a tibble
#'
#' Reads CSV, Excel (`.xlsx`, `.xls`), SAS (`.sas7bdat`), and XPT (`.xpt`)
#' files into a tibble. Dispatches on file extension.
#'
#' @param file Path to the data file.
#' @param sheet For Excel files, the sheet name or index to read. Passed to
#'   [readxl::read_excel()]. Ignored for non-Excel formats.
#' @param encoding Character encoding for CSV files (e.g. `"UTF-8"`,
#'   `"latin1"`). Passed as `readr::locale(encoding = encoding)`. Ignored when
#'   reading non-CSV formats or when the caller already supplies a `locale`
#'   argument in `...`.
#' @param col_select Optional character vector of column names to keep. For CSV
#'   files the selection is passed directly to [readr::read_csv()] so excluded
#'   columns are never parsed. For other formats columns are subset after
#'   reading.
#' @param ... Additional arguments passed to the underlying reader
#'   (`readr::read_csv()`, `readxl::read_excel()`, `haven::read_sas()`, or
#'   `haven::read_xpt()`).
#'
#' @return A [tibble::tibble()]. SAS/XPT imports preserve `haven_labelled`
#'   vectors as-is.
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".csv")
#' readr::write_csv(data.frame(id = 1:3, grp = c("a", "b", "c")), path)
#' read_input(path)
read_input <- function(file, sheet = NULL, encoding = NULL, col_select = NULL, ...) {
  if (!file.exists(file)) {
    cli::cli_abort("File does not exist: {.path {file}}")
  }

  ext <- tolower(tools::file_ext(file))

  out <- switch(ext,
    csv = {
      if (!is.null(encoding) && !"locale" %in% names(list(...))) {
        if (is.null(col_select)) {
          readr::read_csv(file, locale = readr::locale(encoding = encoding),
                          ..., show_col_types = FALSE)
        } else {
          readr::read_csv(file, locale = readr::locale(encoding = encoding),
                          col_select = dplyr::all_of(col_select),
                          ..., show_col_types = FALSE)
        }
      } else {
        if (is.null(col_select)) {
          readr::read_csv(file, ..., show_col_types = FALSE)
        } else {
          readr::read_csv(file, col_select = dplyr::all_of(col_select), ...,
                          show_col_types = FALSE)
        }
      }
    },
    xlsx = ,
    xls = {
      if (is.null(sheet)) {
        readxl::read_excel(file, ...)
      } else {
        readxl::read_excel(file, sheet = sheet, ...)
      }
    },
    sas7bdat = {
      haven::read_sas(file, ...)
    },
    xpt = {
      haven::read_xpt(file, ...)
    },
    {
      cli::cli_abort(c(
        "Unsupported file extension: {.val {ext}}",
        "i" = "Supported formats: {.val .csv}, {.val .xlsx}, {.val .xls}, {.val .sas7bdat}, {.val .xpt}"
      ))
    }
  )

  # For non-CSV formats, col_select is applied after reading.
  if (!is.null(col_select) && ext != "csv") {
    keep <- intersect(col_select, names(out))
    out <- out[, keep, drop = FALSE]
  }

  out
}
