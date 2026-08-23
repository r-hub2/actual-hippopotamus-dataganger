# ===========================================================================
# Postal code format registry and detection
# ===========================================================================

#' Country postal code format registry
#'
#' Returns a named list of country entries keyed by ISO 3166-1 alpha-2 code.
#' Each entry carries the regex, display template, and per-position slot
#' specification needed to generate valid synthetic postal codes.
#'
#' @return A named list of country format entries.
#' @keywords internal
#' @noRd
dg_postal_format_registry <- function() {
  list(
    CA = list(
      country = "CA",
      name = "Canada",
      regex = "^[ABCEGHJ-NPRSTVXY][0-9][ABCEGHJ-NPRSTV-Z] [0-9][ABCEGHJ-NPRSTV-Z][0-9]$",
      template = "A1A 1A1",
      slots = list(
        list(type = "letter", chars = "ABCEGHJKLMNPRSTVXY"),
        list(type = "digit", chars = "0123456789"),
        list(type = "letter", chars = "ABCEGHJKLMNPRSTVWXYZ"),
        list(type = "literal", chars = " "),
        list(type = "digit", chars = "0123456789"),
        list(type = "letter", chars = "ABCEGHJKLMNPRSTVWXYZ"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    US = list(
      country = "US",
      name = "United States",
      regex = "^[0-9]{5}$",
      template = "12345",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    UK = list(
      country = "UK",
      name = "United Kingdom",
      regex = "^[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][A-Z]{2}$",
      template = "A9 9AA",
      slots = list(
        list(type = "letter", chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        list(type = "digit", chars = "0123456789"),
        list(type = "literal", chars = " "),
        list(type = "digit", chars = "0123456789"),
        list(type = "letter", chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        list(type = "letter", chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      )
    ),
    AU = list(
      country = "AU",
      name = "Australia",
      regex = "^[0-9]{4}$",
      template = "1234",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    DE = list(
      country = "DE",
      name = "Germany",
      regex = "^[0-9]{5}$",
      template = "12345",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    FR = list(
      country = "FR",
      name = "France",
      regex = "^[0-9]{5}$",
      template = "12345",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    JP = list(
      country = "JP",
      name = "Japan",
      regex = "^[0-9]{3}-[0-9]{4}$",
      template = "123-4567",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "literal", chars = "-"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    IN = list(
      country = "IN",
      name = "India",
      regex = "^[0-9]{6}$",
      template = "123456",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    BR = list(
      country = "BR",
      name = "Brazil",
      regex = "^[0-9]{5}-[0-9]{3}$",
      template = "12345-678",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "literal", chars = "-"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789")
      )
    ),
    NL = list(
      country = "NL",
      name = "Netherlands",
      regex = "^[0-9]{4} [A-Z]{2}$",
      template = "1234 AB",
      slots = list(
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "digit", chars = "0123456789"),
        list(type = "literal", chars = " "),
        list(type = "letter", chars = "BCEFGHJKLMNPRTUVWXYZ"),
        list(type = "letter", chars = "BCEFGHJKLMNPRTUVWXYZ")
      )
    )
  )
}

#' Detect the postal code format of a character column
#'
#' Samples up to 200 non-NA values and tests them against the country format
#' registry. Returns the best-matching registry entry or NULL.
#'
#' @param x Character vector of postal code values.
#' @param country_hint Optional ISO 3166-1 alpha-2 code to narrow detection.
#' @return A registry entry list, or NULL if no format matches.
#' @keywords internal
#' @noRd
detect_postal_format <- function(x, country_hint = NA_character_) {
  if (!is.na(country_hint) && nzchar(country_hint)) country_hint <- toupper(country_hint)
  x_sample <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x_sample) > 200L) x_sample <- x_sample[seq_len(200L)]
  x_sample <- toupper(trimws(x_sample))
  if (length(x_sample) < 5L) {
    return(NULL)
  }

  registry <- dg_postal_format_registry()

  if (!is.na(country_hint) && nzchar(country_hint)) {
    entry <- registry[[country_hint]]
    if (is.null(entry)) {
      return(NULL)
    }
    match_rate <- mean(grepl(entry$regex, x_sample))
    if (match_rate >= 0.9) {
      return(entry)
    }
    return(NULL)
  }

  match_rates <- vapply(
    names(registry),
    function(code) mean(grepl(registry[[code]]$regex, x_sample)),
    numeric(1)
  )
  candidates <- names(match_rates)[match_rates >= 0.9]
  if (length(candidates) == 0L) {
    return(NULL)
  }

  best_rate <- max(match_rates[candidates])
  best <- candidates[match_rates[candidates] == best_rate]

  result <- registry[[best[[1L]]]]
  if (length(best) > 1L) {
    attr(result, "ambiguous") <- best
  }
  result
}
