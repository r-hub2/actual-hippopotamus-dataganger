# Categorical type contract for the whole package.
#
# dataganger accepts and emits categorical data as plain `character`, never as
# `factor` and never as `haven_labelled`. This is a deliberate package-wide
# rule.
#
# Why:
#
#   * No supported input format produces a factor. `readr::read_csv()`,
#     `readxl::read_excel()` and `haven::read_sas()` return character, numeric,
#     date, logical, and (for SAS value formats) `haven_labelled`. A factor can
#     only reach the pipeline from a hand-built data frame passed to
#     `synthesize_data()` directly.
#   * A factor passed directly to `synthesize_data()` is a working
#     representation, not user intent. synthpop may convert character columns
#     to factors for its CART models and hand them back as factors; the public
#     boundary flattens those results.
#   * Without a single flattening point the output type of a column depended on
#     which purpose was chosen -- a labelled column came back character from the
#     marginal engine and factor from synthpop. Same input, different type.
#   * The only export format is CSV, which cannot represent the distinction
#     anyway, so nothing is lost on the way out.
#
# The one thing character cannot carry that factor could is a level declared
# with zero rows. That is dropped on purpose: a category with no rows in the
# source gets no rows in the synthetic copy. Preserving it would mean sampling
# had to emit rows of a category that was never observed, which fabricates data
# and creates a k-anonymity singleton. Unused factor slots are metadata, not
# output values, so no factor-level attribute is carried downstream.
#
# Input normalization happens once at the public entry point. This function is
# the final backstop for factors returned by synthpop.
normalize_categorical_input <- function(data) {
  for (col_name in names(data)) {
    x <- data[[col_name]]
    if (haven::is.labelled(x)) {
      data[[col_name]] <- as.character(haven::as_factor(x))
    } else if (is.factor(x)) {
      data[[col_name]] <- as.character(x)
    }
  }

  data
}

flatten_to_character <- function(syn) {
  for (col_name in names(syn)) {
    x <- syn[[col_name]]
    if (haven::is.labelled(x)) {
      syn[[col_name]] <- as.character(haven::as_factor(x))
    } else if (is.factor(x)) {
      syn[[col_name]] <- as.character(x)
    }
  }
  syn
}

# The character rule covers categorical data, not logicals. synthpop models a
# logical column as a two-level factor, so without this the same logical input
# came back `logical` from the marginal engine and `character` ("TRUE"/"FALSE")
# from synthpop -- exactly the purpose-dependent output type the flatten rule
# exists to eliminate. Restoring against the original column's type keeps the
# documented contract that logicals retain their type on both engines.
restore_logical_columns <- function(syn, original) {
  for (col_name in intersect(names(syn), names(original))) {
    if (!is.logical(original[[col_name]]) || is.logical(syn[[col_name]])) {
      next
    }
    values <- as.character(syn[[col_name]])
    if (!all(values %in% c("TRUE", "FALSE", NA))) {
      next
    }
    syn[[col_name]] <- as.logical(values)
  }
  syn
}
