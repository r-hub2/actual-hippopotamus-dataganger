## ----kanon-worked-examples----------------------------------------------------
library(dataganger)
library(knitr)

# Everything below uses only exported DataGangeR functions, so you can run it
# yourself. A "roles" frame just needs `variable` and `disclosure_role`
# ("quasi" marks a quasi-identifier, "direct" maps to Drop unless an explicit
# action override is supplied, and "none" is left alone).
qi_roles <- function(data, qi_cols, drop_cols = "id") {
  data.frame(
    variable = names(data),
    disclosure_role = ifelse(
      names(data) %in% qi_cols, "quasi",
      ifelse(names(data) %in% drop_cols, "direct", "none")
    ),
    stringsAsFactors = FALSE
  )
}

# Probe one configuration: returns the kanon outcome (infeasible? how many
# quasi-identifier cells had to be suppressed?) without printing warnings.
probe_kanon <- function(data, qi_cols, k) {
  out <- withCallingHandlers(
    enforce_kanon(data, qi_roles(data, qi_cols), k = k),
    warning = function(w) invokeRestart("muffleWarning")
  )
  attr(out, "kanon", exact = TRUE)
}

individual_qi <- c("age", "sex", "education", "smoker")

# Escape route 1: walk k down from 5 to the largest value that works
# (never below 3 - smaller values give close to no protection).
feasible_k <- NULL
for (k_try in 5:3) {
  info <- probe_kanon(individual_sample, individual_qi, k_try)
  if (!isTRUE(info$infeasible)) {
    feasible_k <- k_try
    feasible_k_info <- info
    break
  }
}

# Escape route 2: generate more synthetic rows, so each quasi-identifier
# combination is shared by more rows. The engine is pinned so the numbers
# do not depend on which synthesis packages are installed.
more_rows_spec <- synth_spec(
  purpose = "development", n = 1000, seed = 1, engine = "internal"
)
more_rows <- withCallingHandlers(
  synthesize_data(individual_sample, more_rows_spec,
                  roles = qi_roles(individual_sample, individual_qi)),
  warning = function(w) invokeRestart("muffleWarning")
)
more_rows_info <- attr(more_rows, "kanon", exact = TRUE)

# Escape route 3: shrink the quasi-identifier set (age drives the sparsity:
# it has the most distinct values of the four).
drop_age_info <- probe_kanon(
  individual_sample, c("sex", "education", "smoker"), 5
)

suppressed_text <- function(info) {
  n <- info$suppressed_cells
  sprintf("%d suppressed QI cells", if (is.null(n)) 0L else n)
}

individual_table <- data.frame(
  option = c(
    "Keep age + sex + education + smoker at k = 5",
    sprintf("Apply k = %d", feasible_k),
    "Generate 1000 rows at k = 5",
    "Mark age as not part of the QI set"
  ),
  feasible_at_k = c(
    "No",
    sprintf("Yes (k = %d)", feasible_k),
    if (isTRUE(more_rows_info$infeasible)) "No" else "Yes (k = 5)",
    if (isTRUE(drop_age_info$infeasible)) "No" else "Yes (k = 5)"
  ),
  cost = c(
    "No k-anonymity protection applied",
    suppressed_text(feasible_k_info),
    suppressed_text(more_rows_info),
    suppressed_text(drop_age_info)
  ),
  stringsAsFactors = FALSE
)

# The temporal and geographic samples are aggregate-shaped (site-day records
# and region summaries, not people), so no column is marked as identifying
# in combination and the k-anonymity step never engages.
temporal_table <- data.frame(
  option = "Keep the combination question at none for all columns",
  feasible_at_k = "Not applicable",
  cost = sprintf(
    "No k-anonymity step; %d QI columns selected because rows are site-day records, not people",
    sum(qi_roles(temporal_sample, character(0))$disclosure_role == "quasi")
  ),
  stringsAsFactors = FALSE
)

geographic_table <- data.frame(
  option = "Keep the combination question at none for all columns",
  feasible_at_k = "Not applicable",
  cost = sprintf(
    "No k-anonymity step; %d QI columns selected because rows are region summaries, not people",
    sum(qi_roles(geographic_sample, character(0))$disclosure_role == "quasi")
  ),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
kable(individual_table, col.names = c("Option", "Feasible at k", "Cost"))

## -----------------------------------------------------------------------------
kable(temporal_table, col.names = c("Option", "Feasible at k", "Cost"))

## -----------------------------------------------------------------------------
kable(geographic_table, col.names = c("Option", "Feasible at k", "Cost"))

