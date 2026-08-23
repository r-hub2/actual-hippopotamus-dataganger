
test_that("portable English month-name dates are detected", {
  variants <- list(
    english_abbrev = "Jun 8, 2019",
    english_full = "June 8, 2019",
    lowercase = "june 8, 2019",
    dotted = "jun. 8, 2019"
  )
  for (label in names(variants)) {
    value <- variants[[label]]
    df <- data.frame(
      when = rep(value, 10),
      stringsAsFactors = FALSE
    )
    roles <- detect_roles(df)
    expect_equal(
      roles$recommended_role[roles$variable == "when"], "date",
      info = paste0(label, ": ", value)
    )
  }
})

test_that("foreign month text unsupported by LC_TIME does not receive a date role", {
  withr::local_locale(c(LC_TIME = "C"))
  variants <- c("juin 8, 2019", "d\u00e9c 8, 2019")
  for (value in variants) {
    roles <- detect_roles(data.frame(
      when = rep(value, 10),
      stringsAsFactors = FALSE
    ))
    expect_false(
      identical(roles$recommended_role[[1]], "date"),
      info = paste("Unsupported month text received date role under",
                   Sys.getlocale("LC_TIME"), ":", value)
    )
  }
})

test_that("detect_roles() returns correct S3 class and columns", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  r <- detect_roles(df)
  expect_s3_class(r, "dataganger_roles")
  expect_named(
    r,
    c(
      "variable", "class", "recommended_role", "user_role", "simulation",
      "postal_strategy", "postal_country", "label_strategy",
      "reason", "identifies", "sensitive", "disclosure_role", "disclosure_reason",
      "user_identifies", "user_sensitive"
    )
  )
  expect_equal(r$variable, c("x", "y"))
  expect_equal(r$simulation, c("synthesize", "synthesize"))
})

test_that("detect_roles assigns disclosure_role per the conservative policy", {
  set.seed(123)
  df <- data.frame(
    patient_id = sprintf("P%04d", 1:50),
    zip = rep(c("M5V", "M4C"), 25),
    visit_date = as.Date("2020-01-01") + 0:49,
    sex = rep(c("F", "M"), 25),
    lab_value = rnorm(50),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(df)
  expect_true("disclosure_role" %in% names(roles))
  expect_true("disclosure_reason" %in% names(roles))
  dr <- stats::setNames(roles$disclosure_role, roles$variable)
  expect_equal(dr[["patient_id"]], "direct")
  expect_equal(dr[["zip"]], "quasi")
  expect_equal(dr[["visit_date"]], "quasi")
  expect_true(is.na(dr[["sex"]]))
  expect_equal(dr[["lab_value"]], "none")
})

test_that("detect_roles() detects ID candidate from high cardinality", {
  set.seed(123)
  df <- data.frame(token = sprintf("tok%03d", 1:50))
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "token"], "alphanumeric ID")
  expect_match(r$reason[r$variable == "token"], "Nearly every value is unique")
})

test_that("detect_roles() does NOT flag high-cardinality as ID when nrow < 20", {
  # Column named "col_x" to avoid triggering ID name pattern
  df <- data.frame(col_x = 1:10)
  r <- detect_roles(df)
  expect_false(r$recommended_role[r$variable == "col_x"] == "alphanumeric ID")
})

test_that("detect_roles() labels distinctive numeric as numeric, not ID candidate", {
  # High-cardinality numeric with no ID-like name: users classify it in the UI,
  # so it must NOT be auto-flagged as an identifier (design intent).
  df <- data.frame(measurement = seq(1.1, 50.1, length.out = 50))
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "numeric")
  expect_false(r$recommended_role[1] == "alphanumeric ID")
})

test_that("detect_roles() still flags distinctive numeric as ID when name matches", {
  df <- data.frame(record_id = seq(1.1, 50.1, length.out = 50))
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
  expect_match(r$reason[1], "suggests an identifier")
})

test_that("detect_roles() still flags distinctive character as ID candidate", {
  df <- data.frame(token = sprintf("tok%03d", 1:50))
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
})

test_that("detect_roles() detects ID from column name pattern", {
  # Use data that does NOT trigger the cardinality-based ID check
  # n_distinct=3, nrow=25 -> ratio 0.12 < 0.95, so only name triggers ID
  df <- data.frame(patient_id = rep(1:3, length.out = 25))
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
  expect_match(r$reason[1], "suggests an identifier")
})

test_that("detect_roles() detects multiple ID name patterns", {
  patterns <- c("id", "record_id", "subject", "patient", "record", "case_no")
  for (nm in patterns) {
    df <- setNames(data.frame(x = rep(1:3, length.out = 25)), nm)
    r <- detect_roles(df)
    expect_equal(r$recommended_role[1], "alphanumeric ID", info = nm)
  }
})

test_that("detect_roles() detects date columns", {
  df <- data.frame(
    d1 = as.Date("2024-01-01") + 1:5,
    d2 = as.POSIXct("2024-01-01 12:00:00") + 1:5
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "d1"], "date")
  expect_equal(r$recommended_role[r$variable == "d2"], "date")
})

test_that("detect_roles() detects haven_labelled columns", {
  df <- data.frame(
    status = haven::labelled(
      c(1, 2, 1, 2, 1),
      labels = c(Active = 1, Inactive = 2)
    ),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "label_check")
  expect_match(r$reason[1], "labelled survey codes")
})

test_that("detect_roles() detects categorical candidate (low cardinality ratio)", {
  df <- data.frame(
    small = rep(letters[1:3], length.out = 50),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "small"], "categorical candidate")
})

test_that("detect_roles() detects categorical candidate (n_distinct <= 20)", {
  df <- data.frame(
    x = factor(rep(letters[1:15], each = 10)),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "categorical candidate")
})

test_that("detect_roles() detects free text", {
  # 60 unique long strings, each > 50 characters
  # 100 rows -> n_distinct=60 > nrow*0.5=50, mean_nchar > 50
  # ratio 0.6 < 0.95, n_distinct=60 > 20 -> not ID, not categorical
  base_strings <- sprintf(
    "very_long_unique_text_for_testing_free_text_number_%030d",
    1:60
  )
  notes <- c(base_strings, sample(base_strings, 40, replace = TRUE))
  df <- data.frame(notes = notes, stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "free text")
})

test_that("detect_roles() classifies long narrative text as free text before ID", {
  df <- data.frame(
    note_id = sprintf(
      "patient follow up note describing symptoms, medications, and context %03d",
      1:30
    ),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "free text")
})

test_that("detect_roles() classifies geographic names by cardinality, not a special role", {
  low_card <- data.frame(
    region = rep(c("north", "south", "east", "west", "central"), each = 6),
    stringsAsFactors = FALSE
  )
  low_roles <- detect_roles(low_card)
  expect_equal(low_roles$recommended_role[1], "categorical candidate")

  high_card <- data.frame(
    region = sprintf("region%03d", 1:50),
    stringsAsFactors = FALSE
  )
  high_roles <- detect_roles(high_card)
  expect_equal(high_roles$recommended_role[1], "alphanumeric ID")
})

test_that("detect_roles() labels a distinctive numeric column as numeric", {
  # 50 rows, exactly 30 distinct values -> n_distinct=30
  # ratio 30/50=0.6, >=0.05 and <0.95 AND n_distinct=30 > 20
  # Not ID (ratio < 0.95), not categorical (ratio >= 0.05 AND > 20), numeric class
  # Name "normal_col" matches no patterns -> numeric (user classifies via UI)
  df <- data.frame(
    normal_col = c(1:30, 1:20),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "numeric")
})

test_that("detect_roles maps confident identifiers to direct, leaves rest unselected", {
  df <- data.frame(
    record_id  = rep(1:3, length.out = 50),
    visit_date = as.Date("2024-01-01") + 1:50,
    city_name  = sample(30:59, 50, replace = TRUE),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$disclosure_role[r$variable == "record_id"], "direct")
  expect_equal(r$disclosure_role[r$variable == "visit_date"], "quasi")
  expect_equal(r$disclosure_role[r$variable == "city_name"], "none")
})

test_that("detect_roles populates identifies/sensitive and a consistent disclosure_role", {
  r <- detect_roles(example_health_survey)
  expect_true(
    all(c("identifies", "sensitive", "disclosure_role") %in% names(r)),
    info = paste("Role columns:", paste(names(r), collapse = ", "))
  )
  expect_equal(r$identifies[r$variable == "record_id"], "direct")
  derived <- vapply(
    seq_len(nrow(r)),
    function(i) dg_axes_to_role(r$identifies[i], r$sensitive[i]),
    character(1)
  )
  expect_equal(r$disclosure_role, derived)
})

test_that("CLI disclosure override back-fills axes", {
  r <- detect_roles(example_health_survey)
  r2 <- apply_disclosure_overrides(r, list(bmi = "sensitive"))
  expect_equal(r2$identifies[r2$variable == "bmi"], "none")
  expect_true(isTRUE_vec(r2$sensitive[r2$variable == "bmi"]))
  expect_equal(r2$disclosure_role[r2$variable == "bmi"], "sensitive")
})

test_that("detect_roles() user_role is initially NA", {
  df <- data.frame(x = 1:5)
  r <- detect_roles(df)
  expect_true(is.na(r$user_role[1]))
})

test_that("detect_roles() accepts a profile argument and reuses it", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  p <- profile_data(df)
  r <- detect_roles(df, profile = p)
  expect_s3_class(r, "dataganger_roles")
  expect_equal(nrow(r), 2)
})

test_that("print.dataganger_roles works without error", {
  df <- data.frame(x = rep(1:3, length.out = 25))
  r <- detect_roles(df)
  expect_s3_class(r, "dataganger_roles")
  expect_no_error(print(r))
})

test_that("detect_roles() does not classify long character values as ID even at high cardinality", {
  # 50 unique strings each ~30 chars, no spaces -- high cardinality but long values
  vals <- sprintf("item-description-key-value-%03d", 1:50)
  df <- data.frame(item_desc = vals, stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_false(r$recommended_role[1] == "alphanumeric ID",
               label = "long char column should not be classified as ID candidate")
})

test_that("detect_roles() rejects non-data-frame", {
  expect_error(detect_roles("not a df"), "must be a data frame")
})

# Regression: Bug 5 -- character-stored dates were classified "unknown" and fed
# to synthpop as high-cardinality factors, hanging CART.
test_that("detect_roles() classifies ISO date strings as 'date'", {
  df <- data.frame(
    event_date = format(as.Date("2020-01-01") + 1:50, "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "event_date"], "date")
})

test_that("a character-stored date gets the same quasi-identifier default as a native Date column", {
  df <- data.frame(
    native = as.Date("2020-01-01") + 1:50,
    text   = format(as.Date("2020-01-01") + 1:50, "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$disclosure_role[r$variable == "native"], "quasi")
  expect_equal(r$disclosure_role[r$variable == "text"], "quasi")
  expect_equal(r$identifies[r$variable == "native"], "combination")
  expect_equal(r$identifies[r$variable == "text"], "combination")
})

test_that("detect_roles() classifies 'Month DD, YYYY' date strings as 'date'", {
  withr::local_locale(c(LC_TIME = "C"))
  df <- data.frame(
    report_date = format(as.Date("2019-06-01") + 1:50, "%b %e, %Y"),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "report_date"], "date")
})

test_that("detect_roles() does not classify non-date strings as 'date'", {
  df <- data.frame(
    code = sprintf("CASE-%04d", 1:50),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_false(r$recommended_role[1] == "date",
               label = "arbitrary code strings should not be classified as date")
})

test_that("detect_roles() classifies a bare time-of-day column (no date part) as 'date'", {
  df <- data.frame(
    check_in = sprintf("%02d:%02d", sample(6:20, 50, TRUE), sample(0:59, 50, TRUE)),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "check_in"], "date")
})

test_that("detect_roles() classifies a datetime-with-AM/PM-time string as 'date'", {
  df <- data.frame(
    visit = sprintf("%02d/%02d/2020 %02d:%02d %s",
      sample(1:12, 50, TRUE), sample(1:28, 50, TRUE),
      sample(1:12, 50, TRUE), sample(0:59, 50, TRUE),
      sample(c("AM", "PM"), 50, TRUE)
    ),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[r$variable == "visit"], "date")
})

test_that("detect_roles leaves uncertain columns unselected (NA disclosure_role)", {
  set.seed(1)
  df <- data.frame(
    patient_id = sprintf("P%04d", 1:50),
    zip        = rep(c("M5V", "M4C"), 25),
    visit_date = as.Date("2020-01-01") + 0:49,
    sex        = rep(c("F", "M"), 25),
    case_count = sample(1:9, 50, TRUE),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  dr <- stats::setNames(r$disclosure_role, r$variable)
  expect_equal(dr[["patient_id"]], "direct")
  expect_equal(dr[["zip"]], "quasi")
  expect_equal(dr[["visit_date"]], "quasi")
  expect_true(is.na(dr[["sex"]]))
  expect_equal(dr[["case_count"]], "none")
})

test_that("detect_roles auto-assigns sensitive for known-sensitive names", {
  df <- data.frame(
    diagnosis = rep(c("flu", "cold"), 25),
    race      = rep(c("a", "b"), 25),
    income    = rnorm(50),
    treatment = rep(c("x", "y"), 25),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  ids <- stats::setNames(r$identifies, r$variable)
  sens <- stats::setNames(r$sensitive, r$variable)
  dr <- stats::setNames(r$disclosure_role, r$variable)
  expect_true(sens[["diagnosis"]])
  expect_true(sens[["race"]])
  expect_true(sens[["income"]])
  expect_equal(ids[["diagnosis"]], "")
  expect_equal(ids[["race"]], "")
  expect_equal(ids[["income"]], "none")
  expect_true(is.na(dr[["diagnosis"]]))
  expect_true(is.na(dr[["race"]]))
  expect_equal(dr[["income"]], "sensitive")
  expect_true(is.na(dr[["treatment"]]))
})

test_that("detect_roles sensitive-name heuristic rejects near-miss false positives", {
  df <- data.frame(
    trace_element      = rnorm(50),
    sewage_volume      = rnorm(50),
    unconditional_prob = rnorm(50),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  dr <- stats::setNames(r$disclosure_role, r$variable)
  expect_equal(dr[["trace_element"]], "none")
  expect_equal(dr[["sewage_volume"]], "none")
  expect_equal(dr[["unconditional_prob"]], "none")
})

test_that("detect_roles leaves geographic categorical columns unselected for disclosure", {
  df <- data.frame(city = rep(c("Toronto", "Montreal"), 25), stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "categorical candidate")
  expect_true(is.na(r$disclosure_role[1]))
})

test_that("detect_roles classifies a delimited letter+digit column as alphanumeric ID", {
  set.seed(1)
  df <- data.frame(
    order_id = sprintf("OR-%04d-%02d", 1:30, sample(1:99, 30, TRUE)),
    stringsAsFactors = FALSE
  )
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
  expect_equal(r$identifies[1], "direct")
  expect_equal(r$disclosure_role[1], "direct")
  expect_equal(r$simulation[1], "scramble")
})

test_that("detect_roles treats a fixed-prefix reference number as alphanumeric ID too", {
  # A constant letter prefix plus a sequence is still a structured
  # letter+digit+delimiter pattern (e.g. an invoice number).
  df <- data.frame(invoice = sprintf("INV-%06d", 1:30), stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
})

test_that("detect_roles classifies a plain letter-prefixed ID (no delimiter) as alphanumeric ID", {
  df <- data.frame(token = sprintf("tok%03d", 1:30), stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_equal(r$recommended_role[1], "alphanumeric ID")
})

test_that("detect_roles does not classify a small set of alphanumeric category codes as an ID", {
  # Only 3 distinct codes repeated many times -- not identifying.
  df <- data.frame(plan = rep(c("A-1", "B-2", "C-3"), each = 10), stringsAsFactors = FALSE)
  r <- detect_roles(df)
  expect_false(identical(r$recommended_role[1], "alphanumeric ID"))
})

test_that("scramble_alphanumeric_id preserves delimiter positions and never leaks the original value", {
  set.seed(1)
  x <- sprintf("OR-%04d-%02d", 1:50, sample(1:99, 50, TRUE))
  scrambled <- dataganger:::scramble_alphanumeric_id(x)

  expect_equal(length(scrambled), length(x))
  expect_match(scrambled, "^..-....-..$")
  expect_false(
    any(scrambled == x),
    info = paste("Unchanged rows:", paste(which(scrambled == x), collapse = ", "))
  )
  # Same multiset of non-delimiter characters per value (a permutation, not new data)
  strip <- function(v) sort(strsplit(gsub("-", "", v), "")[[1]])
  same_chars <- vapply(seq_along(x), function(i) identical(strip(x[[i]]), strip(scrambled[[i]])), logical(1))
  expect_true(
    all(same_chars),
    info = paste("Rows with changed character multisets:",
                 paste(which(!same_chars), collapse = ", "))
  )
})

test_that("scramble_alphanumeric_id passes through NA and empty strings unchanged", {
  out <- dataganger:::scramble_alphanumeric_id(c("AB-123", NA, ""))
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "")
  expect_false(is.na(out[[1]]))
})

test_that("scramble_alphanumeric_id de-identifies short numeric IDs (no value survives in place)", {
  set.seed(1)
  # A sequential integer ID: single digits and repeated digits cannot be
  # de-identified by character reordering alone (1..9 have nothing to permute,
  # 11/22/... permute to themselves), so a plain permutation leaves the
  # original value in place -- a re-identification leak. The fix must change
  # every value.
  x <- as.character(1:200)
  scrambled <- dataganger:::scramble_alphanumeric_id(x)

  expect_equal(length(scrambled), length(x))
  expect_false(
    any(scrambled == x),
    info = paste("Unchanged rows:", paste(which(scrambled == x), collapse = ", "))
  ) # no value survives in its own row
  expect_equal(nchar(scrambled), nchar(x))   # width/format preserved
})

test_that("scramble_alphanumeric_id de-identifies repeated-digit values in place", {
  set.seed(2)
  x <- c("5", "11", "222", "77", "9")
  scrambled <- dataganger:::scramble_alphanumeric_id(x)
  expect_false(
    any(scrambled == x),
    info = paste("Unchanged rows:", paste(which(scrambled == x), collapse = ", "))
  )
  expect_match(scrambled, "^[0-9]+$")
  expect_equal(nchar(scrambled), nchar(x))
})

test_that("scramble_alphanumeric_id never emits any observed value", {
  # Permutation preserves the character multiset, so one row's scramble can
  # land on a DIFFERENT row's identifier ("T0010" -> "T0001"); the output
  # must avoid the entire observed set, not only the value being scrambled.
  x <- sprintf("T%04d", 1:60)
  for (seed in c(7L, 42L, 20260804L)) {
    set.seed(seed)
    scrambled <- dataganger:::scramble_alphanumeric_id(x)
    expect_length(intersect(x, scrambled), 0L)
    expect_equal(nchar(scrambled), nchar(x))
  }
})

test_that("print.dataganger_roles handles subset objects without required columns", {
  roles <- detect_roles(data.frame(age = 1:5, city = letters[1:5]))

  expect_no_error(print(roles[, c("variable", "identifies")]))
})

test_that("postal code columns detected by name", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9"),
    zip = c("10001", "90210", "60601", "30301", "85001"),
    PLZ = c("10115", "80331", "50667", "60311", "20095"),
    name = c("Alice", "Bob", "Carol", "Dave", "Eve"),
    stringsAsFactors = FALSE
  )
  roles <- dataganger::detect_roles(df)
  expect_equal(roles$recommended_role[roles$variable == "postal_code"], "postal code")
  expect_equal(roles$recommended_role[roles$variable == "zip"], "postal code")
  expect_equal(roles$recommended_role[roles$variable == "PLZ"], "postal code")
  expect_equal(roles$recommended_role[roles$variable == "name"], "categorical candidate")
})

test_that("postal code columns get quasi disclosure role", {
  df <- data.frame(
    postcode = c("SW1A 1AA", "EC1A 1BB", "M1 1AE", "LS1 4AP", "B1 1BB"),
    stringsAsFactors = FALSE
  )
  roles <- dataganger::detect_roles(df)
  expect_equal(roles$disclosure_role[roles$variable == "postcode"], "quasi")
})

test_that("postal code columns get postal_strategy and postal_country", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9"),
    stringsAsFactors = FALSE
  )
  roles <- dataganger::detect_roles(df)
  expect_true("postal_strategy" %in% names(roles))
  expect_true("postal_country" %in% names(roles))
  expect_equal(roles$postal_strategy[roles$variable == "postal_code"], "generate")
  expect_true(is.na(roles$postal_country[roles$variable == "postal_code"]))
})

test_that("non-postal columns have NA postal_strategy", {
  df <- data.frame(
    x = 1:10,
    y = letters[1:10],
    stringsAsFactors = FALSE
  )
  roles <- dataganger::detect_roles(df)
  expect_true(
    all(is.na(roles$postal_strategy)),
    info = paste("Unexpected postal strategies:",
                 paste(stats::na.omit(roles$postal_strategy), collapse = ", "))
  )
  expect_true(
    all(is.na(roles$postal_country)),
    info = paste("Unexpected postal countries:",
                 paste(stats::na.omit(roles$postal_country), collapse = ", "))
  )
})
