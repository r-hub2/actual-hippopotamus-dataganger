
# Tests for postal code format registry and detection

test_that("registry returns all 10 countries with correct names", {
  reg <- dataganger:::dg_postal_format_registry()
  expect_length(reg, 10L)
  expect_named(reg, c("CA", "US", "UK", "AU", "DE", "FR", "JP", "IN", "BR", "NL"))
  expect_equal(reg$CA$name, "Canada")
  expect_equal(reg$US$name, "United States")
  expect_equal(reg$UK$name, "United Kingdom")
  expect_equal(reg$AU$name, "Australia")
  expect_equal(reg$DE$name, "Germany")
  expect_equal(reg$FR$name, "France")
  expect_equal(reg$JP$name, "Japan")
  expect_equal(reg$IN$name, "India")
  expect_equal(reg$BR$name, "Brazil")
  expect_equal(reg$NL$name, "Netherlands")
})

test_that("each registry entry has required fields", {
  reg <- dataganger:::dg_postal_format_registry()
  for (code in names(reg)) {
    entry <- reg[[code]]
    expect_true(all(c("country", "name", "regex", "template", "slots") %in% names(entry)),
      info = paste("Missing fields in", code))
    expect_equal(entry$country, code)
    expect_type(entry$slots, "list")
    expect_true(length(entry$slots) > 0L)
    for (slot in entry$slots) {
      expect_true(slot$type %in% c("letter", "digit", "literal"))
      expect_type(slot$chars, "character")
    }
  }
})

test_that("valid postal codes match their country regex", {
  reg <- dataganger:::dg_postal_format_registry()

  valid <- list(
    CA = c("K1A 0B1", "M5V 3L9", "H2X 1Y4"),
    US = c("10001", "90210", "60601"),
    UK = c("SW1A 1AA", "EC1A 1BB", "M1 1AE"),
    AU = c("2000", "3000", "4000"),
    DE = c("10115", "80331", "50667"),
    FR = c("75001", "69001", "13001"),
    JP = c("100-0001", "150-0002", "530-0001"),
    IN = c("110001", "400001", "600001"),
    BR = c("01310-100", "20040-020", "30130-010"),
    NL = c("1012 AB", "3011 CE", "2511 BK")
  )

  for (code in names(valid)) {
    for (val in valid[[code]]) {
      expect_true(grepl(reg[[code]]$regex, val),
        info = paste(val, "should match", code))
    }
  }
})

test_that("invalid postal codes do not match their country regex", {
  reg <- dataganger:::dg_postal_format_registry()

  expect_false(grepl(reg$CA$regex, "D1A 0B1"))
  expect_false(grepl(reg$CA$regex, "K1A 0B"))
  expect_false(grepl(reg$CA$regex, "12345"))

  expect_false(grepl(reg$US$regex, "1234"))
  expect_false(grepl(reg$US$regex, "123456"))
  expect_false(grepl(reg$US$regex, "ABCDE"))

  expect_false(grepl(reg$JP$regex, "1000001"))
  expect_false(grepl(reg$JP$regex, "10-0001"))

  expect_false(grepl(reg$NL$regex, "123 AB"))
  expect_false(grepl(reg$NL$regex, "1234 12"))
})

test_that("detect_postal_format with country_hint detects correct format", {
  detect <- dataganger:::detect_postal_format

  ca_vals <- c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  result <- detect(ca_vals, country_hint = "CA")
  expect_equal(result$country, "CA")

  us_vals <- c("10001", "90210", "60601", "30301", "85001")
  result <- detect(us_vals, country_hint = "US")
  expect_equal(result$country, "US")

  jp_vals <- c("100-0001", "150-0002", "530-0001", "460-0001", "810-0001")
  result <- detect(jp_vals, country_hint = "JP")
  expect_equal(result$country, "JP")

  nl_vals <- c("1012 AB", "3011 CE", "2511 BK", "5611 AA", "6511 DP")
  result <- detect(nl_vals, country_hint = "NL")
  expect_equal(result$country, "NL")
})

test_that("detect_postal_format without hint auto-detects CA", {
  detect <- dataganger:::detect_postal_format
  ca_vals <- c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  result <- detect(ca_vals)
  expect_equal(result$country, "CA")
})

test_that("detect_postal_format returns NULL for non-postal data", {
  detect <- dataganger:::detect_postal_format
  result <- detect(c("hello", "world", "foo", "bar", "baz"))
  expect_null(result)
})

test_that("detect_postal_format returns NULL when match rate below 90%", {
  detect <- dataganger:::detect_postal_format
  mixed <- c("K1A 0B1", "M5V 3L9", "hello", "world", "foo", "bar", "baz", "qux", "quux", "corge")
  result <- detect(mixed)
  expect_null(result)
})

test_that("ambiguous detection sets ambiguous attribute for 5-digit codes", {
  detect <- dataganger:::detect_postal_format
  vals <- c("10115", "80331", "50667", "10001", "90210")
  result <- detect(vals)
  expect_false(is.null(result))
  amb <- attr(result, "ambiguous")
  expect_true(
    all(c("US", "DE", "FR") %in% amb),
    info = paste("Ambiguous countries:", paste(amb, collapse = ", "))
  )
})

test_that("country_hint overrides ambiguity", {
  detect <- dataganger:::detect_postal_format
  vals <- c("10115", "80331", "50667", "10001", "90210")
  result <- detect(vals, country_hint = "DE")
  expect_equal(result$country, "DE")
  expect_null(attr(result, "ambiguous"))
})

test_that("NA and empty string handling still detects format", {
  detect <- dataganger:::detect_postal_format
  vals <- c("K1A 0B1", NA, "", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  result <- detect(vals)
  expect_equal(result$country, "CA")
})
