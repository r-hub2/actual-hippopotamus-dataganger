
# Tests for postal code synthesis functions

test_that("synth_postal_code_generate produces correct length output", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  out <- withr::with_seed(1, gen(x, 50L, reg$CA))
  expect_length(out, 50L)
  expect_type(out, "character")
})

test_that("generated CA postal codes all match the CA regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  out <- withr::with_seed(1, gen(x, 100L, reg$CA, missing_strategy = "none"))
  expect_match(out, reg$CA$regex)
})

test_that("generated US postal codes all match the US regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("10001", "90210", "60601", "30301", "85001")
  out <- withr::with_seed(1, gen(x, 100L, reg$US, missing_strategy = "none"))
  expect_match(out, reg$US$regex)
})

test_that("generated JP postal codes all match the JP regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("100-0001", "150-0002", "530-0001", "460-0001", "810-0001")
  out <- withr::with_seed(1, gen(x, 100L, reg$JP, missing_strategy = "none"))
  expect_match(out, reg$JP$regex)
})

test_that("generated NL postal codes all match the NL regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("1012 AB", "3011 CE", "2511 BK", "5611 AA", "6511 DP")
  out <- withr::with_seed(1, gen(x, 100L, reg$NL, missing_strategy = "none"))
  expect_match(out, reg$NL$regex)
})

test_that("generated UK postal codes all match the UK regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("SW1A 1AA", "EC1A 1BB", "M1 1AE", "LS1 4AP", "B1 1BB")
  out <- withr::with_seed(1, gen(x, 100L, reg$UK, missing_strategy = "none"))
  expect_match(out, reg$UK$regex)
})

test_that("generated BR postal codes all match the BR regex", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("01310-100", "20040-020", "30130-010", "40010-000", "50010-000")
  out <- withr::with_seed(1, gen(x, 100L, reg$BR, missing_strategy = "none"))
  expect_match(out, reg$BR$regex)
})

test_that("zero source-value leakage in generate", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("K1A 0B1", "M5V 3L9")
  out <- withr::with_seed(42, gen(x, 200L, reg$CA, missing_strategy = "none"))
  expect_false(
    any(out %in% c("K1A 0B1", "M5V 3L9")),
    info = paste("Leaked source values:",
                 paste(intersect(out, c("K1A 0B1", "M5V 3L9")), collapse = ", "))
  )
})

test_that("synth_postal_code_resample only produces values from input set", {
  resample <- dataganger:::synth_postal_code_resample
  x <- c("10001", "90210", "60601")
  out <- withr::with_seed(1, resample(x, 100L, missing_strategy = "none"))
  expect_true(
    all(out %in% x),
    info = paste("Unexpected resampled values:", paste(setdiff(out, x), collapse = ", "))
  )
})

test_that("synth_postal_code_resample produces correct length", {
  resample <- dataganger:::synth_postal_code_resample
  x <- c("10001", "90210", "60601")
  out <- withr::with_seed(1, resample(x, 75L, missing_strategy = "none"))
  expect_length(out, 75L)
})

test_that("seeded determinism for generate", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  x <- c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9")
  out1 <- withr::with_seed(42, gen(x, 50L, reg$CA, missing_strategy = "none"))
  out2 <- withr::with_seed(42, gen(x, 50L, reg$CA, missing_strategy = "none"))
  expect_identical(out1, out2)
})

test_that("seeded determinism for resample", {
  resample <- dataganger:::synth_postal_code_resample
  x <- c("10001", "90210", "60601")
  out1 <- withr::with_seed(42, resample(x, 50L, missing_strategy = "none"))
  out2 <- withr::with_seed(42, resample(x, 50L, missing_strategy = "none"))
  expect_identical(out1, out2)
})

test_that("missingness produces NAs when input has NAs", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  resample <- dataganger:::synth_postal_code_resample

  x <- c("K1A 0B1", NA, "M5V 3L9", NA, "H2X 1Y4", NA, "V6B 3K9", NA, "T2P 1J9", NA)
  out_gen <- withr::with_seed(1, gen(x, 100L, reg$CA, missing_strategy = "approx"))
  expect_true(
    any(is.na(out_gen)),
    info = paste("Generated missing count:", sum(is.na(out_gen)))
  )

  out_res <- withr::with_seed(1, resample(x, 100L, missing_strategy = "approx"))
  expect_true(
    any(is.na(out_res)),
    info = paste("Resampled missing count:", sum(is.na(out_res)))
  )
})

test_that("all-NA input returns all NA character", {
  reg <- dataganger:::dg_postal_format_registry()
  gen <- dataganger:::synth_postal_code_generate
  resample <- dataganger:::synth_postal_code_resample

  x <- rep(NA_character_, 10L)
  out_gen <- withr::with_seed(1, gen(x, 20L, reg$CA, missing_strategy = "approx"))
  expect_true(
    all(is.na(out_gen)),
    info = paste("Generated non-missing rows:", paste(which(!is.na(out_gen)), collapse = ", "))
  )
  expect_type(out_gen, "character")

  out_res <- withr::with_seed(1, resample(x, 20L, missing_strategy = "approx"))
  expect_true(
    all(is.na(out_res)),
    info = paste("Resampled non-missing rows:", paste(which(!is.na(out_res)), collapse = ", "))
  )
  expect_type(out_res, "character")
})
