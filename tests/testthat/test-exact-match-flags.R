test_that("exact_row_match_flags flags the matching original and synthetic rows", {
  # Fixed-width string keys so the test exercises match logic, not numeric
  # formatting (row_key coerces via apply()/as.matrix, which pads numbers).
  original <- data.frame(
    a = sprintf("%02d", 1:30), b = rep(c("x", "y"), 15), stringsAsFactors = FALSE
  )
  synthetic <- data.frame(
    a = sprintf("%02d", 31:60), b = rep(c("y", "x"), 15), stringsAsFactors = FALSE
  )
  synthetic[c(3, 7), ] <- original[c(3, 7), ]  # inject two exact copies

  fl <- dataganger:::exact_row_match_flags(original, synthetic)

  expect_length(fl$original, nrow(original))
  expect_length(fl$synthetic, nrow(synthetic))
  expect_true(
    all(fl$synthetic[c(3, 7)]),
    info = paste("Synthetic flags:", paste(fl$synthetic, collapse = ", "))
  )
  expect_equal(sum(fl$synthetic), 2L)
  expect_true(
    all(fl$original[c(3, 7)]),
    info = paste("Original flags:", paste(fl$original, collapse = ", "))
  )
  expect_equal(sum(fl$original), 2L)
  # The synthetic-row flag count must equal the stat-box count exactly.
  expect_equal(sum(fl$synthetic), dataganger:::exact_row_match_count(original, synthetic))
})

test_that("exact_row_match_flags returns all-FALSE below 20 rows or with no synthetic", {
  small <- data.frame(a = 1:10)
  fl <- dataganger:::exact_row_match_flags(small, small)
  expect_false(any(fl$original), info = paste("Original flags:", paste(fl$original, collapse = ", ")))
  expect_false(any(fl$synthetic), info = paste("Synthetic flags:", paste(fl$synthetic, collapse = ", ")))

  fl2 <- dataganger:::exact_row_match_flags(
    data.frame(a = 1:30), data.frame(a = integer(0))
  )
  expect_false(any(fl2$original), info = paste("Original flags:", paste(fl2$original, collapse = ", ")))
  expect_length(fl2$synthetic, 0L)
})

test_that("exact_row_match_flags excludes alphanumeric-ID columns from matching", {
  original <- data.frame(
    id = sprintf("P%04d", 1:30), v = rep(1:2, 15), stringsAsFactors = FALSE
  )
  synthetic <- original
  synthetic$id <- sprintf("Q%04d", 1:30)  # IDs differ, but v is identical
  role_map <- c(id = "alphanumeric ID", v = "numeric")

  fl <- dataganger:::exact_row_match_flags(original, synthetic, role_map)
  # Every row matches on v once the ID column is excluded -- and this equals
  # the count, keeping highlight and stat box consistent.
  expect_true(all(fl$synthetic), info = paste("Synthetic flags:", paste(fl$synthetic, collapse = ", ")))
  expect_equal(sum(fl$synthetic), dataganger:::exact_row_match_count(original, synthetic, role_map))
})
