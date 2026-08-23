test_that("suspected_direct_identifiers flags direct/ID/free-text columns with reasons", {
  df <- data.frame(
    email = paste0(letters[1:20], "@x.com"),
    mrn = sprintf("MRN%04d", 1:20),
    age = 40L + seq_len(20),
    stringsAsFactors = FALSE
  )
  roles <- dg_sync_roles_axes(detect_roles(df))
  flagged <- suspected_direct_identifiers(roles)

  expect_true(is.data.frame(flagged))
  expect_true(
    all(c("variable", "reason") %in% names(flagged)),
    info = paste("Flag columns:", paste(names(flagged), collapse = ", "))
  )
  expect_true("email" %in% flagged$variable || "mrn" %in% flagged$variable)
  expect_false("age" %in% flagged$variable)
})
