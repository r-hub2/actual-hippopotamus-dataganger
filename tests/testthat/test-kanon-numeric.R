test_that("age as combination survives the full pipeline without crashing", {
  skip_if_not_installed("synthpop")

  df <- example_health_survey
  r <- detect_roles(df)
  r$identifies[r$variable %in% c("age", "sex", "province")] <- "combination"
  r$identifies[r$variable == "smoking_status"] <- "none"
  r$sensitive[r$variable == "smoking_status"] <- TRUE
  r <- dg_sync_roles_axes(r)

  spec <- synth_spec(roles = r, purpose = "development")
  spec$k_anon <- 5

  # run_synthesis_pipeline() captures warnings for the app UI instead of
  # emitting them; the infeasibility signal now arrives in res$warnings.
  res <- run_synthesis_pipeline(df, spec, roles = r)
  expect_match(paste(res$warnings, collapse = "\n"),
               "Could not apply k-anonymity", fixed = TRUE)

  expect_s3_class(res$synthetic, "data.frame")
  all_na <- vapply(res$synthetic, function(x) all(is.na(x)), logical(1))
  expect_false(
    any(all_na),
    info = paste("All-missing columns:", paste(names(all_na)[all_na], collapse = ", "))
  )
})

test_that("numeric quasi coarsening yields readable ranges", {
  x <- c(20, 27, 35, 42, 49, 56, 63, 70, NA_real_)
  out <- coarsen_qi_step(x, step = 1L)
  labels <- ifelse(is.na(out), "NA", as.character(out))
  out2 <- coarsen_qi_step(out, step = 2L)
  labels2 <- ifelse(is.na(out2), "NA", as.character(out2))

  expect_match(labels, "^\\[.*\\]$|^\\(.*\\]$|^NA$")
  expect_match(labels2, "^\\[.*\\]$|^\\(.*\\]$|^NA$")
  expect_false(all(is.na(out)), info = paste("Generalized output:", paste(out, collapse = ", ")))
  expect_false(
    any(labels2 %in% "(other)"),
    info = paste("Generalized labels:", paste(labels2, collapse = ", "))
  )
})
