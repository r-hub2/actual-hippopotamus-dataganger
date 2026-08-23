test_that("roles survive a YAML round-trip with both axes and actions", {
  df <- data.frame(age = 1:5, name = letters[1:5], stringsAsFactors = FALSE)
  roles <- detect_roles(df)
  roles$identifies[roles$variable == "age"]  <- "combination"
  roles$identifies[roles$variable == "name"] <- "none"
  roles$sensitive[roles$variable == "age"]   <- TRUE
  roles <- dg_sync_roles_axes(roles)

  tmp <- withr::local_tempfile(fileext = ".yaml")
  cli_write_yaml(roles_to_yaml_list(roles), tmp)
  rt <- cli_read_roles_yaml(tmp, df)

  for (col in c("variable", "identifies", "sensitive", "simulation")) {
    expect_equal(rt[[col]], roles[[col]], info = col)
  }
})

test_that("cli_read_roles_yaml preserves disclosure_role when axes are omitted", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  df <- data.frame(age = 1:10)
  yaml::write_yaml(list(roles = list(list(variable = "age", disclosure_role = "quasi"))), tmp)

  roles <- cli_read_roles_yaml(tmp, df)

  expect_equal(roles$disclosure_role[roles$variable == "age"], "quasi")
  expect_equal(roles$identifies[roles$variable == "age"], "combination")
})

test_that("postal_strategy and postal_country survive recipe YAML roundtrip", {
  df <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4", "V6B 3K9", "T2P 1J9"),
    x = 1:5,
    stringsAsFactors = FALSE
  )
  roles <- dataganger::detect_roles(df)
  roles$postal_strategy[roles$variable == "postal_code"] <- "resample"
  roles$postal_country[roles$variable == "postal_code"] <- "CA"

  yaml_list <- dataganger:::roles_to_yaml_list(roles)
  postal_entry <- yaml_list[[which(vapply(yaml_list, function(e) identical(e$variable, "postal_code"), logical(1)))]]
  expect_equal(postal_entry$postal_strategy, "resample")
  expect_equal(postal_entry$postal_country, "CA")

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  yaml::write_yaml(list(roles = yaml_list), tmp)
  restored <- dataganger:::cli_read_roles_yaml(tmp, df)
  expect_equal(restored$postal_strategy[restored$variable == "postal_code"], "resample")
  expect_equal(restored$postal_country[restored$variable == "postal_code"], "CA")
})
