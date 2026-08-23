test_that("engine output is identical for the UI path and the CLI --roles path", {
  skip_if_no_synthpop()
  set.seed(0)
  df <- data.frame(
    age = sample(20:80, 80, TRUE),
    grp = sample(c("a", "b", "c"), 80, TRUE),
    stringsAsFactors = FALSE
  )
  roles <- dg_sync_roles_axes(detect_roles(df))
  spec <- synth_spec(purpose = "development", n = 80, seed = 7L)

  ui_syn <- synthesize_data(df, spec, roles = roles)

  tmp <- withr::local_tempfile(fileext = ".yaml")
  cli_write_yaml(roles_to_yaml_list(roles), tmp)
  cli_roles <- cli_read_roles_yaml(tmp, df)
  cli_syn <- synthesize_data(df, spec, roles = cli_roles)

  expect_equal(as.data.frame(ui_syn), as.data.frame(cli_syn))
})
