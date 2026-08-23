test_that("spec YAML roundtrips label_strategy and defaults legacy recipes", {
  original <- synth_spec(purpose = "development", label_strategy = "preserve")
  first_path <- withr::local_tempfile(fileext = ".yaml")
  second_path <- withr::local_tempfile(fileext = ".yaml")

  cli_write_yaml(cli_spec_to_list(original), first_path)
  restored <- cli_read_spec_yaml(first_path)
  cli_write_yaml(cli_spec_to_list(restored), second_path)

  expect_equal(restored$label_strategy, "preserve")
  expect_equal(yaml::read_yaml(second_path), yaml::read_yaml(first_path))

  legacy_path <- withr::local_tempfile(fileext = ".yaml")
  yaml::write_yaml(list(purpose = "demo"), legacy_path)
  expect_equal(cli_read_spec_yaml(legacy_path)$label_strategy, "mask_rare")
})
