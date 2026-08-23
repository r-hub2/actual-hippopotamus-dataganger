test_that("skill command prints the packaged SKILL.md", {
  skill_path <- system.file("agent-skill", "SKILL.md", package = "dataganger")
  expect_true(nzchar(skill_path))
  expect_true(file.exists(skill_path))

  out <- capture.output(code <- dataganger_cli(c("skill"), quit = FALSE))
  expect_identical(code, 0L)
  expect_identical(out[[1]], "You are not allowed to read the original data.")
  expect_identical(paste(out, collapse = "\n"), paste(readLines(skill_path, warn = FALSE), collapse = "\n"))
})

test_that("CLI help lists the skill command", {
  out <- capture.output(code <- dataganger_cli(c("--help"), quit = FALSE))

  expect_identical(code, 0L)
  expect_match(paste(out, collapse = "\n"), "skill \\[--out <file>\\]")
})

test_that("skill command copies the packaged SKILL.md with --out", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "SKILL.md")
  skill_path <- system.file("agent-skill", "SKILL.md", package = "dataganger")

  result <- run_cli(c("skill", "--out", out_path))

  expect_identical(result$code, 0L)
  expect_true(file.exists(out_path))
  expect_identical(
    paste(readLines(out_path, warn = FALSE), collapse = "\n"),
    paste(readLines(skill_path, warn = FALSE), collapse = "\n")
  )
})
