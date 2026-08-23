local({
# Tests for mod_export_ui / mod_export_server (single-bundle design)
# Uses testServer() - no runApp(), no browser()

export_test_state <- function(purpose = "development", seed = 1L) {
  toy_data <- data.frame(secret_col = 1:3, val = c("x", "y", "z"))

  shiny::reactiveValues(
    synthetic = toy_data,
    raw_data = toy_data,
    roles = NULL,
    spec = synth_spec(purpose = purpose, seed = seed),
    comparison = NULL,
    privacy = NULL,
    seed_used = seed,
    nav_request = NULL,
    stale = list(
      synthesis = FALSE,
      comparison = FALSE,
      export = FALSE
    )
  )
}

test_that("download filename is a seeded bundle zip", {
  testthat::skip_if_not_installed("shiny")

  state <- export_test_state(purpose = "development", seed = 1L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    expect_match(output$download, "synthetic_data_seed1_bundle\\.zip$")
  })
})

test_that("download filename reflects state$seed_used", {
  testthat::skip_if_not_installed("shiny")

  state <- export_test_state(purpose = "development", seed = 12345L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    expect_match(output$download, "synthetic_data_seed12345_bundle\\.zip$")
  })
})

test_that("use_original_names delegates to export_synthetic name-strategy resolution", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(mod_export_server, args = list(state = export_test_state("demo")), {
    expect_null(use_original_names())
  })
  shiny::testServer(mod_export_server, args = list(state = export_test_state("development")), {
    expect_null(use_original_names())
  })
})

test_that("export UI leads with bundle contents and omits generation summary", {
  html <- as.character(mod_export_ui("export"))

  expect_no_match(html, "export-export_summary", fixed = TRUE)
  expect_no_match(html, "Generation summary", fixed = TRUE)
  expect_lt(
    regexpr("What's in the bundle", html, fixed = TRUE)[[1]],
    regexpr("export-exact_match_export_gate", html, fixed = TRUE)[[1]]
  )
})

test_that("module export manifest hashes match after post-generation spec edits", {
  testthat::skip_if_not_installed("shiny")

  raw_data <- data.frame(
    patient_id = sprintf("P%02d", 1:25),
    grp = rep(letters[1:5], each = 5),
    score = seq_len(25),
    stringsAsFactors = FALSE
  )
  roles <- detect_roles(raw_data)
  roles$identifies <- c("direct", "none", "none")
  roles$sensitive <- FALSE
  roles <- dg_sync_roles_axes(roles)
  spec <- synth_spec(purpose = "development", seed = 77L, n = nrow(raw_data))
  synthetic <- synthesize_data(raw_data, spec, roles = roles)
  comparison <- compare_synthetic(raw_data, synthetic, roles = roles)
  privacy <- privacy_check(raw_data, synthetic, roles = roles, stage = "post", spec = spec)

  state <- export_test_state()
  state$raw_data <- raw_data
  state$synthetic <- synthetic
  state$comparison <- comparison
  state$privacy <- privacy
  state$roles <- roles
  state$generated_roles <- roles
  state$spec <- spec
  state$seed_used <- spec$seed
  shiny::isolate({
    state$spec$seed <- 999L
    state$roles$simulation[state$roles$variable == "grp"] <- "pass_through"
  })

  out_dir <- withr::local_tempdir()
  shiny::testServer(mod_export_server, args = list(state = state), {
    zip_path <- build_export(out_dir)
    expect_true(file.exists(zip_path))
  })

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  for (rel in names(manifest$file_sha256)) {
    expect_equal(
      digest::digest(file.path(out_dir, rel), algo = "sha256", file = TRUE, serialize = FALSE),
      manifest$file_sha256[[rel]],
      info = rel
    )
  }
})

test_that("export module blocks bundle download until k-anon is acknowledged", {
  testthat::skip_if_not_installed("shiny")

  state <- export_test_state()
  shiny::isolate({
    synthetic <- state$synthetic
    attr(synthetic, "kanon") <- list(
      qi_cols = c("age", "sex"),
      k = 5L,
      smallest_cell = 1L,
      suppressed_cells = 0L,
      infeasible = TRUE
    )
    state$synthetic <- synthetic
    state$kanon <- attr(synthetic, "kanon", exact = TRUE)
  })

  shiny::testServer(mod_export_server, args = list(state = state), {
    gate <- paste(as.character(output$kanon_export_gate), collapse = "\n")
    expect_match(gate, "every combination", fixed = TRUE)
    expect_match(gate, "<code>age</code>", fixed = TRUE)
    expect_match(gate, "<code>sex</code>", fixed = TRUE)
    expect_match(gate, "at least 5 rows", fixed = TRUE)
    expect_match(gate, "smallest combination has 1 row", fixed = TRUE)
    expect_error(
      build_export(withr::local_tempdir()),
      "requires explicit acknowledgment"
    )
  })
})

test_that("export module records acknowledgment and clears blockers once approved", {
  testthat::skip_if_not_installed("shiny")

  state <- export_test_state()
  shiny::isolate({
    synthetic <- state$synthetic
    attr(synthetic, "kanon") <- list(
      qi_cols = c("age", "sex"),
      k = 5L,
      smallest_cell = 1L,
      suppressed_cells = 0L,
      infeasible = TRUE
    )
    state$synthetic <- synthetic
    state$kanon <- attr(synthetic, "kanon", exact = TRUE)
  })

  out_dir <- withr::local_tempdir()
  shiny::testServer(mod_export_server, args = list(state = state), {
    session$setInputs(kanon_acknowledged = TRUE)
    zip_path <- build_export(out_dir)
    expect_true(file.exists(zip_path))
  })

  manifest <- jsonlite::read_json(file.path(out_dir, "agent", "manifest.json"), simplifyVector = TRUE)
  expect_true(isTRUE(manifest$kanon$acknowledged))
  expect_length(manifest$blockers, 0L)
})

# --- Exact-match export gate -------------------------------------------------

# 30 rows clears the >= 20 threshold in exact_row_match_flags(); `dx` is marked
# sensitive in question 2, so the reproduced rows expose a sensitive value.
exact_match_gate_state <- function(generation_count = 1L, n_match = 2L) {
  original <- data.frame(
    a = sprintf("%02d", 1:30),
    dx = rep(c("flu", "cold"), 15),
    stringsAsFactors = FALSE
  )
  synthetic <- data.frame(
    a = sprintf("%02d", 31:60),
    dx = rep(c("cold", "flu"), 15),
    stringsAsFactors = FALSE
  )
  if (n_match > 0) {
    rows <- seq_len(n_match)
    synthetic[rows, ] <- original[rows, ]
  }

  shiny::reactiveValues(
    synthetic = synthetic,
    raw_data = original,
    roles = data.frame(
      variable = c("a", "dx"), sensitive = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    spec = synth_spec(purpose = "development", seed = 1L),
    comparison = NULL,
    privacy = NULL,
    seed_used = 1L,
    generation_count = generation_count,
    nav_request = NULL,
    stale = list(synthesis = FALSE, comparison = FALSE, export = FALSE)
  )
}

test_that("exact matches on sensitive columns block the export on the first run", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 1L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    expect_equal(exact_match_blockers(), 2L)

    gate <- paste(as.character(output$exact_match_export_gate), collapse = "\n")
    expect_match(gate, "Exact matches on sensitive columns")
    expect_match(gate, "regenerate with a new seed", ignore.case = TRUE)
    # No override is offered until the user has actually regenerated.
    expect_false(grepl("exact_match_acknowledged", gate, fixed = TRUE))

    expect_error(build_export(tempfile()), "Regenerate with a new seed")
  })
})

test_that("the override appears only after a regeneration, and must be ticked", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 2L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()

    gate <- paste(as.character(output$exact_match_export_gate), collapse = "\n")
    expect_match(gate, "exact_match_acknowledged")

    # Offered but not ticked: still blocked.
    expect_error(build_export(tempfile()), "requires explicit acknowledgment")
  })
})

test_that("no sensitive exact matches means no gate and no block", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 1L, n_match = 0L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    expect_equal(exact_match_blockers(), 0L)
    expect_null(output$exact_match_export_gate)
  })
})

test_that("exact matches on non-sensitive columns do not block the export", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 1L)
  # Same reproduced rows, but nothing is marked sensitive in question 2.
  state$roles <- data.frame(
    variable = c("a", "dx"), sensitive = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    expect_equal(exact_match_blockers(), 0L)
    expect_null(output$exact_match_export_gate)
  })
})

# --- Disclosure-risk modal on entering Export --------------------------------

# Attach a kanon attribute to a state's synthetic output, mirroring how the
# generate step stores it (both on the attribute and in state$kanon).
attach_kanon <- function(state, kanon) {
  shiny::isolate({
    synthetic <- state$synthetic
    attr(synthetic, "kanon") <- kanon
    state$synthetic <- synthetic
    state$kanon <- kanon
  })
  state
}

test_that("disclosure modal reports the group size in plain language", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 2L, n_match = 2L)
  attach_kanon(state, list(
    qi_cols = c("age", "sex"), k = 5L, smallest_cell = 5L,
    suppressed_cells = 1L, suppressed_rows = 7L,
    suppressed_row_frac = 7 / 30, infeasible = FALSE
  ))

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(disclosure_modal()), collapse = "\n")
    # The plain-language k sentence, with the value substituted in.
    expect_match(html, "at least 5 times", fixed = TRUE)
    expect_match(html, "no individual row stands out", fixed = TRUE)
    # The columns the protection applies to.
    expect_match(html, "<code>age</code>", fixed = TRUE)
    expect_match(html, "<code>sex</code>", fixed = TRUE)
    # How many rows were blanked, with the fraction.
    expect_match(html, "7 row(s)", fixed = TRUE)
    expect_match(html, "23.3%", fixed = TRUE)
  })
})

test_that("disclosure modal says plainly when the protection was infeasible", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 2L, n_match = 0L)
  attach_kanon(state, list(
    qi_cols = c("age", "sex"), k = 5L, smallest_cell = 1L,
    suppressed_cells = 0L, suppressed_rows = 0L,
    suppressed_row_frac = 0, infeasible = TRUE
  ))

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(disclosure_modal()), collapse = "\n")
    expect_match(html, "could not be applied to this output", fixed = TRUE)
    expect_match(html, "<code>age</code>", fixed = TRUE)
  })
})

test_that("disclosure modal reports exact-match and sensitive counts", {
  testthat::skip_if_not_installed("shiny")

  # 2 reproduced rows, dx marked sensitive and populated -> both disclose.
  state <- exact_match_gate_state(generation_count = 2L, n_match = 2L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(disclosure_modal()), collapse = "\n")
    expect_match(html, "2 synthetic row(s) reproduce a real record exactly.", fixed = TRUE)
    expect_match(html, "Of those, 2 expose a value you marked sensitive.", fixed = TRUE)
  })
})

test_that("disclosure modal reports privacy_check_post() flags with severity and advice", {
  testthat::skip_if_not_installed("shiny")

  # 30 rows with 2 exact matches -> privacy_check_post() raises an exact-row
  # match flag (HIGH) plus a rare-category flag, both surfaced in the modal.
  state <- exact_match_gate_state(generation_count = 2L, n_match = 2L)

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    html <- paste(as.character(disclosure_modal()), collapse = "\n")
    expect_match(html, "Other checks", fixed = TRUE)
    expect_match(html, "HIGH", fixed = TRUE)
    expect_match(html, "exact-row match", fixed = TRUE)
    expect_match(html, "Recommendation:", fixed = TRUE)
  })
})

test_that("disclosure modal never shows jargon the user will not understand", {
  testthat::skip_if_not_installed("shiny")

  # Poisoned flags straight from privacy_check_post()-shaped output, containing
  # every forbidden term, must be scrubbed at display time.
  poisoned <- data.frame(
    variable = c("(quasi-identifiers)", "x"),
    flag = c(
      "Synthetic output has a QI cell of size 1 (< k=5)",
      "k-anonymity not reached"
    ),
    severity = c("HIGH", "HIGH"),
    recommendation = c(
      "k-anonymity enforcement did not reach the target",
      "review the quasi-identifier set"
    ),
    stringsAsFactors = FALSE
  )
  modal <- disclosure_risk_modal(
    kanon = list(
      qi_cols = "x", k = 5L, smallest_cell = 5L,
      suppressed_rows = 0L, suppressed_row_frac = 0, infeasible = FALSE
    ),
    n_exact = 0L, n_sensitive = 0L, privacy_flags = poisoned
  )
  html <- tolower(paste(as.character(modal), collapse = "\n"))
  expect_false(grepl("k-anon", html, fixed = TRUE))
  expect_false(grepl("quasi-identifier", html, fixed = TRUE))

  # And a realistic, module-built modal is clean too.
  state <- exact_match_gate_state(generation_count = 2L, n_match = 2L)
  attach_kanon(state, list(
    qi_cols = c("age", "sex"), k = 5L, smallest_cell = 5L,
    suppressed_cells = 0L, suppressed_rows = 0L,
    suppressed_row_frac = 0, infeasible = FALSE
  ))
  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    live_html <- tolower(paste(as.character(disclosure_modal()), collapse = "\n"))
    expect_false(grepl("k-anon", live_html, fixed = TRUE))
    expect_false(grepl("quasi-identifier", live_html, fixed = TRUE))
  })
})

test_that("arriving on the export step builds and shows the disclosure brief", {
  testthat::skip_if_not_installed("shiny")

  state <- exact_match_gate_state(generation_count = 2L, n_match = 2L)
  attach_kanon(state, list(
    qi_cols = c("age", "sex"), k = 5L, smallest_cell = 5L,
    suppressed_cells = 0L, suppressed_rows = 0L,
    suppressed_row_frac = 0, infeasible = FALSE
  ))

  shiny::testServer(mod_export_server, args = list(state = state), {
    session$flushReact()
    modal <- disclosure_modal()
    expect_false(is.null(modal))
    html <- paste(as.character(modal), collapse = "\n")
    expect_match(html, "Disclosure risk of this bundle", fixed = TRUE)

    # The single Continue button dismisses; the modal grants nothing.
    expect_match(html, "Continue", fixed = TRUE)

    # Setting the active tab to export fires the observer (showModal); this
    # must run without error and must not alter any gate.
    expect_no_error({
      state$active_tab <- "export"
      session$flushReact()
    })
  })
})

test_that("build_export() keeps all three refusal conditions after the modal change", {
  testthat::skip_if_not_installed("shiny")

  # 1. Infeasible group-size protection, unacknowledged.
  state_kanon <- export_test_state()
  attach_kanon(state_kanon, list(
    qi_cols = c("age", "sex"), k = 5L, smallest_cell = 1L,
    suppressed_cells = 0L, infeasible = TRUE
  ))
  shiny::testServer(mod_export_server, args = list(state = state_kanon), {
    expect_error(build_export(tempfile()), "requires explicit acknowledgment")
  })

  # 2. Sensitive exact matches on the first run: hard block, no override.
  state_hard <- exact_match_gate_state(generation_count = 1L)
  shiny::testServer(mod_export_server, args = list(state = state_hard), {
    session$flushReact()
    expect_error(build_export(tempfile()), "Regenerate with a new seed")
  })

  # 3. Sensitive exact matches after regenerating, override not ticked.
  state_override <- exact_match_gate_state(generation_count = 2L)
  shiny::testServer(mod_export_server, args = list(state = state_override), {
    session$flushReact()
    expect_error(build_export(tempfile()), "requires explicit acknowledgment")
  })
})

test_that("plain language covers the real privacy_check_post flag wording", {
  # Both strings are copied verbatim from R/privacy-check.R -- the HIGH flag
  # raised when the minimum group size is not reached. Asserting against the
  # real wording rather than an invented input is the point: an earlier version
  # of this suite poisoned a flag with only the terms the scrubber already
  # handled, so it passed while this exact flag reached users untouched. That
  # flag says "QI" and "k=5", not "quasi-identifier" and "k-anonymity".
  flag <- sprintf("Synthetic output has a QI cell of size %d (< k=%d)", 3L, 5L)
  recommendation <- paste(
    "k-anonymity enforcement did not reach the target;",
    "review enforce_kanon settings"
  )
  jargon <- "k-anon|quasi-identifier|\\bQI\\b|\\bk *= *[0-9]|enforce_kanon"

  expect_false(grepl(jargon, dg_plain_language(flag), ignore.case = TRUE))
  expect_false(grepl(jargon, dg_plain_language(recommendation),
    ignore.case = TRUE
  ))
  expect_true(grepl("fewer than the 5 required",
    dg_plain_language(flag),
    fixed = TRUE
  ))
})
})
