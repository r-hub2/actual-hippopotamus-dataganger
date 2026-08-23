local({
roles_upload_fixture_path <- function(data, filename) {
  path <- tempfile(pattern = tools::file_path_sans_ext(filename))
  path <- paste0(path, ".", tools::file_ext(filename))
  readr::write_csv(data, path)
  path
}

roles_load_example_data <- function(name) {
  data_env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "dataganger", envir = data_env)
  data_env[[name]]
}

roles_upload_input_value <- function(path, type = "text/csv") {
  data.frame(
    name = basename(path),
    size = file.info(path)$size,
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

roles_host_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- mod_state_server("state")
    mod_upload_server("upload", state)
    mod_column_filter_server("column_filter", state)
    mod_roles_server("roles", state)

    shiny::observe({
      if (!is.null(state$raw_data) &&
          !is.null(state$profile) &&
          is.null(state$roles)) {
        state$roles <- detect_roles(state$raw_data, profile = state$profile)
      }
    })

    list(state = state)
  })
}

roles_test_state <- function() {
  state <- shiny::reactiveValues()
  state$raw_data <- data.frame(
    id = c("P001", "P002", "P003"),
    zip = c("100", "100", "200"),
    income = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  state$roles <- tibble::tibble(
    variable = c("id", "zip", "income"),
    recommended_role = c("alphanumeric ID", "categorical candidate", "numeric"),
    user_role = c(NA_character_, NA_character_, NA_character_),
    class = c("alphanumeric ID", "categorical candidate", "numeric"),
    identifies = c("direct", "combination", "none"),
    sensitive = c(FALSE, FALSE, FALSE),
    disclosure_role = c("direct", "quasi", "none"),
    simulation = c("drop", "synthesize", "synthesize"),
    reason = c("Likely identifies a person.", "May identify in combination.", "Looks numeric."),
    disclosure_reason = c(NA_character_, NA_character_, NA_character_)
  )
  state$profile <- list()
  state
}

roles_test_state_with_unset <- function() {
  state <- shiny::reactiveValues()
  state$raw_data <- data.frame(
    id = c("P001", "P002", "P003"),
    zip = c("100", "100", "200"),
    income = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  state$roles <- tibble::tibble(
    variable = c("id", "zip", "income"),
    recommended_role = c("alphanumeric ID", "categorical candidate", "numeric"),
    user_role = c(NA_character_, NA_character_, NA_character_),
    class = c("alphanumeric ID", "categorical candidate", "categorical candidate"),
    identifies = c("direct", "combination", NA_character_),
    sensitive = c(FALSE, FALSE, FALSE),
    disclosure_role = c("direct", "quasi", ""),
    simulation = c("drop", "synthesize", "synthesize"),
    reason = c("Likely identifies a person.", "May identify in combination.", "Needs review."),
    disclosure_reason = c(NA_character_, NA_character_, NA_character_)
  )
  state$profile <- list()
  state
}

test_that("editing user_role and confirming writes back to state", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-five.csv"
  )

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    expect_s3_class(state$roles, "dataganger_roles")

    session$setInputs(
      `roles-role_change` = list(row = 1L, value = "numeric")
    )
    session$flushReact()
    session$setInputs(`roles-confirm` = 1L)
    session$flushReact()

    expect_equal(state$roles$user_role[[1]], "numeric")
  })
})

test_that("editing Action treatment and confirming writes back to state", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-simulation-five.csv"
  )

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    # record_id is an alphanumeric ID by default, so its default action is
    # scramble, not synthesize.
    expect_equal(state$roles$simulation[[1]], "scramble")

    session$setInputs(
      `roles-simulation_change` = list(row = 1L, value = "pass_through")
    )
    session$flushReact()
    session$setInputs(`roles-confirm` = 1L)
    session$flushReact()

    expect_equal(state$roles$simulation[[1]], "pass_through")
  })
})

test_that("invalid simulation treatment is ignored silently", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-simulation-invalid-five.csv"
  )

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    original_simulation <- state$roles$simulation
    session$setInputs(
      `roles-simulation_change` = list(row = 1L, value = "explode")
    )
    session$flushReact()
    session$setInputs(`roles-confirm` = 1L)
    session$flushReact()

    expect_identical(state$roles$simulation, original_simulation)
  })
})

test_that("confirming role edits invalidates downstream state", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-stale-five.csv"
  )

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    state$spec <- list(purpose = "development")
    state$synthetic <- tibble::tibble(x = 1)
    state$comparison <- list(ok = TRUE)
    state$privacy <- tibble::tibble(flag = "none")
    state$stale <- list(synthesis = FALSE, comparison = FALSE, export = FALSE)
    session$flushReact()

    session$setInputs(
      `roles-role_change` = list(row = 1L, value = "numeric")
    )
    session$flushReact()
    session$setInputs(`roles-confirm` = 1L)
    session$flushReact()

    expect_true(isTRUE(state$stale$synthesis))
    expect_null(state$spec)
    expect_null(state$synthetic)
    expect_null(state$comparison)
    expect_null(state$privacy)
  })
})

test_that("editing a non-user_role column is ignored silently", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-readonly-five.csv"
  )

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    original_user_roles <- state$roles$user_role

    # Invalid role value is silently rejected
    session$setInputs(
      `roles-role_change` = list(row = 1L, value = "hacked_role")
    )
    session$flushReact()
    session$setInputs(`roles-confirm` = 1L)
    session$flushReact()

    expect_identical(state$roles$user_role, original_user_roles)
  })
})


test_that("roles table labels the four configure columns", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey[1:5, ],
    "roles-type-header.csv"
  )

  shiny::testServer(roles_host_server, {
    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    html <- paste(as.character(output$`roles-roles_table`), collapse = "
")
    expect_match(html, ">Column<")
    # Q1 and Q2 now share a single stacked header cell to save horizontal
    # space, so Q1's label is followed by the line break, not by </th>.
    expect_match(html, "Points to a person\\? \\(Q1\\)")
    expect_match(html, "Sensitive\\? \\(Q2\\)")
    expect_match(html, ">Action override<")
    expect_false(grepl(">What we'll do<", html, fixed = TRUE))
    expect_false(grepl(">Action<", html, fixed = TRUE))
  })
})

test_that("Configure table shows both axis selects, examples, and inline override controls", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_match(html, "identifies_change")
    expect_match(html, "sensitive_change")
    expect_match(html, "Only in combination with other columns")
    expect_match(html, "on its own")
    expect_match(html, "Action override")
    expect_match(html, "Data type override")
    # The always-on "Pass through keeps the real values" hint was removed from
    # every row: it warned about an action almost no row uses. The warning is
    # not gone -- it is in the legend's amber Pass through row, and the export
    # path still flags raw columns before the bundle is written.
    expect_false(grepl("Pass through keeps the real values", html, fixed = TRUE))
    expect_false(grepl("What we.ll do", html))
    expect_false(grepl(">Action<", html, fixed = TRUE))
    expect_false(grepl(">TYPE<", html, fixed = TRUE))
    expect_false(grepl("Advanced", html, fixed = TRUE))
  })
})

test_that("inline override controls expose drop/pass-through and data type", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_match(html, "Action override")
    # "Pass through" remains as a dropdown option; only the always-on hint
    # under every dropdown was removed. See the legend test for where the
    # warning now lives.
    expect_match(html, "Pass through")
    expect_false(grepl("verify before sharing", html, fixed = TRUE))
    expect_false(grepl("<summary", html, fixed = TRUE))
    expect_false(grepl("<details", html, fixed = TRUE))
  })
})

test_that("'identifier' (pseudo identifier) is no longer a selectable type; alphanumeric ID covers it", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_false(grepl('value="identifier"', html, fixed = TRUE))
    expect_false(grepl("pseudo identifier", html, fixed = TRUE))
    expect_match(html, 'value="alphanumeric_id"', fixed = TRUE)
    expect_match(html, "alpha-numeric ID", fixed = TRUE)
  })
})

test_that("role_change silently rejects 'identifier' since it is no longer a valid type", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    original_user_roles <- state$roles$user_role
    session$setInputs(role_change = list(row = 1, value = "identifier"))
    expect_identical(state$roles$user_role, original_user_roles)
  })
})

test_that("logical is no longer a selectable type; the dropdown offers categorical instead", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_false(grepl('value="logical"', html, fixed = TRUE))
    expect_match(html, 'value="categorical"', fixed = TRUE)
  })
})

test_that("role_change silently rejects 'logical' since it is no longer a valid type", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    original_user_roles <- state$roles$user_role
    session$setInputs(role_change = list(row = 1, value = "logical"))
    expect_identical(state$roles$user_role, original_user_roles)
  })
})

test_that("the type dropdown offers alpha-numeric ID and the action dropdown offers scramble", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_match(html, 'value="alphanumeric_id"', fixed = TRUE)
    expect_match(html, "alpha-numeric ID", fixed = TRUE)
    expect_match(html, 'value="scramble"', fixed = TRUE)
    expect_match(html, "Scramble", fixed = TRUE)
  })
})

test_that("'drop' does not appear as a type dropdown option", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$roles_table), collapse = "\n")
    # "drop" must still appear once, as an Action override option, but never
    # as a value="drop" <option> inside a type (role_change) dropdown.
    type_selects <- regmatches(html, gregexpr(
      '(?s)<select[^>]*role_change[^>]*>.*?</select>', html, perl = TRUE
    ))[[1]]
    expect_true(length(type_selects) > 0L)
    expect_no_match(type_selects, 'value="drop"', fixed = TRUE)
  })
})

test_that("choosing alpha-numeric ID as the type sets identifies=direct and simulation=scramble", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # Row 3 ("income") starts as a plain non-identifying numeric column.
    session$setInputs(role_change = list(row = 3, value = "alphanumeric_id"))
    expect_equal(state$roles$identifies[[3]], "direct")
    expect_equal(state$roles$disclosure_role[[3]], "direct")
    expect_equal(state$roles$simulation[[3]], "scramble")
  })
})

test_that("retyping a Q1-confirmed direct identifier to categorical resets Q1 instead of staying dropped", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()
  # Row 1 ("id") already has identifies="direct" AND a user-confirmed Q1
  # answer -- simulating a user who earlier answered "yes, this identifies
  # a person" for the alphanumeric ID column.
  shiny::isolate({
    state$roles$user_identifies <- c("direct", NA_character_, NA_character_)
  })

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 1, value = "categorical"))
    # The type change is itself an explicit statement that this column is
    # now ordinary data, so it overrides the earlier Q1 answer instead of
    # leaving it stuck on "direct" (which would keep forcing a drop).
    expect_true(is.na(state$roles$identifies[[1]]))
    expect_true(is.na(state$roles$user_identifies[[1]]))
    expect_equal(state$roles$simulation[[1]], "synthesize")
    # Q1 is reset to unanswered (not silently flipped to "none"), so
    # generation stays blocked until the user re-confirms it.
    pending <- shiny::isolate(roles_generation_pending(state$roles))
    expect_true(length(pending) > 0)
  })
})

test_that("overriding an alphanumeric ID to categorical shows a plain reset caption", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 1, value = "categorical"))
    session$flushReact()
    html <- paste(as.character(output$roles_table), collapse = "\n")
    expect_match(html, "Now treated as ordinary data", fixed = TRUE)
    expect_match(html, "Q1 was reset", fixed = TRUE)
    # Only 3 distinct values in a 3-row fixture -- well under the Compare
    # cap, so no cardinality warning should appear.
    expect_false(grepl("Compare limit", html, fixed = TRUE))
  })
})

test_that("overriding a high-cardinality alphanumeric ID to categorical warns about the Compare limit", {
  testthat::skip_if_not_installed("shiny")
  state <- shiny::reactiveValues()
  state$raw_data <- data.frame(
    id = sprintf("REC-%03d", 1:50),
    x  = rep(1:5, 10),
    stringsAsFactors = FALSE
  )
  state$roles <- tibble::tibble(
    variable = c("id", "x"),
    recommended_role = c("alphanumeric ID", "numeric"),
    user_role = c(NA_character_, NA_character_),
    class = c("alphanumeric ID", "numeric"),
    identifies = c("direct", "none"),
    sensitive = c(FALSE, FALSE),
    disclosure_role = c("direct", "none"),
    simulation = c("scramble", "synthesize"),
    reason = c("Looks like an ID.", "Looks numeric."),
    disclosure_reason = c(NA_character_, NA_character_)
  )
  state$profile <- list()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 1, value = "categorical"))
    session$flushReact()
    html <- paste(as.character(output$roles_table), collapse = "\n")
    # 50 distinct values in 50 rows is well above dg_max_comparable_levels(50) = 10.
    expect_match(html, "distinct values is above", fixed = TRUE)
    expect_match(html, "Compare limit", fixed = TRUE)
  })
})

test_that("the legend shows each type's default treatment", {
  html <- as.character(type_action_legend_ui())
  expect_match(html, "Resample")
  expect_match(html, "Simulate")
  expect_match(html, "Scramble")
  expect_match(html, "alpha-numeric ID")
  expect_false(grepl("pseudo identifier", html, fixed = TRUE))

  # The legend is now the only place on this step that warns about Pass
  # through, so the warning and its amber styling have to stay put.
  expect_match(html, "The real values are copied across unchanged", fixed = TRUE)
  expect_match(html, "Verify before sharing", fixed = TRUE)
  expect_match(html, "#b7791f", fixed = TRUE)

  # The mask_rare qualifier is broken onto a second line to keep the action
  # column narrow; the dropdown keeps the one-line label (an <option> cannot
  # hold a line break), so the two labels are deliberately not shared.
  expect_match(html, "(rare levels masked)", fixed = TRUE)
  expect_match(html, "<br/>", fixed = TRUE)
  expect_false(grepl("white-space:nowrap", html, fixed = TRUE))
})

test_that("a logical/boolean column is classified as categorical, not a distinct logical type", {
  df <- data.frame(
    flag = rep(c(TRUE, FALSE), 10),
    other = 1:20
  )
  roles <- detect_roles(df)
  row <- roles[roles$variable == "flag", ]
  expect_identical(dg_class_to_role(row$class), "categorical")
})

test_that("changing identifies derives drop action and changing sensitive keeps synthesis", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(identifies_change = list(row = 2, value = "direct"))
    expect_equal(state$roles$simulation[[2]], "drop")
    expect_equal(state$roles$disclosure_role[[2]], "direct")
    session$setInputs(identifies_change = list(row = 2, value = "none"))
    expect_equal(state$roles$simulation[[2]], "synthesize")
    session$setInputs(sensitive_change = list(row = 2, value = "yes"))
    expect_true(state$roles$sensitive[[2]])
    expect_equal(state$roles$disclosure_role[[2]], "sensitive")
  })
})

test_that("overriding the type away from identifier clears the direct disclosure role", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # Row 1 ("id") starts as an auto-detected direct identifier.
    expect_equal(state$roles$identifies[[1]], "direct")
    expect_equal(state$roles$disclosure_role[[1]], "direct")
    expect_equal(state$roles$simulation[[1]], "drop")

    session$setInputs(role_change = list(row = 1, value = "categorical"))

    expect_true(is.na(state$roles$identifies[[1]]))
    expect_true(is.na(state$roles$disclosure_role[[1]]))
    expect_equal(state$roles$simulation[[1]], "synthesize")
  })
})

test_that("choosing alphanumeric_id or free_text as the type always sets direct, even without a prior direct role", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # Row 3 ("income") starts as a plain non-identifying numeric column.
    expect_equal(state$roles$identifies[[3]], "none")

    session$setInputs(role_change = list(row = 3, value = "alphanumeric_id"))
    expect_equal(state$roles$identifies[[3]], "direct")
    expect_equal(state$roles$disclosure_role[[3]], "direct")
    expect_equal(state$roles$simulation[[3]], "scramble")

    session$setInputs(role_change = list(row = 3, value = "free_text"))
    expect_equal(state$roles$identifies[[3]], "direct")
    expect_equal(state$roles$simulation[[3]], "drop")
  })
})

test_that("'drop' is no longer a selectable type; it only lives in Action override", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    original_user_roles <- state$roles$user_role
    original_simulation <- state$roles$simulation
    # Row 2 ("zip") starts as a quasi-identifier (combination); "drop" is not
    # a valid type value any more, so the change is silently rejected.
    session$setInputs(role_change = list(row = 2, value = "drop"))
    expect_identical(state$roles$user_role, original_user_roles)
    expect_identical(state$roles$simulation, original_simulation)

    # The Action override dropdown still supports drop directly.
    session$setInputs(simulation_change = list(row = 2, value = "drop"))
    expect_equal(state$roles$simulation[[2]], "drop")
  })
})

test_that("an explicit keep decision survives answering Q2 (sensitivity)", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # Row 1 ("id") is seeded as a direct identifier. The user deliberately
    # keeps it via the Action override.
    session$setInputs(simulation_change = list(row = 1, value = "pass_through"))
    expect_equal(state$roles$simulation[[1]], "pass_through")

    # Answering Q2 asks about sensitivity, not about dropping. It previously
    # re-derived the action and silently turned the keep into "drop" -- and
    # because Q2 is mandatory, that fired for every direct-seeded column.
    session$setInputs(sensitive_change = list(row = 1, value = "no"))
    expect_equal(state$roles$simulation[[1]], "pass_through")

    session$setInputs(sensitive_change = list(row = 1, value = "yes"))
    expect_equal(state$roles$simulation[[1]], "pass_through")
  })
})

test_that("an explicit keep decision survives a later Q1 answer", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(simulation_change = list(row = 1, value = "scramble"))
    session$setInputs(identifies_change = list(row = 1, value = "combination"))
    expect_equal(state$roles$simulation[[1]], "scramble")
  })
})

test_that("choosing Q1 = direct still drops, so the drop layer is preserved", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # No explicit action chosen: answering Q1 "direct" is itself the drop
    # decision and must still take effect.
    session$setInputs(identifies_change = list(row = 2, value = "direct"))
    expect_equal(state$roles$identifies[[2]], "direct")
    expect_equal(state$roles$simulation[[2]], "drop")
  })
})

test_that("an explicit Q1 answer is not silently overridden by a later type change", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # User explicitly confirms row 1 ("id") is not identifying via the Q1 dropdown.
    session$setInputs(identifies_change = list(row = 1, value = "none"))
    expect_equal(state$roles$identifies[[1]], "none")
    expect_equal(state$roles$simulation[[1]], "synthesize")

    # Changing the type dropdown afterwards must not clobber that explicit answer.
    session$setInputs(role_change = list(row = 1, value = "numeric"))
    expect_equal(state$roles$identifies[[1]], "none")
    expect_equal(state$roles$simulation[[1]], "synthesize")
  })
})

test_that("disclosure help leads with the two questions and is not wrapped in details", {
  html <- as.character(disclosure_help_ui())
  expect_match(html, "Could a value point to a specific person")
  expect_match(html, "Would it be considered private or intrusive")
  expect_match(html, "Only in combination with other columns")
  expect_false(grepl("<details", html, fixed = TRUE))
})

test_that("gate copy uses 'need an answer'", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state_with_unset()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$disclosure_gate), collapse = "\n")
    expect_match(html, "still need an answer before you can generate")
  })
})

test_that("disclosure gate excludes drop and pass-through rows from unset count", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  shiny::testServer(roles_host_server, {
    state <- session$getReturned()$state

    roles <- tibble::tibble(
      variable = c("drop_me", "share_me", "needs_role"),
      class = c("character", "numeric", "categorical candidate"),
      recommended_role = c("free text", "numeric", "numeric"),
      user_role = c(NA_character_, NA_character_, NA_character_),
      identifies = c(NA_character_, NA_character_, NA_character_),
      sensitive = c(FALSE, FALSE, FALSE),
      simulation = c("drop", "pass_through", "synthesize"),
      reason = c("reason", "reason", "reason"),
      disclosure_role = c(NA_character_, NA_character_, NA_character_),
      disclosure_reason = c(NA_character_, NA_character_, NA_character_)
    )
    class(roles) <- c("dataganger_roles", class(roles))

    state$roles <- roles
    session$flushReact()

    html <- paste(as.character(output$`roles-disclosure_gate`), collapse = "\n")
    expect_match(html, "1 column still need an answer")
    expect_false(grepl("3 columns", html, fixed = TRUE))
  })
})


test_that("agg_warning banner shows for aggregated count tables", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  agg <- data.frame(
    region = rep(c("North", "South", "East", "West"), each = 2L),
    sex    = rep(c("F", "M"), times = 4L),
    n      = c(12L, 9L, 7L, 4L, 15L, 11L, 6L, 3L),
    stringsAsFactors = FALSE
  )
  csv_path <- roles_upload_fixture_path(agg, "roles-aggregated.csv")

  shiny::testServer(roles_host_server, {
    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    html <- paste(as.character(output$`roles-agg_warning`), collapse = "\n")
    expect_match(html, "aggregated data")
  })
})

test_that("agg_warning banner stays empty for individual-level microdata", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  example_health_survey <- roles_load_example_data("example_health_survey")
  csv_path <- roles_upload_fixture_path(
    example_health_survey,
    "roles-microdata.csv"
  )

  shiny::testServer(roles_host_server, {
    session$setInputs(`upload-file` = roles_upload_input_value(csv_path))
    session$flushReact()
    cf_continue(session, state)
    session$flushReact()

    html <- paste(as.character(output$`roles-agg_warning`), collapse = "\n")
    expect_false(grepl("aggregated data", html))
  })
})


test_that("roles confirm is blocked until every generated column has an answer", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DT")

  state <- roles_test_state_with_unset()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(confirm = 1L)
    session$flushReact()
    expect_identical(state$roles_confirmed %||% 0L, 0L)

    # Must confirm both Q1 and Q2 for every eligible column
    session$setInputs(identifies_change = list(row = 2L, value = "combination"))
    session$flushReact()
    session$setInputs(sensitive_change = list(row = 2L, value = "no"))
    session$flushReact()
    session$setInputs(identifies_change = list(row = 3L, value = "none"))
    session$flushReact()
    session$setInputs(sensitive_change = list(row = 3L, value = "no"))
    session$flushReact()
    session$setInputs(confirm = 2L)
    session$flushReact()
    expect_identical(state$roles_confirmed, 1L)
  })
})


test_that("question 1 always offers 'direct', attested or not", {
  # Hiding 'direct' when attested made a direct-seeded column's own state
  # unrepresentable in its control: the select rendered with no option
  # selected and no "needs an answer" styling, so the classification that
  # decides whether the column is dropped could be neither seen nor changed.
  expect_equal(q1_identifies_choices(attested = FALSE),
               c("none", "combination", "direct"))
  expect_equal(q1_identifies_choices(attested = TRUE),
               c("none", "combination", "direct"))
})

test_that("a direct-seeded column renders with its own answer selectable", {
  meta <- dg_identifies_option_meta()
  allowed <- q1_identifies_choices(attested = TRUE)
  shown <- Filter(function(m) m$value %in% allowed, meta)
  expect_true("direct" %in% vapply(shown, function(m) m$value, character(1)))
})

test_that("disclosure help uses the attested direct-identifier framing copy", {
  html <- as.character(disclosure_help_ui(attested = TRUE))
  expect_match(html, "You('|&#39;|&apos;)ve confirmed there are no direct identifiers")
  expect_match(html, "Could this column, combined with others, help single out a person\\?")
  expect_match(html, "Is this column sensitive \u2014 would it be considered private or intrusive")
  expect_false(grepl("Yes, directly", html, fixed = TRUE))
})

# ---- Bulk configure ----

test_that("bulk toolbar prompts for a selection when nothing is checked", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    html <- paste(as.character(output$bulk_toolbar), collapse = "\n")
    expect_match(html, "Check columns below to bulk-edit", fixed = TRUE)
    expect_false(grepl("Apply to", html, fixed = TRUE))
  })
})

test_that("checking a row via row_select adds it to the selection and the toolbar reflects the count", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(row_select = list(variable = "zip", checked = TRUE))
    session$flushReact()
    expect_identical(selected_vars(), "zip")

    html <- paste(as.character(output$bulk_toolbar), collapse = "\n")
    expect_match(html, "1 column selected", fixed = TRUE)

    # Unchecking removes it again.
    session$setInputs(row_select = list(variable = "zip", checked = FALSE))
    session$flushReact()
    expect_identical(selected_vars(), character(0))
  })
})

test_that("select_all_visible selects and clears every currently visible row", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(select_all_visible = TRUE)
    session$flushReact()
    expect_setequal(selected_vars(), c("id", "zip", "income"))

    session$setInputs(select_all_visible = FALSE)
    session$flushReact()
    expect_identical(selected_vars(), character(0))
  })
})

test_that("bulk_clear empties the selection", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(row_select = list(variable = "zip", checked = TRUE))
    session$setInputs(row_select = list(variable = "income", checked = TRUE))
    session$flushReact()
    expect_length(selected_vars(), 2L)

    session$setInputs(bulk_clear = 1L)
    session$flushReact()
    expect_identical(selected_vars(), character(0))
  })
})

test_that("bulk-applying a type change updates every selected row using the same rules as a single edit", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(row_select = list(variable = "zip", checked = TRUE))
    session$setInputs(row_select = list(variable = "income", checked = TRUE))
    session$flushReact()

    session$setInputs(bulk_type_value = "alphanumeric_id")
    session$setInputs(bulk_apply_type = 1L)
    session$flushReact()

    roles <- state$roles
    idx <- roles$variable %in% c("zip", "income")
    expect_true(
      all(roles$user_role[idx] == "alphanumeric_id"),
      info = paste("Selected user roles:", paste(roles$user_role[idx], collapse = ", "))
    )
    # Same consequence as the single-row handler: choosing alphanumeric_id
    # sets identifies=direct and defaults the action to scramble.
    expect_true(
      all(roles$identifies[idx] == "direct"),
      info = paste("Selected identifies values:", paste(roles$identifies[idx], collapse = ", "))
    )
    expect_true(
      all(roles$simulation[idx] == "scramble"),
      info = paste("Selected simulations:", paste(roles$simulation[idx], collapse = ", "))
    )
    # The untouched row is unaffected.
    expect_false(roles$user_role[roles$variable == "id"] %in% "alphanumeric_id")
  })
})

test_that("bulk-applying a Q1 answer resets simulation the same way the single dropdown does", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(select_all_visible = TRUE)
    session$flushReact()

    session$setInputs(bulk_identifies_value = "none")
    session$setInputs(bulk_apply_identifies = 1L)
    session$flushReact()

    roles <- state$roles
    expect_true(
      all(roles$identifies == "none"),
      info = paste("Identifies values:", paste(roles$identifies, collapse = ", "))
    )
    expect_true(
      all(roles$user_identifies == "none"),
      info = paste("User identifies values:", paste(roles$user_identifies, collapse = ", "))
    )
    expect_true(
      all(roles$simulation == "synthesize"),
      info = paste("Simulation values:", paste(roles$simulation, collapse = ", "))
    )
  })
})

test_that("bulk-applying an action override sets simulation for every selected row", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(row_select = list(variable = "id", checked = TRUE))
    session$setInputs(row_select = list(variable = "zip", checked = TRUE))
    session$flushReact()

    session$setInputs(bulk_simulation_value = "drop")
    session$setInputs(bulk_apply_simulation = 1L)
    session$flushReact()

    roles <- state$roles
    expect_equal(roles$simulation[roles$variable == "id"], "drop")
    expect_equal(roles$simulation[roles$variable == "zip"], "drop")
    expect_equal(roles$simulation[roles$variable == "income"], "synthesize")
  })
})

test_that("bulk apply with an empty selection is a silent no-op", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    original <- state$roles
    session$setInputs(bulk_type_value = "categorical")
    session$setInputs(bulk_apply_type = 1L)
    session$flushReact()
    expect_identical(state$roles, original)
  })
})

test_that("postal_code appears in ROLE_OPTIONS and ROLE_LABELS", {
  expect_true("postal_code" %in% dataganger:::dg_rec_to_role("postal code"))
  expect_equal(dataganger:::dg_rec_to_role("postal code"), "postal_code")
})

test_that("apply_type_change sets postal defaults when switching to postal_code", {
  testthat::skip_if_not_installed("shiny")
  state <- shiny::reactiveValues()
  state$raw_data <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4"),
    x = 1:3,
    stringsAsFactors = FALSE
  )
  state$roles <- tibble::tibble(
    variable = c("postal_code", "x"),
    recommended_role = c("categorical candidate", "numeric"),
    user_role = c(NA_character_, NA_character_),
    class = c("character", "numeric"),
    identifies = c("combination", "none"),
    sensitive = c(FALSE, FALSE),
    disclosure_role = c("quasi", "none"),
    simulation = c("synthesize", "synthesize"),
    postal_strategy = c(NA_character_, NA_character_),
    postal_country = c(NA_character_, NA_character_),
    reason = c("test", "test"),
    disclosure_reason = c(NA_character_, NA_character_),
    user_identifies = c(NA_character_, NA_character_),
    user_sensitive = c(NA, NA)
  )
  state$profile <- list()
  state$attested_no_direct <- TRUE

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 1L, value = "postal_code"))
    expect_equal(state$roles$postal_strategy[[1]], "generate")
    expect_true(is.na(state$roles$postal_country[[1]]))
    expect_equal(state$roles$user_role[[1]], "postal_code")
  })
})

test_that("apply_type_change clears postal fields when switching away from postal_code", {
  testthat::skip_if_not_installed("shiny")
  state <- shiny::reactiveValues()
  state$raw_data <- data.frame(
    postal_code = c("K1A 0B1", "M5V 3L9", "H2X 1Y4"),
    x = 1:3,
    stringsAsFactors = FALSE
  )
  state$roles <- tibble::tibble(
    variable = c("postal_code", "x"),
    recommended_role = c("postal code", "numeric"),
    user_role = c("postal_code", NA_character_),
    class = c("character", "numeric"),
    identifies = c("combination", "none"),
    sensitive = c(FALSE, FALSE),
    disclosure_role = c("quasi", "none"),
    simulation = c("synthesize", "synthesize"),
    postal_strategy = c("generate", NA_character_),
    postal_country = c(NA_character_, NA_character_),
    reason = c("test", "test"),
    disclosure_reason = c(NA_character_, NA_character_),
    user_identifies = c(NA_character_, NA_character_),
    user_sensitive = c(NA, NA)
  )
  state$profile <- list()
  state$attested_no_direct <- TRUE

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 1L, value = "categorical"))
    expect_true(is.na(state$roles$postal_strategy[[1]]))
    expect_true(is.na(state$roles$postal_country[[1]]))
  })
})

test_that("action options are type-aware", {
  # Free text gains both resample and scramble; neither makes sense for a
  # numeric column, and an alphanumeric ID has no meaningful resample.
  expect_identical(
    dg_action_options("free_text"),
    c("synthesize", "mask_rare", "scramble", "pass_through", "drop")
  )
  expect_true("mask_rare" %in% dg_action_options("categorical"))
  expect_false("mask_rare" %in% dg_action_options("numeric"))
  expect_false("mask_rare" %in% dg_action_options("date"))
  expect_false("mask_rare" %in% dg_action_options("postal_code"))
  expect_false("mask_rare" %in% dg_action_options("alphanumeric_id"))
  expect_false("scramble" %in% dg_action_options("numeric"))
  expect_false("synthesize" %in% dg_action_options("alphanumeric_id"))
  # Postal strategy is an action now, not a second dropdown.
  expect_true("postal_resample" %in% dg_action_options("postal_code"))
  # Every type offers a way out.
  for (role in c("alphanumeric_id", "free_text", "categorical", "numeric",
                 "date", "postal_code")) {
    options <- dg_action_options(role)
    expect_true(
      all(c("pass_through", "drop") %in% options),
      info = paste(role, "action options:", paste(options, collapse = ", "))
    )
  }
})

test_that("one stored action is labelled for the type it applies to", {
  expect_equal(dg_action_label("synthesize", "free_text"), "Resample")
  expect_equal(dg_action_label("synthesize", "categorical"), "Resample")
  expect_equal(dg_action_label("synthesize", "numeric"), "Simulate")
  expect_equal(dg_action_label("synthesize", "date"), "Simulate")
  expect_equal(dg_action_label("synthesize", "postal_code"), "Generate new")
  # Postal resample is the same action under the same name; only the stored
  # representation differs.
  expect_equal(dg_action_label("postal_resample", "postal_code"), "Resample")
  expect_equal(dg_action_label("mask_rare", "categorical"), "Resample (rare levels masked)")
  expect_equal(dg_action_label("scramble", "free_text"), "Scramble")
})

test_that("masked-resample action translates through label_strategy", {
  roles <- tibble::tibble(
    simulation = "synthesize",
    variable = "category"
  )

  masked <- dg_apply_action_token(roles, 1L, "mask_rare", "categorical")
  expect_equal(masked$simulation[[1]], "synthesize")
  expect_equal(masked$label_strategy[[1]], "mask_rare")
  expect_equal(
    action_token(masked$simulation[[1]], "categorical", label_strategy = masked$label_strategy[[1]]),
    "mask_rare"
  )

  preserved <- dg_apply_action_token(masked, 1L, "synthesize", "categorical")
  expect_equal(preserved$simulation[[1]], "synthesize")
  expect_equal(preserved$label_strategy[[1]], "preserve")
})

test_that("an action the type does not offer is rejected", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    # Row 3 ("income") is numeric. Scramble runs the identifier scrambler,
    # which is meaningless over numbers -- the dropdown never offers it, and a
    # hand-crafted browser message must not be able to set it either.
    before <- state$roles$simulation[[3]]
    session$setInputs(simulation_change = list(row = 3, value = "scramble"))
    expect_equal(state$roles$simulation[[3]], before)
  })
})

test_that("free text accepts both resample and scramble", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 2, value = "free_text"))
    expect_equal(state$roles$user_role[[2]], "free_text")

    session$setInputs(simulation_change = list(row = 2, value = "scramble"))
    expect_equal(state$roles$simulation[[2]], "scramble")

    # "Resample" is the free-text label for the stored `synthesize` action;
    # the storage vocabulary is the bundle contract and does not change.
    session$setInputs(simulation_change = list(row = 2, value = "synthesize"))
    expect_equal(state$roles$simulation[[2]], "synthesize")
  })
})

test_that("postal strategy is chosen through the action dropdown", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 2, value = "postal_code"))
    expect_equal(state$roles$postal_strategy[[2]], "generate")

    session$setInputs(simulation_change = list(row = 2, value = "postal_resample"))
    # The UI-only token is translated, never persisted: `simulation` keeps its
    # contract vocabulary and the strategy lands in its own column.
    expect_equal(state$roles$simulation[[2]], "synthesize")
    expect_equal(state$roles$postal_strategy[[2]], "resample")

    session$setInputs(simulation_change = list(row = 2, value = "synthesize"))
    expect_equal(state$roles$postal_strategy[[2]], "generate")
  })
})

test_that("a sticky action invalid for a new type is discarded", {
  testthat::skip_if_not_installed("shiny")
  state <- roles_test_state()

  shiny::testServer(mod_roles_server, args = list(state = state), {
    session$setInputs(role_change = list(row = 2, value = "free_text"))
    session$setInputs(simulation_change = list(row = 2, value = "scramble"))
    expect_equal(state$roles$user_simulation[[2]], "scramble")

    # Retyping to numeric leaves scramble meaningless. If the override
    # survived, a later Q1 answer would restore an action this type never
    # offers.
    session$setInputs(role_change = list(row = 2, value = "numeric"))
    expect_true(is.na(state$roles$user_simulation[[2]]))
    expect_true(state$roles$simulation[[2]] %in% dg_action_options("numeric"))

    session$setInputs(identifies_change = list(row = 2, value = "combination"))
    expect_true(state$roles$simulation[[2]] %in% dg_action_options("numeric"))
  })
})
})
