local({
app_test_roles <- function() {
  roles <- tibble::tibble(
    variable = c("email", "age"),
    recommended_role = c("alphanumeric ID", "numeric"),
    user_role = c(NA_character_, NA_character_),
    class = c("alphanumeric ID", "numeric"),
    identifies = c("direct", "none"),
    sensitive = c(FALSE, FALSE),
    disclosure_role = c("direct", "none"),
    simulation = c("drop", "synthesize"),
    reason = c("Likely identifies a person.", "Looks numeric."),
    disclosure_reason = c(NA_character_, NA_character_)
  )
  class(roles) <- c("dataganger_roles", class(roles))
  roles
}

test_that("entry gate defaults to FALSE, agree sets attestation, and refuse calls wrapper", {
  skip_if_not_installed("shiny")

  refused <- FALSE
  state <- shiny::reactiveValues(
    attested_no_direct = FALSE,
    raw_data = NULL,
    roles = NULL,
    fail_safe_status = "idle",
    fail_safe_flagged = data.frame(variable = character(0), reason = character(0)),
    fail_safe_upload_token = NULL
  )

  shiny::testServer(dataganger:::app_guardrail_server, args = list(
    state = state,
    app_refuse = function(...) {
      refused <<- TRUE
      invisible(NULL)
    }
  ), {
    expect_false(isTRUE(state$attested_no_direct))

    session$setInputs(agree = 1L)
    session$flushReact()
    expect_true(isTRUE(state$attested_no_direct))

    session$setInputs(refuse = 1L)
    session$flushReact()
    expect_true(refused)
  })
})

test_that("fail-safe flags suspected direct identifiers and drop or confirm resolves them", {
  skip_if_not_installed("shiny")

  state <- shiny::reactiveValues(
    attested_no_direct = TRUE,
    raw_data = data.frame(email = c("a@x.com", "b@y.com"), age = c(40L, 51L), stringsAsFactors = FALSE),
    roles = app_test_roles(),
    fail_safe_status = "idle",
    fail_safe_flagged = data.frame(variable = character(0), reason = character(0)),
    fail_safe_upload_token = NULL,
    profile = list(ok = TRUE),
    filename = "test.csv"
  )

  shiny::testServer(dataganger:::app_guardrail_server, args = list(state = state), {
    session$flushReact()
    expect_identical(state$fail_safe_status, "pending")
    expect_equal(state$fail_safe_flagged$variable, "email")

    session$setInputs(confirm_keep_flagged = 1L)
    session$flushReact()
    expect_identical(state$fail_safe_status, "ready")
    # email's recommended_role is "alphanumeric ID", so confirming keeps it
    # scrambled rather than treating it as ordinary data to synthesize.
    expect_equal(state$roles$simulation[state$roles$variable == "email"], "scramble")
    expect_equal(state$roles$identifies[state$roles$variable == "email"], "")
  })

  state2 <- shiny::reactiveValues(
    attested_no_direct = TRUE,
    raw_data = data.frame(email = c("a@x.com", "b@y.com"), age = c(40L, 51L), stringsAsFactors = FALSE),
    roles = app_test_roles(),
    fail_safe_status = "idle",
    fail_safe_flagged = data.frame(variable = character(0), reason = character(0)),
    fail_safe_upload_token = NULL,
    profile = list(ok = TRUE),
    filename = "test.csv"
  )

  shiny::testServer(dataganger:::app_guardrail_server, args = list(state = state2), {
    session$flushReact()
    session$setInputs(drop_flagged = 1L)
    session$flushReact()
    expect_identical(state2$fail_safe_status, "ready")
    expect_equal(state2$roles$simulation[state2$roles$variable == "email"], "drop")
  })
})

test_that("confirming keep on a flagged alphanumeric ID defaults to scramble, not synthesize", {
  skip_if_not_installed("shiny")

  roles <- tibble::tibble(
    variable = c("order_id", "age"),
    recommended_role = c("alphanumeric ID", "numeric"),
    user_role = c(NA_character_, NA_character_),
    class = c("character", "numeric"),
    identifies = c("direct", "none"),
    sensitive = c(FALSE, FALSE),
    disclosure_role = c("direct", "none"),
    simulation = c("scramble", "synthesize"),
    reason = c("Values mix letters and digits in a consistent pattern.", "Looks numeric."),
    disclosure_reason = c(NA_character_, NA_character_)
  )
  class(roles) <- c("dataganger_roles", class(roles))

  state <- shiny::reactiveValues(
    attested_no_direct = TRUE,
    raw_data = data.frame(order_id = c("OR-0001-01", "OR-0002-02"), age = c(40L, 51L), stringsAsFactors = FALSE),
    roles = roles,
    fail_safe_status = "idle",
    fail_safe_flagged = data.frame(variable = character(0), reason = character(0)),
    fail_safe_upload_token = NULL,
    profile = list(ok = TRUE),
    filename = "test.csv"
  )

  shiny::testServer(dataganger:::app_guardrail_server, args = list(state = state), {
    session$flushReact()
    expect_equal(state$fail_safe_flagged$variable, "order_id")

    session$setInputs(confirm_keep_flagged = 1L)
    session$flushReact()
    expect_identical(state$fail_safe_status, "ready")
    expect_equal(state$roles$simulation[state$roles$variable == "order_id"], "scramble")
  })
})
})
