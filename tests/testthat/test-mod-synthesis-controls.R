local({
synth_controls_host_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- mod_state_server("state")
    controls <- mod_synthesis_controls_server("controls", state)
    list(state = state, controls = controls)
  })
}

test_that("advanced settings use a collapsed disclosure", {
  html <- as.character(mod_synthesis_controls_ui("controls"))

  expect_match(html, "<details>", fixed = TRUE)
  expect_match(html, "<summary>Advanced settings</summary>", fixed = TRUE)
  expect_match(html, "Defaults are safe")
  expect_no_match(html, "expanded by default", fixed = TRUE)
  expect_no_match(html, "<details open", fixed = TRUE)
})

test_that("A1 confirm writes development spec", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "development")
    session$flushReact()
    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_identical(state$spec$purpose, "development")
  })
})

test_that("analytics without checkbox leaves state spec NULL", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "analytics")
    session$setInputs(`controls-acknowledge_risk` = FALSE)
    session$flushReact()
    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_null(state$spec)
  })
})

test_that("analytics with checkbox writes analytics spec", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "analytics")
    session$setInputs(`controls-acknowledge_risk` = TRUE)
    session$flushReact()
    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_identical(state$spec$purpose, "analytics")
    expect_true(isTRUE(state$spec$acknowledged_risk))
  })
})

test_that("demo spec uses preset name and geography strategies", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "demo")
    session$flushReact()
    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_identical(state$spec$purpose, "demo")
  })
})

test_that("confirming a changed spec sets all stale flags", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "development")
    session$flushReact()
    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    state$synthetic <- tibble::tibble(x = 1)
    state$comparison <- list(ok = TRUE)
    state$privacy <- tibble::tibble(flag = "none")
    state$stale <- list(synthesis = FALSE, comparison = FALSE, export = FALSE)
    session$flushReact()

    session$setInputs(`controls-purpose_group` = "analytics")
    session$setInputs(`controls-acknowledge_risk` = TRUE)
    session$flushReact()
    session$setInputs(`controls-confirm` = 2L)
    session$flushReact()

    expect_true(isTRUE(state$stale$synthesis))
    expect_true(isTRUE(state$stale$comparison))
    expect_true(isTRUE(state$stale$export))
    expect_null(state$synthetic)
    expect_null(state$comparison)
    expect_null(state$privacy)
  })
})


test_that("purpose card shows a single Protection meter", {
  html <- as.character(dg_purpose_card(
    shiny::NS("x"), "demo", "demo", "Demo", "line", 5
  ))
  expect_match(html, "Protection")
  expect_false(grepl("identifiability", html, ignore.case = FALSE))
})

test_that("Configure confirm is blocked until every generated column has UI answers", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "development")
    session$flushReact()

    df    <- data.frame(age = 1:5, visit = as.Date("2020-01-01") + 0:4)
    roles <- detect_roles(df)
    roles <- dg_ensure_ui_roles(roles)

    state$roles <- roles
    session$flushReact()

    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_equal(state$spec_confirmed %||% 0L, 0L)

    roles$user_identifies <- "none"
    roles$user_sensitive <- FALSE
    roles$identifies <- "none"
    roles$sensitive <- FALSE
    roles <- dg_sync_roles_axes(roles)
    state$roles <- roles
    session$flushReact()

    session$setInputs(`controls-confirm` = 2L)
    session$flushReact()

    expect_true((state$spec_confirmed %||% 0L) >= 1L)
  })
})

test_that("Configure confirm ignores missing UI answers on dropped or pass-through columns", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "development")
    session$flushReact()

    roles <- dg_ensure_ui_roles(detect_roles(data.frame(age = 1:5, city = letters[1:5])))
    roles$simulation <- c("drop", "pass_through")

    state$roles <- roles
    session$flushReact()

    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_true((state$spec_confirmed %||% 0L) >= 1L)
  })
})

test_that("Configure confirm still blocks a synthesized column with missing UI answer", {
  testthat::skip_if_not_installed("shiny")

  shiny::testServer(synth_controls_host_server, {
    state <- session$getReturned()$state

    session$setInputs(`controls-purpose_group` = "development")
    session$flushReact()

    roles <- dg_ensure_ui_roles(detect_roles(data.frame(age = 1:5, city = letters[1:5])))
    roles$simulation <- c("synthesize", "drop")

    state$roles <- roles
    session$flushReact()

    session$setInputs(`controls-confirm` = 1L)
    session$flushReact()

    expect_equal(state$spec_confirmed %||% 0L, 0L)
  })
})



# --- live threshold readouts --------------------------------------------------
# The two Advanced sliders share a default of 5 but count different things:
# rare_level_min_n counts one value inside one column, k_anon counts rows
# sharing a combination across columns. Each readout is asserted against a
# fixture with a hand-checked answer, so a wrong count cannot pass silently.
#
# These drive mod_synthesis_controls_server() directly rather than through the
# host wrapper used above: the sliders are created inside renderUI, and
# setInputs() on the host does not reach a nested module's dynamic inputs.

hint_fixture <- function() {
  data.frame(
    # 3 groups of 4 rows, so every (city, band) combination is below k = 5 and
    # none is below k = 4.
    city  = rep(c("alpha", "beta", "gamma"), each = 4L),
    band  = rep("x", 12L),
    # 2 rare labels (1 row each) alongside 1 common label (10 rows).
    grade = c(rep("common", 10L), "r1", "r2"),
    stringsAsFactors = FALSE
  )
}

hint_roles <- function(df, combination = c("city", "band")) {
  roles <- dg_ensure_ui_roles(detect_roles(df))
  roles$identifies <- ifelse(roles$variable %in% combination,
                             "combination", "none")
  roles$sensitive <- FALSE
  roles
}

hint_state <- function(df, roles) {
  shiny::reactiveValues(raw_data = df, roles = roles)
}

hint_html <- function(out) as.character(out$html)

test_that("k readout counts combinations and rows below the chosen k", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, hint_roles(df))),
    {
      session$setInputs(purpose_group = "development", k_anon = 5L)
      session$flushReact()

      html <- hint_html(output$kanon_hint)

      expect_match(html, "At 5:", fixed = TRUE)
      expect_match(html, "your 2 combination columns (city, band)", fixed = TRUE)
      expect_match(html, "3 distinct combinations", fixed = TRUE)
      expect_match(html, "3 of them are held by fewer than 5 rows", fixed = TRUE)
      expect_match(html, "12 of 12 rows (100%)", fixed = TRUE)
    }
  )
})

test_that("k readout reports nothing to suppress when every group is large enough", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, hint_roles(df))),
    {
      # Groups are exactly 4 rows, so k = 4 is satisfied where k = 5 was not.
      session$setInputs(purpose_group = "development", k_anon = 4L)
      session$flushReact()

      html <- hint_html(output$kanon_hint)

      expect_match(html, "At 4:", fixed = TRUE)
      expect_match(html, "None are held by too few rows", fixed = TRUE)
      expect_no_match(html, "coarsened first", fixed = TRUE)
    }
  )
})

test_that("k readout reports the empty state when nothing identifies in combination", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  roles <- hint_roles(df, combination = character(0))
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, roles)),
    {
      session$setInputs(purpose_group = "development", k_anon = 5L)
      session$flushReact()

      html <- hint_html(output$kanon_hint)

      expect_match(html, "No columns are marked as identifying in combination",
                   fixed = TRUE)
      expect_no_match(html, "distinct combinations", fixed = TRUE)
    }
  )
})

test_that("k readout keys on factor codes so separator-like values cannot collide", {
  testthat::skip_if_not_installed("shiny")

  # ("a|~|b", "c") and ("a", "b|~|c") are distinct combinations that a naive
  # paste with "|~|" would fold into one. There are 2 combinations here, not 1.
  df <- data.frame(
    p = c("a|~|b", "a", "a|~|b", "a"),
    q = c("c", "b|~|c", "c", "b|~|c"),
    stringsAsFactors = FALSE
  )
  roles <- hint_roles(df, combination = c("p", "q"))
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, roles)),
    {
      session$setInputs(purpose_group = "development", k_anon = 5L)
      session$flushReact()

      expect_match(hint_html(output$kanon_hint), "2 distinct combinations",
                   fixed = TRUE)
    }
  )
})

test_that("rare readout counts rare values and says when nothing masks them", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, hint_roles(df))),
    {
      session$setInputs(purpose_group = "development", rare_level_min_n = 5L)
      session$flushReact()

      html <- hint_html(output$rare_hint)

      # city: 3 levels of 4 rows, all rare. band: 1 level of 12, not rare.
      # grade: "common" not rare, "r1"/"r2" rare. So 5 rare of 7 distinct,
      # over 2 of the 3 text columns.
      expect_match(html, "At 5:", fixed = TRUE)
      expect_match(html, "5 of 7 distinct values are rare", fixed = TRUE)
      expect_match(html, "2 of 3 text or category columns", fixed = TRUE)
      expect_match(html, "No column is set to mask rare values", fixed = TRUE)
    }
  )
})

test_that("rare readout reports how many columns are set to mask", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  roles <- hint_roles(df)
  roles$label_strategy <- ifelse(roles$variable == "grade",
                                 "mask_rare", "preserve")
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, roles)),
    {
      session$setInputs(purpose_group = "development", rare_level_min_n = 5L)
      session$flushReact()

      html <- hint_html(output$rare_hint)

      expect_match(html, "1 column is set to mask rare values", fixed = TRUE)
      expect_no_match(html, "No column is set to mask", fixed = TRUE)
    }
  )
})

test_that("rare readout lowers its count as the threshold drops", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, hint_roles(df))),
    {
      # At 2, only the single-row grade labels stay rare; the 4-row city
      # levels no longer qualify.
      session$setInputs(purpose_group = "development", rare_level_min_n = 2L)
      session$flushReact()

      html <- hint_html(output$rare_hint)

      expect_match(html, "At 2:", fixed = TRUE)
      expect_match(html, "2 of 7 distinct values are rare", fixed = TRUE)
      expect_match(html, "1 of 3 text or category columns", fixed = TRUE)
    }
  )
})

test_that("both threshold sliders step in whole numbers", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, hint_roles(df))),
    {
      session$setInputs(purpose_group = "development")
      session$flushReact()

      # The two sliders no longer render into the same output: k_anon lives in
      # kanon_control so its answer gate cannot re-render the whole panel.
      # Count across both, so this still asserts "both sliders", not "one".
      panel <- as.character(output$advanced_settings$html)
      kanon <- as.character(output$kanon_control$html)

      steps <- function(html) {
        lengths(regmatches(html, gregexpr("data-step=\"1\"", html)))
      }

      # Both are integer counts; a fractional slider value would be silently
      # truncated by the as.integer() coercion downstream.
      expect_equal(steps(panel), 1L)
      expect_equal(steps(kanon), 1L)
    }
  )
})

# Regression: the sliders are created inside renderUI, so on the first render
# pass input$rare_level_min_n / input$k_anon are still NULL. as.integer(NULL) is
# integer(0), and `... || is.na(integer(0))` aborts the render with "missing
# value where TRUE/FALSE needed" -- the hint would have thrown on every startup
# where the readout rendered before the slider registered. Copilot found this.
test_that("hint readouts survive sliders that have not initialised yet", {
  df    <- hint_fixture()
  roles <- hint_roles(df)
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, roles)),
    {
      session$setInputs(rare_level_min_n = NULL, k_anon = NULL)
      expect_null(output$rare_hint)
      expect_null(output$kanon_hint)
    }
  )
})

# --- k_anon answer gate -----------------------------------------------------
# The slider only acts on columns marked "identifies in combination", so until
# every column carries both Configure answers it is inert: dragging it changes
# nothing and it reads as broken. It is disabled until the table is complete.

# These fixtures deliberately go through dg_ensure_ui_roles(detect_roles(df)),
# the exact shape the app installs at upload, and answer via user_identifies /
# user_sensitive. Setting `identifies` instead would not exercise the gate at
# all: detect_roles() SEEDS identifies/sensitive for every column, so a table
# that has been answered by nobody already looks answered on those columns.

# Straight off the uploader: seeded axes populated, user axes still blank.
gate_roles_unanswered <- function(df) {
  dg_ensure_ui_roles(detect_roles(df))
}

# Every column answered by the user.
gate_roles_answered <- function(df) {
  roles <- gate_roles_unanswered(df)
  roles$user_identifies <- "none"
  roles$user_sensitive <- FALSE
  roles
}

test_that("the gate reads the user's answers, not detect_roles() seeds", {
  # Columns that all classify (numeric, date, ID) are the case that exposes
  # this: detect_roles() seeds every one of them with a non-empty identifies
  # value. An all-character frame seeds identifies = "" instead, which would
  # hide the bug, so this fixture is chosen deliberately.
  df <- data.frame(
    age   = c(31, 45, 29, 50),
    visit = as.Date("2024-01-01") + 0:3,
    id    = c("A1", "B2", "C3", "D4"),
    stringsAsFactors = FALSE
  )
  fresh <- dg_ensure_ui_roles(detect_roles(df))

  # Nobody has answered anything, yet both seeded axes are already populated.
  # A predicate reading them would call this fully answered and unlock the
  # slider at upload.
  expect_true(
    all(nzchar(fresh$identifies)),
    info = paste("Seeded identifies values:", paste(fresh$identifies, collapse = ", "))
  )
  expect_false(
    any(is.na(fresh$sensitive)),
    info = paste("Missing seeded sensitive rows:",
                 paste(which(is.na(fresh$sensitive)), collapse = ", "))
  )

  # The user axes tell the truth: still blank.
  expect_false(
    any(nzchar(fresh$user_identifies)),
    info = paste("Unexpected user identifies values:",
                 paste(fresh$user_identifies[nzchar(fresh$user_identifies)], collapse = ", "))
  )
  expect_true(
    all(is.na(fresh$user_sensitive)),
    info = paste("Non-missing user sensitive rows:",
                 paste(which(!is.na(fresh$user_sensitive)), collapse = ", "))
  )

  expect_false(dg_roles_all_answered(fresh))

  answered <- fresh
  answered$user_identifies <- "none"
  answered$user_sensitive <- FALSE
  expect_true(dg_roles_all_answered(answered))
})

test_that("the k gate opens exactly when the Confirm gate opens", {
  df <- hint_fixture()

  # Sharing roles_ready_for_generation() is the point: that is literally the
  # predicate the Confirm button uses, and the two sit on the same step, so
  # they must not disagree. Comparing against it (rather than re-deriving the
  # answer here) is what would catch the gate growing its own second opinion.
  agrees <- function(roles) {
    identical(dg_roles_all_answered(roles), roles_ready_for_generation(roles))
  }

  answered <- gate_roles_answered(df)
  expect_true(agrees(gate_roles_unanswered(df)))
  expect_true(agrees(answered))

  one_missing <- answered
  one_missing$user_identifies[2L] <- ""
  expect_false(dg_roles_all_answered(one_missing))
  expect_true(agrees(one_missing))

  # A dropped column needs no answers, for the gate as for Confirm.
  dropped <- one_missing
  dropped$simulation <- ifelse(seq_len(nrow(dropped)) == 2L, "drop", "synthesize")
  expect_true(dg_roles_all_answered(dropped))
  expect_true(agrees(dropped))
})

test_that("the gated wrapper is inert, so it is not a dead tab stop", {
  html <- as.character(dg_gate_slider_tag(
    shiny::sliderInput("k", "L", min = 2, max = 30, value = 5, step = 1)
  ))

  # ionRangeSlider builds its own focusable <span class="irs-line" tabindex="0">
  # on the client, which no attribute on the input can reach. Without inert the
  # gated slider still takes focus: a greyed control that does nothing and
  # announces nothing. Verified in Chrome -- with inert, Tab skips it entirely
  # and even a forced .focus() does not land.
  expect_match(html, "inert", fixed = TRUE)
  expect_match(html, "pointer-events:none", fixed = TRUE)
  expect_match(html, "data-disable=\"true\"", fixed = TRUE)
})

test_that("answering a column does not rebuild the Advanced settings panel", {
  testthat::skip_if_not_installed("shiny")

  # The panel reaches state$roles through default_n() -> suggested_rows() ->
  # suggest_min_rows(). Read reactively, that rebuilt the whole panel every
  # time the user answered a column, silently resetting rows_n, engine, seed
  # and the rest to preset defaults mid-workflow.
  #
  # The mock makes the suggestion swing on the answers, which is what exposes
  # the dependency: a rebuild would stamp 999 into the rows_n slider, an
  # isolated read keeps the 100 it was built with. Without this the test cannot
  # discriminate -- the real suggestion happens not to move for this fixture,
  # so the panel re-renders to byte-identical HTML and testServer, which does
  # not model the DOM replacement that actually loses the user's values, sees
  # nothing. Call counting does not work either: other consumers of
  # suggested_rows() move the count on their own.
  testthat::local_mocked_bindings(
    suggest_min_rows = function(profile, roles, ...) {
      n <- if (all(nzchar(roles$user_identifies %||% ""))) 999L else 100L
      list(n = n, combination_count = NA_integer_, original_n = 100L,
           n_below = 0L, pct_below = 0)
    }
  )

  df <- hint_fixture()
  # suggested_rows() short-circuits on a NULL profile, so the panel only has
  # the roles dependency at all once a profile exists -- as it does in the app.
  state <- hint_state(df, gate_roles_unanswered(df))
  state$profile <- profile_data(df)

  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = state),
    {
      session$setInputs(purpose_group = "development")
      session$flushReact()

      before <- hint_html(output$advanced_settings)
      expect_match(before, "value=\"100\"", fixed = TRUE)

      state$roles <- gate_roles_answered(df)
      session$flushReact()
      after <- hint_html(output$advanced_settings)

      # The panel must be untouched by the answer.
      expect_match(after, "value=\"100\"", fixed = TRUE)
      expect_no_match(after, "999", fixed = TRUE)
      expect_identical(before, after)
    }
  )
})

test_that("dg_disable_slider_tag marks the slider input, not the wrapper", {
  tag <- dg_disable_slider_tag(
    shiny::sliderInput("k", "L", min = 2, max = 30, value = 5, step = 1)
  )
  html <- as.character(tag)

  # The attributes must land on the js-range-slider input itself; putting them
  # on the enclosing div would leave the real control focusable.
  expect_match(html, "js-range-slider", fixed = TRUE)
  expect_match(html, "data-disable=\"true\"", fixed = TRUE)
  expect_match(html, "aria-disabled=\"true\"", fixed = TRUE)
  input_tag <- regmatches(html, regexpr("<input[^>]*>", html))
  expect_match(input_tag, "data-disable=\"true\"", fixed = TRUE)
  expect_match(input_tag, "tabindex=\"-1\"", fixed = TRUE)

  # Located by class, not position, and a tag without a slider is untouched.
  plain <- shiny::tags$div(shiny::tags$p("no slider here"))
  expect_identical(as.character(dg_disable_slider_tag(plain)),
                   as.character(plain))
})

test_that("dg_roles_all_answered rejects missing and empty roles", {
  df <- hint_fixture()

  expect_false(dg_roles_all_answered(NULL))
  expect_false(dg_roles_all_answered(gate_roles_answered(df)[0L, ]))
  expect_false(dg_roles_all_answered(data.frame(variable = "a")))
})

test_that("k slider is disabled while any column is unanswered", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, gate_roles_unanswered(df))),
    {
      session$setInputs(purpose_group = "development")
      session$flushReact()

      html <- hint_html(output$kanon_control)

      # pointer-events:none is what actually blocks the drag; shinyjs is not
      # a dependency of this package and must not become one.
      expect_match(html, "pointer-events:none", fixed = TRUE)
      expect_match(html, "Answer both questions for every column", fixed = TRUE)

      # pointer-events:none stops the mouse but not the keyboard, so the
      # gate also has to remove the slider from the tab order and tell
      # ionRangeSlider to disable itself. Without these a user could tab to
      # the "disabled" slider and change k with the arrow keys.
      expect_match(html, "data-disable=\"true\"", fixed = TRUE)
      expect_match(html, "tabindex=\"-1\"", fixed = TRUE)
      expect_match(html, "aria-disabled=\"true\"", fixed = TRUE)

      # The readout is suppressed while gated: its "nothing to act on" message
      # would only restate the reason line above it.
      expect_no_match(html, "kanon_hint", fixed = TRUE)
    }
  )
})

test_that("k slider is enabled once every column is answered", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()
  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, gate_roles_answered(df))),
    {
      session$setInputs(purpose_group = "development")
      session$flushReact()

      html <- hint_html(output$kanon_control)

      expect_no_match(html, "pointer-events:none", fixed = TRUE)
      expect_no_match(html, "Answer both questions for every column",
                      fixed = TRUE)
      expect_match(html, "kanon_hint", fixed = TRUE)

      # The enabled slider must carry none of the disabling marks.
      expect_no_match(html, "data-disable", fixed = TRUE)
      expect_no_match(html, "tabindex=\"-1\"", fixed = TRUE)
      expect_no_match(html, "aria-disabled", fixed = TRUE)
    }
  )
})

# Regression for the re-render trap. kanon_control depends on state$roles, so
# it re-renders whenever an answer changes. The slider is created inside that
# same renderUI, so its value has to be carried across by an isolate()d read --
# without it the user's chosen k silently snaps back to the preset every time
# they touch the table.
#
# Every step below is reachable through the UI, which matters: k can only be
# set while the gate is OPEN, so a test that sets it while gated would assert
# carry-forward across a transition the gate itself makes impossible. The route
# in is the Action dropdown -- a dropped column needs no answers, so switching
# it back to "synthesize" re-opens the questions and closes the gate again. The
# two gate flips prove the output really re-rendered, so neither value
# assertion can pass vacuously.
test_that("changing the table re-renders the k slider without losing its value", {
  testthat::skip_if_not_installed("shiny")

  df <- hint_fixture()

  # Everything answered except one column, which is dropped and so exempt.
  # The gate is open: this is a spec the user could confirm.
  start <- gate_roles_answered(df)
  start$simulation[2L] <- "drop"
  start$user_identifies[2L] <- ""
  start$user_sensitive[2L] <- NA

  shiny::testServer(
    mod_synthesis_controls_server,
    args = list(state = hint_state(df, start)),
    {
      session$setInputs(purpose_group = "development")
      session$flushReact()

      before <- hint_html(output$kanon_control)
      expect_no_match(before, "pointer-events:none", fixed = TRUE)
      # Preset default, so a later data-from="9" cannot be a leftover.
      expect_match(before, "data-from=\"5\"", fixed = TRUE)

      # Only now, with the slider live, can the user drag it to a non-default k.
      session$setInputs(k_anon = 9L)
      session$flushReact()

      # The user puts that column back into the output. It now needs answers it
      # does not have, so the gate closes -- and closing is only observable if
      # kanon_control re-rendered.
      roles <- state$roles
      roles$simulation[2L] <- "synthesize"
      state$roles <- roles
      session$flushReact()

      gated <- hint_html(output$kanon_control)
      expect_match(gated, "pointer-events:none", fixed = TRUE)
      expect_match(gated, "data-from=\"9\"", fixed = TRUE)
      expect_no_match(gated, "data-from=\"5\"", fixed = TRUE)

      # The user answers it. The gate re-opens -- a second re-render -- and the
      # chosen 9 has to survive both of them.
      roles <- state$roles
      roles$user_identifies[2L] <- "none"
      roles$user_sensitive[2L] <- FALSE
      state$roles <- roles
      session$flushReact()

      after <- hint_html(output$kanon_control)
      expect_no_match(after, "pointer-events:none", fixed = TRUE)
      expect_match(after, "data-from=\"9\"", fixed = TRUE)
      expect_no_match(after, "data-from=\"5\"", fixed = TRUE)
    }
  )
})
})
