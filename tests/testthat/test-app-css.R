local({
# tests/testthat/test-app-css.R

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")

library(shinytest2)

# Chrome cannot open its own sandbox inside a container or CI runner. Harmless
# on a desktop, where a headless throwaway session does not need the sandbox.
chromote::set_chrome_args(c(
  chromote::default_chrome_args(),
  "--no-sandbox",
  "--disable-dev-shm-usage"
))

test_that("design system CSS loads and tokens are applied", {
  testthat::skip_if(
    dataganger:::synthesis_dev_loaded(),
    "shinytest2 subprocess requires an installed package; skipping under devtools::load_all()"
  )
  # AppDriver$new() below raises skip_on_cran() itself, but it does so three
  # lines after Chromote$new() has already tried to launch a browser. On a
  # machine with chromote installed and no Chrome binary -- the likely CRAN
  # configuration, since chromote is a CRAN package and Chrome is not -- the
  # launch errors before the skip is ever reached, turning an untestable
  # environment into a check ERROR. Guard before anything launches.
  testthat::skip_on_cran()
  testthat::skip_if(
    is.null(chromote::find_chrome()),
    "no Chrome binary available for the browser harness"
  )
  # chromote waits getOption("chromote.timeout", 10) seconds for the debugging
  # port and then aborts with error_stop_port_search. Ten seconds is not enough
  # on a cold CI runner (observed on windows-latest and ubuntu-devel,
  # 2026-08-05). This is the launch window; default_timeout below is a
  # different clock, covering responses to individual CDP commands.
  withr::local_options(chromote.timeout = 60)
  # 60s because the app loads bslib, DT and the full module tree before it is
  # ready, which exceeds 15s on a cold cache.
  #
  # Known limitation: on some Linux setups AppDriver still fails here with
  # "Chromote: timed out waiting for response to command Page.navigate", and
  # raising this timeout does not help -- a standalone ChromoteSession can
  # navigate to the same running app, so the fault is in AppDriver's shiny
  # subprocess plus chromote child-loop combination, not in the app. Use
  # tests/manual/verify-app-browser.R to confirm the app in a real browser when
  # this harness cannot start.
  # Chrome writes scratch directories (com.google.Chrome.*,
  # com.google.Chrome.scoped_dir.*) straight into TMPDIR and does not remove
  # them, which R CMD check reports as "checking for detritus in the temp
  # directory ... NOTE". Chrome inherits TMPDIR from this process when chromote
  # spawns it, so pointing it at R's own session temp directory puts that
  # scratch inside RtmpXXXX, which R deletes when the session ends. R's
  # tempdir() is fixed at startup and is not itself moved by this. TMP/TEMP are
  # set alongside TMPDIR because Windows Chrome reads those instead.
  withr::local_envvar(
    TMPDIR = tempdir(), TMP = tempdir(), TEMP = tempdir()
  )
  chromote_client <- chromote::Chromote$new()
  chromote_client$default_timeout <- 60
  chromote::set_default_chromote_object(chromote_client)

  app <- AppDriver$new(
    system.file("app", package = "dataganger"),
    name = "css-check",
    height = 800,
    width = 1200,
    load_timeout = 60000,
    timeout = 60000
  )
  on.exit(app$stop())

  # 1. CSS files served -- check network resources loaded
  html <- app$get_html("html")
  expect_true(grepl("www/colors_and_type.css", html),
              label = "colors_and_type.css link tag present in HTML")
  expect_true(grepl("www/shiny-app.css", html),
              label = "shiny-app.css link tag present in HTML")

  # 2. Background token applied -- body or #app background should be warm off-white
  bg <- app$get_js(
    "getComputedStyle(document.body).backgroundColor"
  )
  # --paper-50: #FBFAF6 = rgb(251, 250, 246)
  expect_equal(bg, "rgb(251, 250, 246)",
               label = "body background matches --paper-50 token")

  # 3. Body font is Inter
  font <- app$get_js(
    "getComputedStyle(document.body).fontFamily"
  )
  expect_true(grepl("Inter", font, ignore.case = TRUE),
              label = "body font includes Inter")
})
})
