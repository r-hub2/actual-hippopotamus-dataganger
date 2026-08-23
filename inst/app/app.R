# DataGangeR Shiny App
# Loaded by run_app() via shiny::runApp(system.file("app", package = "dataganger"))
# Do not add roxygen tags -- this is not a package source file.

pkgload_available <- requireNamespace("pkgload", quietly = TRUE)
if (pkgload_available && pkgload::is_dev_package("dataganger")) {
  pkgload::load_all(quiet = TRUE)
}

library(shiny)
library(bslib)

# Serve www/ assets explicitly so the stylesheets resolve regardless of how
# the app dir is mounted. Must run before any UI is defined.
www_dir <- system.file("app/www", package = "dataganger")
shiny::addResourcePath("www", www_dir)

# Cache-bust stylesheets by appending the file's mtime as a query string, so
# the browser is forced to re-fetch whenever the CSS changes (no manual
# hard-refresh required).
css_href <- function(file) {
  path <- file.path(www_dir, file)
  ver <- if (file.exists(path)) as.integer(file.info(path)$mtime) else 0L
  sprintf("www/%s?v=%d", file, ver)
}

detect_roles <- dataganger::detect_roles
dg_timeit <- dataganger:::dg_timeit
mod_compare_server <- dataganger:::mod_compare_server
mod_compare_ui <- dataganger:::mod_compare_ui
mod_export_server <- dataganger:::mod_export_server
mod_export_ui <- dataganger:::mod_export_ui
mod_generate_server <- dataganger:::mod_generate_server
mod_generate_ui <- dataganger:::mod_generate_ui
mod_roles_server <- dataganger:::mod_roles_server
mod_roles_ui <- dataganger:::mod_roles_ui
mod_state_server <- dataganger:::mod_state_server
mod_synthesis_controls_server <- dataganger:::mod_synthesis_controls_server
mod_synthesis_controls_objective_ui <- dataganger:::mod_synthesis_controls_objective_ui
mod_synthesis_controls_spec_ui <- dataganger:::mod_synthesis_controls_spec_ui
mod_upload_server <- dataganger:::mod_upload_server
mod_upload_ui <- dataganger:::mod_upload_ui
mod_column_filter_server <- dataganger:::mod_column_filter_server
mod_data_panel_server <- dataganger:::mod_data_panel_server
mod_data_panel_ui <- dataganger:::mod_data_panel_ui
dg_sync_roles_axes <- dataganger:::dg_sync_roles_axes
dg_ensure_ui_roles <- dataganger:::dg_ensure_ui_roles
suspected_direct_identifiers <- dataganger:::suspected_direct_identifiers
app_fail_safe_empty <- dataganger:::app_fail_safe_empty
app_guardrail_server <- dataganger:::app_guardrail_server

dg_theme <- bslib::bs_theme(
  version = 5,
  bg = "#FBFAF6",
  fg = "#11140F",
  primary = "#D43A8A",
  secondary = "#4F7D32",
  danger = "#C76B12",
  base_font = "Inter",
  heading_font = "Instrument Serif",
  code_font = "JetBrains Mono",
  font_scale = 1
)

# Sidebar nav step helper
step_item <- function(num, label, input_id) {
  tags$li(
    class = "step locked",
    id = paste0("step-", input_id),
    `data-step` = input_id,
    onclick = sprintf(
      "if (!this.classList.contains('locked')) { Shiny.setInputValue('nav_go', '%s', {priority: 'event'}); }",
      input_id
    ),
    tags$span(class = "num", sprintf("%02d", num)),
    tags$span(class = "label", label),
    tags$span(class = "check", "\u2713"),
    tags$span(class = "lock-icon", "\U0001F512")
  )
}

report_issue_modal <- function(url, body) {
  shiny::modalDialog(
    title = "Report a problem",
    shiny::tags$p(
      "Nothing is sent automatically. Copy the pre-filled URL or issue body into your browser."
    ),
    shiny::tags$label(`for` = "report_issue_url", "GitHub issue URL"),
    shiny::tags$textarea(
      id = "report_issue_url",
      class = "form-control",
      readonly = "readonly",
      onclick = "this.select();",
      onfocus = "this.select();",
      style = "width:100%; min-height:96px; font-family:var(--font-mono, monospace);",
      url
    ),
    shiny::tags$div(style = "height:12px;"),
    shiny::tags$label(`for` = "report_issue_body", "Issue body"),
    shiny::tags$textarea(
      id = "report_issue_body",
      class = "form-control",
      readonly = "readonly",
      onclick = "this.select();",
      onfocus = "this.select();",
      style = "width:100%; min-height:260px; font-family:var(--font-mono, monospace); white-space:pre;",
      body
    ),
    footer = shiny::modalButton("Close"),
    easyClose = TRUE,
    size = "l"
  )
}

sidebar_content <- tags$nav(
  class = "sidebar",
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setActiveStep', function(tab) {
        document.querySelectorAll('.step').forEach(function(el) {
          el.classList.remove('active');
        });
        var active = document.getElementById('step-' + tab);
        if (active) active.classList.add('active');
      });
      Shiny.addCustomMessageHandler('setDoneStep', function(stepId) {
        var el = document.getElementById('step-' + stepId);
        if (el) el.classList.add('done');
      });
      Shiny.addCustomMessageHandler('unlockStep', function(stepId) {
        var el = document.getElementById('step-' + stepId);
        if (el) el.classList.remove('locked');
      });

      function DGsetPurpose(el, group, key, isProto) {
        document.querySelectorAll('.purpose-card').forEach(function(c){ c.classList.remove('selected'); });
        el.classList.add('selected');
        Shiny.setInputValue('synthesis_controls-purpose_group', group, {priority: 'event'});
        Shiny.setInputValue('synthesis_controls-purpose_chosen', true, {priority: 'event'});
        // Move the detail block under the specific selected card.
        var host = document.getElementById('synthesis_controls-purpose_detail_host');
        var slot = el.querySelector('.pc-detail-slot');
        if (host && slot) { slot.appendChild(host); }
      }
      window.DGsetPurpose = DGsetPurpose;

      // k+/-1 navigation: only adjacent steps are clickable
      var STEP_ORDER = ['upload','objective','configure','generate','compare','export'];
      Shiny.addCustomMessageHandler('setCurrentStep', function(data) {
        var cur = data.current;   // 0-based index into STEP_ORDER
        var max = data.max;       // 0-based furthest reached
        STEP_ORDER.forEach(function(id, i) {
          var el = document.getElementById('step-' + id);
          if (!el) return;
          var isActive     = i === cur;
          var isAdjacent   = Math.abs(i - cur) === 1 && i <= max;
          var isAccessible = isActive || isAdjacent;
          el.classList.toggle('active', isActive);
          el.classList.toggle('locked', !isAccessible);
        });
      });

      // Drag-to-resize between main and data panel
      var _resizeInited = false;
      function initResizeHandle() {
        if (_resizeInited) return;
        var handle = document.getElementById('resize-handle');
        var shell  = document.getElementById('app-shell');
        if (!handle || !shell) return;
        _resizeInited = true;
        var dragging = false;
        function stopDrag() { dragging = false; document.body.style.cursor = ''; }
        handle.addEventListener('mousedown', function(e) {
          dragging = true;
          document.body.style.cursor = 'col-resize';
          e.preventDefault();
        });
        document.addEventListener('mousemove', function(e) {
          if (!dragging) return;
          var rect = shell.getBoundingClientRect();
          var newW = Math.max(240, Math.min(900, rect.right - e.clientX));
          shell.style.gridTemplateColumns = '200px 1fr 5px ' + newW + 'px';
        });
        document.addEventListener('mouseup', stopDrag);
        // Cancel drag if mouse leaves the browser window
        document.addEventListener('mouseleave', stopDrag);
      }
      document.addEventListener('DOMContentLoaded', initResizeHandle);
      $(document).on('shiny:connected', initResizeHandle);
    "))
  ),
  tags$div(
    class = "brand",
    tags$img(src = "www/logomark.svg", alt = ""),
    tags$div(
      class = "wordmark",
      tags$span(
        class = "name",
        "DataGange", tags$span(class = "r", "R")
      ),
      tags$span(class = "version", paste0("v", utils::packageVersion("dataganger")))
    )
  ),
  tags$div(class = "section-label", "Workflow"),
  tags$ul(
    class = "steps",
    step_item(1, "Upload data", "upload"),
    step_item(2, "Objective", "objective"),
    step_item(3, "Configuration", "configure"),
    step_item(4, "Generation", "generate"),
    step_item(5, "Comparison", "compare"),
    step_item(6, "Export", "export")
  ),
  tags$div(
    style = "margin-top:auto; padding-top:16px; border-top:1px solid var(--border);",
    actionButton(
      "report_issue", "\u2709 Report a problem",
      class = "btn btn-sm btn-ghost",
      style = "width:100%;"
    ),
    tags$div(style = "height:8px;"),
    actionButton(
      "reset_all", "\u21ba Start over",
      class = "btn btn-sm btn-ghost",
      style = "width:100%;"
    )
  )
)

configure_ui <- function() {
  shiny::tagList(
    shiny::tags$header(
      class = "main-header",
      shiny::tags$div(
        class = "main-header-text",
        shiny::tags$span(class = "eyebrow", "Step 03 \u00b7 Configure"),
        shiny::tags$h1("Configure synthesis"),
        shiny::tags$p(
          class = "subtitle",
          "Review column roles, then adjust synthesis settings only if needed. ",
          shiny::tags$strong("Defaults are safe"),
          " for the objective you selected."
        ),
        shiny::tags$p(
          class = "subtitle scroll-hint",
          shiny::tags$span(class = "scroll-hint-glyph", "\u2193"),
          " Scroll down for advanced settings and the data summary."
        )
      ),
      shiny::tags$div(
        class = "main-header-action",
        shiny::actionButton(
          shiny::NS("synthesis_controls")("confirm"),
          "Confirm and Continue \u2192",
          class = "btn btn-primary"
        )
      )
    ),
    shiny::tags$section(
      class = "configure-section",
      shiny::tags$div(
        class = "section-label",
        style = "margin:0 0 8px;",
        "Column Roles"
      ),
      mod_roles_ui("roles", embedded = TRUE)
    ),
    shiny::tags$section(
      class = "configure-section",
      style = "margin-top:24px;",
      shiny::tags$div(
        class = "section-label",
        style = "margin:0 0 8px;",
        "Synthesis Settings"
      ),
      mod_synthesis_controls_spec_ui("synthesis_controls", embedded = TRUE)
    ),
    shiny::tags$section(
      class = "configure-section",
      style = "margin-top:24px;",
      shiny::tags$details(
        class = "configure-summary-disclosure",
        shiny::tags$summary(
          class = "section-label disclosure-summary",
          "Column summary",
          shiny::tags$span(
            class = "disclosure-hint",
            "distributions \u00b7 percentiles"
          )
        ),
        shiny::tags$div(
          class = "disclosure-body",
          shiny::uiOutput("configure_summary_stats")
        )
      )
    ),
    shiny::tags$div(
      class = "main-header-action",
      style = "display:flex; justify-content:flex-end; margin-top:24px;",
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary",
        onclick = sprintf(
          "document.getElementById('%s').click();",
          shiny::NS("synthesis_controls")("confirm")
        ),
        "Confirm and Continue \u2192"
      )
    )
  )
}

ui <- bslib::page(
  theme = dg_theme,
  tags$head(
    tags$link(rel = "stylesheet", href = css_href("colors_and_type.css")),
    tags$link(rel = "stylesheet", href = css_href("shiny-app.css")),
    tags$link(rel = "stylesheet", href = css_href("_alignment.css")),
    tags$script(HTML("
      // Column-filter triage modal: drag (or click-to-cycle) a column chip
      // between the Synthesise / Pass through / Drop zones. Delegated on
      // document so it keeps working across repeated showModal() calls.
      (function() {
        var BUCKET_ORDER = ['synthesize', 'pass_through', 'drop'];

        function nextBucket(current) {
          var idx = BUCKET_ORDER.indexOf(current);
          return BUCKET_ORDER[(idx + 1) % BUCKET_ORDER.length];
        }

        function moveChip(chip, targetZone) {
          var chips = targetZone.querySelector('.cf-zone-chips');
          if (chips) chips.appendChild(chip);
        }

        document.addEventListener('dragstart', function(e) {
          var chip = e.target.closest('.cf-chip');
          if (!chip) return;
          e.dataTransfer.setData('text/plain', chip.getAttribute('data-col'));
          e.dataTransfer.effectAllowed = 'move';
          chip.classList.add('cf-dragging');
        });

        document.addEventListener('dragend', function(e) {
          var chip = e.target.closest('.cf-chip');
          if (chip) chip.classList.remove('cf-dragging');
        });

        document.addEventListener('dragover', function(e) {
          if (e.target.closest('.cf-zone')) e.preventDefault();
        });

        document.addEventListener('dragenter', function(e) {
          var zone = e.target.closest('.cf-zone');
          if (zone) zone.classList.add('cf-zone-over');
        });

        document.addEventListener('dragleave', function(e) {
          var zone = e.target.closest('.cf-zone');
          if (zone && !zone.contains(e.relatedTarget)) zone.classList.remove('cf-zone-over');
        });

        document.addEventListener('drop', function(e) {
          var zone = e.target.closest('.cf-zone');
          if (!zone) return;
          e.preventDefault();
          zone.classList.remove('cf-zone-over');
          var col = e.dataTransfer.getData('text/plain');
          if (!col) return;
          var chip = document.querySelector('.cf-chip[data-col=\"' + CSS.escape(col) + '\"]');
          if (chip) moveChip(chip, zone);
        });

        document.addEventListener('keydown', function(e) {
          if (e.key !== 'Enter' && e.key !== ' ') return;
          var chip = e.target.closest('.cf-chip');
          if (!chip) return;
          e.preventDefault();
          chip.click();
        });

        document.addEventListener('click', function(e) {
          var chip = e.target.closest('.cf-chip');
          if (chip) {
            var zone = chip.closest('.cf-zone');
            var current = zone ? zone.getAttribute('data-bucket') : 'synthesize';
            var target = document.querySelector('.cf-zone[data-bucket=\"' + nextBucket(current) + '\"]');
            if (target) moveChip(chip, target);
            return;
          }
          var btn = e.target.closest('.cf-apply');
          if (!btn) return;
          var modalContent = btn.closest('.modal-content') || document;
          var mapping = {};
          modalContent.querySelectorAll('.cf-zone').forEach(function(z) {
            var bucket = z.getAttribute('data-bucket');
            z.querySelectorAll('.cf-chip').forEach(function(c) {
              mapping[c.getAttribute('data-col')] = bucket;
            });
          });
          Shiny.setInputValue(btn.getAttribute('data-input-id'), mapping, {priority: 'event'});
        });
      })();
    "))
  ),
  tags$div(
    class = "app",
    id = "app-shell",
    sidebar_content,
    tags$main(
      class = "main",
      bslib::navset_hidden(
        id = "app_tabs",
        bslib::nav_panel_hidden("upload", mod_upload_ui("upload")),
        bslib::nav_panel_hidden("objective", mod_synthesis_controls_objective_ui("synthesis_controls")),
        bslib::nav_panel_hidden("configure", configure_ui()),
        bslib::nav_panel_hidden("generate", mod_generate_ui("generate")),
        bslib::nav_panel_hidden("compare", mod_compare_ui("compare")),
        bslib::nav_panel_hidden("export", mod_export_ui("export"))
      )
    ),
    tags$div(
      id = "resize-handle",
      style = "width:5px; cursor:col-resize; background:var(--border); transition:background 120ms; flex-shrink:0;",
      onmouseover = "this.style.background='var(--synth-300)'",
      onmouseout = "this.style.background='var(--border)'"
    ),
    mod_data_panel_ui("data_panel")
  )
)

server <- function(input, output, session) {
  state <- mod_state_server("state")
  app_guardrail_server("guardrail", state)

  shiny::observeEvent(input$report_issue, ignoreNULL = TRUE, {
    url <- dataganger:::.build_issue_url(context = "Shiny app")
    body <- dataganger:::.issue_body_from_url(url)
    shiny::showModal(report_issue_modal(url, body))
  })

  shiny::observeEvent(input$reset_all, ignoreNULL = TRUE, {
    state$raw_data <- NULL
    state$profile <- NULL
    state$roles <- NULL
    state$roles_confirmed <- 0L
    state$column_filter <- NULL
    state$objective_confirmed <- 0L
    state$spec <- NULL
    state$spec_confirmed <- 0L
    state$synthetic <- NULL
    state$comparison <- NULL
    state$compare_selected_var <- NULL
    state$privacy <- NULL
    state$kanon <- NULL
    state$pipeline_warnings <- NULL
    state$generated_roles <- NULL
    state$seed_used <- NULL
    state$nav_request <- NULL
    state$fail_safe_status <- "idle"
    state$fail_safe_flagged <- app_fail_safe_empty()
    state$fail_safe_upload_token <- NULL
    # Attestation is a claim about a specific dataset; a true "Start over" drops
    # the data, so the no-direct-identifier gate must re-fire before the new one.
    state$attested_no_direct <- FALSE
    state$active_step <- "upload"
    bslib::nav_select("app_tabs", "upload")
    send_step_state(0L)
  })

  mod_upload_server("upload", state)
  mod_column_filter_server("column_filter", state)
  mod_roles_server("roles", state)
  mod_synthesis_controls_server("synthesis_controls", state)
  mod_generate_server("generate", state)
  mod_compare_server("compare", state)
  mod_export_server("export", state)
  mod_data_panel_server("data_panel", state)

  # Record the visible step so modules can react to arrival. mod_export_server
  # shows the disclosure-risk brief when this becomes "export".
  shiny::observeEvent(input$app_tabs, ignoreNULL = TRUE, {
    state$active_tab <- input$app_tabs
  })

  # Set initial step state
  session$onFlushed(function() {
    send_step_state(0L)
  }, once = TRUE)

  STEP_IDS <- c("upload", "objective", "configure", "generate", "compare", "export")

  # Compute the furthest step reached (0-based index into STEP_IDS)
  max_step_reached <- shiny::reactive({
    if (!is.null(state$synthetic)) {
      return(5L)
    }
    if (isTRUE(state$spec_confirmed > 0L)) {
      return(3L)
    }
    if (isTRUE(state$objective_confirmed > 0L)) {
      return(2L)
    }
    if (!is.null(state$raw_data) &&
      identical(state$fail_safe_status, "ready")) {
      return(1L)
    }
    0L
  })

  current_step_num <- shiny::reactiveVal(0L) # 0-based

  send_step_state <- function(cur) {
    current_step_num(cur)
    state$active_step <- STEP_IDS[[cur + 1L]]
    session$sendCustomMessage("setCurrentStep", list(
      current = cur,
      max     = shiny::isolate(max_step_reached())
    ))
  }

  # Sidebar navigation
  shiny::observeEvent(input$nav_go, ignoreNULL = TRUE, ignoreInit = TRUE, {
    target <- input$nav_go
    tgt_idx <- match(target, STEP_IDS) - 1L
    cur_idx <- current_step_num()
    max_idx <- max_step_reached()
    # Only allow adjacent steps
    if (!is.na(tgt_idx) && abs(tgt_idx - cur_idx) <= 1L && tgt_idx <= max_idx) {
      bslib::nav_select("app_tabs", target)
      send_step_state(tgt_idx)
    }
  })

  # Auto-detect roles after upload. Gated on state$column_filter so the
  # column-filter popup's synthesize/pass_through/drop choice (the first,
  # header-only data filter) is always applied to the freshly detected roles
  # rather than raced against or silently overwritten.
  observe({
    req(state$raw_data, state$profile, state$column_filter)
    if (is.null(state$roles)) {
      shiny::withProgress(message = "Detecting column roles\u2026", value = 0.5, {
        roles <- dg_timeit(
          "configure: detect_roles",
          dg_ensure_ui_roles(detect_roles(state$raw_data, profile = state$profile))
        )
        column_filter <- state$column_filter
        for (col in names(column_filter)) {
          idx <- roles$variable == col
          if (any(idx)) {
            roles$simulation[idx] <- column_filter[[col]]
          }
        }
        state$roles <- roles
        shiny::setProgress(value = 1.0)
      })
    }
  })

  # Auto-advance to configure once objective is confirmed
  observeEvent(state$objective_confirmed, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (isTRUE(state$objective_confirmed > 0L)) {
      bslib::nav_select("app_tabs", "configure")
      send_step_state(2L)
    }
  })

  # Auto-advance to objective once upload and the fail-safe are complete
  observeEvent(state$fail_safe_status, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (identical(state$fail_safe_status, "ready")) {
      bslib::nav_select("app_tabs", "objective")
      send_step_state(1L)
    }
  })

  # Auto-advance to generate once spec is confirmed
  observeEvent(state$spec_confirmed, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (isTRUE(state$spec_confirmed > 0L)) {
      bslib::nav_select("app_tabs", "generate")
      send_step_state(3L)
    }
  })

  observeEvent(state$objective_confirmed, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (isTRUE(state$objective_confirmed > 0L)) {
      session$sendCustomMessage("setDoneStep", "objective")
    }
  })

  observeEvent(state$raw_data, ignoreNULL = TRUE, {
    session$sendCustomMessage("setDoneStep", "upload")
  })

  observeEvent(state$roles_confirmed, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if (isTRUE(state$roles_confirmed > 0L)) {
      session$sendCustomMessage("setDoneStep", "configure")
    }
  })

  observeEvent(state$spec, ignoreNULL = TRUE, {
    session$sendCustomMessage("setDoneStep", "configure")
  })

  observeEvent(state$synthetic, ignoreNULL = TRUE, {
    session$sendCustomMessage("setDoneStep", "generate")
  })

  observeEvent(state$comparison, ignoreNULL = TRUE, {
    session$sendCustomMessage("setDoneStep", "compare")
  })

  # P5: summary stats at bottom of Configure page
  output$configure_summary_stats <- shiny::renderUI({
    shiny::req(state$profile, state$raw_data)
    prof <- state$profile$profile
    if (is.null(prof) || nrow(prof) == 0L) {
      return(NULL)
    }

    cat_types <- c("character", "factor")
    num_types <- c("numeric", "integer")

    cat_cols <- prof$variable[prof$type %in% cat_types]
    num_cols <- prof$variable[prof$type %in% num_types]

    th_style <- "padding:5px 8px; font-size:11px; font-weight:600; text-transform:uppercase; letter-spacing:.04em; white-space:nowrap;"
    td_style <- "padding:4px 8px; font-size:12px; font-family:var(--font-mono);"
    td0_style <- "padding:4px 8px; font-size:12px; font-weight:600;"

    # Numeric summary table - cool teal header
    num_section <- if (length(num_cols) > 0L) {
      num_th <- paste0(th_style, " background:#1a6b6b; color:#e8f5f5;")
      num_rows <- lapply(seq_along(num_cols), function(i) {
        cn <- num_cols[[i]]
        r <- prof[prof$variable == cn, ]
        fmt <- function(x) if (!is.null(x) && length(x) == 1L && !is.na(x)) sprintf("%.2f", as.numeric(x)) else "\u2014"
        row_bg <- if (i %% 2 == 0) "background:#f0f7f7;" else "background:#ffffff;"
        shiny::tags$tr(
          style = row_bg,
          shiny::tags$td(style = paste0(td0_style, " color:#1a6b6b;"), cn),
          shiny::tags$td(style = td_style, fmt(r$min)),
          shiny::tags$td(style = td_style, fmt(r$q25)),
          shiny::tags$td(style = paste0(td_style, " font-weight:700;"), fmt(r$median)),
          shiny::tags$td(style = td_style, fmt(r$q75)),
          shiny::tags$td(style = td_style, fmt(r$max)),
          shiny::tags$td(style = td_style, fmt(r$mean)),
          shiny::tags$td(style = td_style, fmt(r$sd))
        )
      })
      shiny::tagList(
        shiny::tags$p(
          style = "font-family:var(--font-sans); font-size:12px; color:#1a6b6b; font-weight:700; margin:0 0 6px; text-transform:uppercase; letter-spacing:.05em;",
          "\u25a0 Continuous columns"
        ),
        shiny::tags$div(
          style = "overflow-x:auto; border:1px solid #b2d8d8; border-radius:4px;",
          shiny::tags$table(
            style = "border-collapse:collapse; width:100%; font-size:12px;",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th(style = num_th, "Column"),
              shiny::tags$th(style = num_th, "Min"),
              shiny::tags$th(style = num_th, "Q1"),
              shiny::tags$th(style = paste0(num_th, " background:#0f4d4d;"), "Median"),
              shiny::tags$th(style = num_th, "Q3"),
              shiny::tags$th(style = num_th, "Max"),
              shiny::tags$th(style = num_th, "Mean"),
              shiny::tags$th(style = num_th, "SD")
            )),
            shiny::tags$tbody(num_rows)
          )
        )
      )
    }

    # Categorical frequency tables - warm amber, one card per column
    cat_section <- if (length(cat_cols) > 0L) {
      cat_th <- paste0(th_style, " background:#7a4419; color:#fff3e0;")
      cat_tables <- lapply(cat_cols, function(cn) {
        x <- state$raw_data[[cn]]
        tbl <- sort(table(x, useNA = "no"), decreasing = TRUE)
        top <- head(tbl, 5L)
        total <- sum(tbl)
        rows <- mapply(function(val, cnt, i) {
          pct <- 100 * cnt / total
          bar_w <- round(pct)
          row_bg <- if (i %% 2 == 0) "background:#fdf6ee;" else "background:#ffffff;"
          shiny::tags$tr(
            style = row_bg,
            shiny::tags$td(style = paste0(td0_style, " color:#7a4419; max-width:200px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;"), val),
            shiny::tags$td(style = td_style, format(cnt, big.mark = ",")),
            shiny::tags$td(
              style = "padding:4px 8px; min-width:100px;",
              shiny::tags$div(
                style = "display:flex; align-items:center; gap:6px;",
                shiny::tags$div(
                  style = sprintf("height:8px; width:%dpx; background:#c97b38; border-radius:3px; flex-shrink:0;", max(2L, bar_w)),
                ),
                shiny::tags$span(
                  style = "font-size:11px; font-family:var(--font-mono); color:var(--fg-muted);",
                  sprintf("%.1f%%", pct)
                )
              )
            )
          )
        }, names(top), as.integer(top), seq_along(top), SIMPLIFY = FALSE)
        shiny::tags$div(
          style = "margin-bottom:14px; border:1px solid #e8c9a0; border-radius:4px; overflow:hidden;",
          shiny::tags$div(
            style = paste0(cat_th, " padding:6px 8px; font-size:12px; font-weight:700;"),
            cn,
            shiny::tags$span(
              style = "font-weight:400; margin-left:8px; opacity:.8;",
              sprintf("(%s distinct)", format(length(unique(x[!is.na(x)])), big.mark = ","))
            )
          ),
          shiny::tags$table(
            style = "border-collapse:collapse; width:100%;",
            shiny::tags$thead(shiny::tags$tr(
              shiny::tags$th(style = paste0(th_style, " background:#a05a2c; color:#fff3e0;"), "Value"),
              shiny::tags$th(style = paste0(th_style, " background:#a05a2c; color:#fff3e0;"), "Count"),
              shiny::tags$th(style = paste0(th_style, " background:#a05a2c; color:#fff3e0;"), "Distribution")
            )),
            shiny::tags$tbody(rows)
          )
        )
      })
      shiny::tagList(
        shiny::tags$p(
          style = "font-family:var(--font-sans); font-size:12px; color:#7a4419; font-weight:700; margin:16px 0 8px; text-transform:uppercase; letter-spacing:.05em;",
          "\u25a0 Categorical columns (top 5 values)"
        ),
        cat_tables
      )
    }

    shiny::tagList(num_section, cat_section)
  })

  # Re-broadcast step state when max_step_reached changes (synthetic data arrives, etc.)
  observe({
    max_step_reached()
    send_step_state(current_step_num())
  })

  # Module navigation requests (e.g. "\u2190 Adjust settings", "Continue to Export \u2192")
  observeEvent(state$nav_request, ignoreNULL = TRUE, {
    target <- state$nav_request
    if (identical(target, "purpose") || identical(target, "spec") || identical(target, "roles")) {
      target <- "configure"
    }
    state$nav_request <- NULL
    tgt_idx <- match(target, STEP_IDS) - 1L
    if (!is.na(tgt_idx) && tgt_idx <= max_step_reached()) {
      bslib::nav_select("app_tabs", target)
      send_step_state(tgt_idx)
    }
  })
}

shinyApp(ui, server)
