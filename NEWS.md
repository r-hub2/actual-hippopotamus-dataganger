# dataganger 0.8.2

A portability release. Every item below is one defect: what went wrong, why,
and what changed. The changes are limited to validated character date/time
handling, test diagnostics, and portability corrections; privacy and export
contracts are unchanged.

## Character columns holding 12-hour times were synthesized incorrectly

*   **Symptom.** A column such as `"01/01/2020 01:15 PM"` came back as
    `"01/01/2020 01:15"` -- no period marker -- and every afternoon value
    collapsed onto its morning hour, so `01:15 PM` and `01:15 AM` became the
    same synthetic value. This appeared on some platform/locale pairs only,
    which is why it surfaced on the CRAN check services and on macOS under
    `LC_TIME=en_GB` while passing elsewhere.

*   **Cause.** `%p` is handed to the C library by both `strptime()` and
    `format()`, and its meaning is locale- and OS-dependent: it can be
    uppercase, lowercase, translated, or empty. Where the locale defines no
    period marker, the 12-hour candidate format neither parsed nor
    round-tripped, so `detect_date_format()` fell through to a 24-hour format
    and discarded the marker together with the morning/afternoon distinction.

*   **Fix.** Detection and parsing read trailing ASCII `AM`/`PM` tokens
    directly and apply the 12-to-24 hour correction in R. Output derives the
    period from each synthesized hour and keeps the source column's upper- or
    lowercase convention. When a `%p` candidate has no ASCII marker, the
    package uses the active locale's native parser and formatter only if at
    least 90 percent of non-missing source values round trip after surrounding
    whitespace is trimmed. Unknown or mixed suffixes are rejected rather than
    stripped or reinterpreted as 24-hour time. 24-hour and date-only formats
    are unchanged.

*   **Regression evidence.** Helper and full-synthesis tests cover `12 AM`,
    `12 PM`, marker-case variants, an unknown `FOO`/`BAR` suffix fixture, a
    C-locale translated-period rejection, and a conditional native-period test
    when the host provides non-ASCII `%p` markers. The ASCII path is also
    traced to prove it does not delegate `%p` to the C library.

## Month-name role detection could disagree with synthesis

*   **Symptom.** A permissive role-detection pattern could label month-name
    text as a date even when the active locale could not parse it. Synthesis
    then silently fell through to categorical resampling. Dotted and lowercase
    English forms could also be recognized by role detection but not preserve
    their format through the date path.

*   **Cause.** `detect_roles()` and synthesis made separate decisions: the
    former used a broad regular expression, while the latter used locale-bound
    `strptime()` candidates. `detect_date_format()` also selected the best
    parsed candidate even when its round-trip fidelity was zero.

*   **Fix.** Role detection now calls the same validated format detector as
    synthesis, and every accepted format must round trip at least 90 percent of
    non-missing values. English abbreviated, full, dotted, and lowercase month
    forms use a locale-independent parser and renderer. A date role without a
    validated parser is now an error rather than a categorical fallback.

*   **Regression evidence and limits.** End-to-end seeded synthesis tests run
    under `LC_ALL=C` for `Jan`, `January`, `Jan.`, and lowercase English forms;
    they assert role, parser selection, output shape, reparsing, and
    determinism. Foreign month text, including accented examples, is supported
    only when the active locale parses and formats the candidate with the
    required fidelity. Otherwise it remains non-date text and is never silently
    reinterpreted. This is not a claim of universal foreign-locale parsing.

## Test failures did not say what had failed

*   **Symptom.** A check failure reported `actual: FALSE / expected: TRUE` and
    nothing about the values that caused it, which is not enough to diagnose a
    platform-specific defect from a check log.

*   **Cause.** Aggregate boolean assertions discarded the inspected values, so
    remote check logs showed only `actual: FALSE / expected: TRUE`.

*   **Fix and regression evidence.** Package-wide `any(grepl(...))`
    expectations now use matching expectations that print the inspected text.
    Every remaining aggregate `expect_true()`/`expect_false()` supplies
    diagnostic context that identifies the inspected or offending values. An
    AST-based test scans every test file, reports file, line, and expression,
    and rejects any aggregate boolean expectation without that context. Its
    self-test inserts a deliberately opaque fixture and verifies the diagnostic.

## Other portability corrections

*   Unicode multiplication signs in sample labels became parser warnings under
    `LC_ALL=C`; they are now ASCII `x`, with a regression that parses every
    package R source file under `LC_CTYPE=C`.
*   Test-file setup is isolated, removing a source-order dependence found by a
    fixed-seed shuffled run.

# dataganger 0.8.0

*   Generation now keeps its KPI row focused on output size, seed, duration,
    and exact matches. The k-anonymity outcome appears in a status card at the
    bottom of the page instead; an infeasible run gets a yellow warning with
    computed regenerate choices and a shortcut that opens the minimum-group-
    size control in Configure's Advanced settings. Export now leads with the
    bundle contents, omits the duplicate generation summary, and explains a
    missing k-anonymity protection using the run's actual k, QI columns, and
    smallest group size before asking for acknowledgment.

*   The minimum group size that drives k-anonymity is now a slider in the
    Configure step's Advanced settings, with live readouts of how many
    combinations and rows fall below the value you pick. It stays disabled
    until every column in the table above has both of its questions answered,
    because the setting has nothing to act on until then -- previously it could
    be dragged and silently did nothing. The slider unlocks on exactly the same
    condition as the Confirm button beside it, and dropped or passed-through
    columns need no answers for either.

*   Answering a column question no longer resets the other Advanced settings.
    The row count, engine, seed, column name handling, rare-category threshold
    and the checkboxes kept their values only until the next answer, which
    rebuilt the whole panel from the objective's presets. The suggested row
    count still tracks your answers, in the hint below the slider.

*   Configure step layout: the objective recap card is gone, since it restated
    a choice already made in the previous step and linked where the sidebar
    already goes. Advanced settings are split into privacy controls, shown
    first, and output settings, collapsed below them. The action legend is
    shorter, and the "pass through keeps the real values" caution now appears
    once in the legend rather than on every row -- it is unchanged in the
    Generate step, the export summary and the manifest.

*   Arriving at the Export step now opens a disclosure-risk brief: the minimum
    group size and which columns it covers, how many rows were blanked to reach
    it, how many synthetic rows reproduce a real record and how many of those
    expose a sensitive value, and the remaining privacy-check findings with
    their severity. The brief is informational only -- the existing export
    checks are unchanged, including the block on reproduced rows that expose a
    sensitive value. Its wording describes what each protection does rather
    than naming the technique.

*   Categorical output is now always plain character. Factors and
    `haven_labelled` columns are normalized at synthesis input and are no
    longer produced or returned by dataganger. A factor level declared with
    zero rows is dropped: a category absent from the source stays absent from
    the synthetic copy rather than being invented.

*   Logical columns now keep the `logical` type on both engines. Previously a
    logical column came back as `"TRUE"`/`"FALSE"` text from the synthpop
    engine because synthpop models it as a two-level factor.

*   The internal categorical resamplers no longer default to merging rare
    values into `.other`. Every purpose preset already set `merge_rare = FALSE`,
    so the old default matched no preset.

*   Demo and development synthesis now mask rare category labels instead of
    merging them into `.other`, so every observed category keeps its own slot.

*   Every category observed in the source now appears in synthetic output for
    demo and development purposes, while k-anonymity suppression no longer
    introduces an `(other)` category.

*   Fix a stale `merge_rare = TRUE` fallback in the marginal engine. Every
    purpose preset and every `synth_*()` helper defaults to `FALSE`, but a spec
    that omitted the field entirely still merged rare labels into `.other`. A
    spec built by `synth_spec()` always carries the field, so this was reachable
    only through a hand-built spec, a recipe that omitted the key, or
    `synth_spec(merge_rare = NULL)` -- assigning `NULL` drops the element.

*   Seeded synthesis now reproduces scrambled identifier columns exactly. The
    pass that scrambles alphanumeric ID columns (simulation = `"scramble"`,
    the default for identifier-shaped columns) previously drew from ambient
    randomness after the seeded step, so two runs with the same original data
    and the same seed could produce different scrambled values even though
    every other column matched. The scramble now runs under the spec's seed on
    both engines, so the reproduction script in `human/human.md` delivers on
    its same-seed, identical-output promise for every bundle.

*   Scrambled identifier columns (simulation = `"scramble"`) no longer risk
    handing back another record's identifier. Because scrambling permutes the
    characters of the original value, one row's scramble could coincidentally
    equal a different row's original ID (e.g. `"T0010"` scrambling to
    `"T0001"`). Candidates are now checked against the full set of observed
    values, so no original identifier from any row appears in the scrambled
    output whenever the character space allows it.

*   Categorical resampling now guarantees every sampled category level appears
    at least once whenever the requested output has enough rows; this preserves
    level presence for downstream code paths, joins, and facets.

*   Capacity warnings for level-presence restoration are now combined into one
    warning per synthesis output while retaining affected columns and sizes.

*   Categorical and free-text columns gain a `label_strategy` setting and a
    "Resample (rare levels masked)" action. It preserves each observed level's
    slot and frequency while replacing rare labels with distinct neutral
    placeholders, without merging categories into `.other`.

*   The Action dropdown on the Configure step now offers only the actions that
    make sense for a column's data type, and names them for that type. Free
    text gains an explicit choice between "Resample" and "Scramble"; numeric
    and date columns no longer offer "Scramble", which would have run the
    identifier scrambler over numbers; and alphanumeric IDs no longer offer a
    resample. The single label "Synthesise" is replaced by the treatment it
    actually applies -- "Resample" for categorical and free text, "Simulate"
    for numeric and date, "Generate new" for postal codes. The stored action
    vocabulary in `roles$simulation` is unchanged, so agent bundles and CLI
    recipes are unaffected.

*   Postal-code columns lose their separate strategy dropdown: "Generate new"
    and "Resample" are actions, so they are now chosen in the Action
    dropdown alongside every other action. A postal column is configured with
    two controls instead of four. Country format remains its own control, as a
    parameter of the action rather than an alternative to it.

*   The Configure step's reference card is keyed by action rather than by data
    type, so the question it answers is "what will this choice do to my data"
    rather than "what type am I looking at".

*   An action override that a later type change makes meaningless is now
    discarded rather than left set, where it could be restored by a
    subsequent answer to question 1.

*   Fix silent data loss on the Configure step: a column you had explicitly
    kept (Action = pass through or scramble) was reset to `drop` as soon as
    you answered question 2 (Is it sensitive?). Sensitivity is not a keep/drop
    decision, but the question-2 handler re-derived the column action anyway,
    overwriting the choice. Because question 2 is mandatory, this affected
    effectively every column detected as a direct identifier. Explicit column
    actions are now recorded as a sticky override and are no longer clobbered
    by either disclosure question.

*   Question 1 on the Configure step always offers "Yes, it identifies a
    person on its own". Previously, attesting at upload that the file held no
    direct identifiers removed that option, which left any column detected as
    a direct identifier displaying no answer at all, with no way to correct
    it. Answering "yes" still drops the column, by design, and now says so.

*   The Configure step stacks questions 1 and 2 in a single column to save
    horizontal space, and the drop notice is now driven by the effective
    column action rather than the disclosure role, so a direct identifier you
    chose to keep is no longer reported as removed.

*   New "Exact matches" tab in the data preview, shown when synthetic rows
    reproduce a real record verbatim. It lists one entry per row and column
    involved, with the row numbers in both tables and whether the column was
    marked sensitive in question 2. The Original and Synthetic previews now
    carry a row-number column so those references can be located.

*   Rows reproduced verbatim are tinted amber, or red when they expose a value
    marked sensitive. Red rows block the browser export: the bundle cannot be
    downloaded until a regeneration clears them, after which an explicit
    acknowledgment is required and is recorded in `agent/manifest.json` as
    `exact_match_acknowledged`. `export_synthetic()` gains a matching
    `exact_match_acknowledged` argument, defaulting to `FALSE`.

*   Fix CRAN policy violation: `rmarkdown::render()` now passes
    `intermediates_dir = tempdir()` so knitr writes intermediate files to the
    R session's temporary directory rather than to the installed package
    directory. The package directory is read-only on some CRAN platforms,
    causing check failures on Debian r-devel and r-patched flavors.

# dataganger 0.7.1

A simpler column-role taxonomy, a de-identify-by-default policy for
identifier columns, honest date/time synthesis, and a batch of Configure-page
privacy and usability fixes.

*   New postal code data type: columns named postal/zip/postcode/plz/cep/pin_code
    are detected as quasi-identifiers with a 10-country format registry
    (CA, US, UK, AU, DE, FR, JP, IN, BR, NL). Two per-column synthesis
    strategies: `generate` (random format-valid values, zero source leakage)
    and `resample` (observed values). Country format auto-detected from values
    with explicit override. Postal config round-trips through recipe YAML.

## Breaking changes

*   **Identifier columns are now scrambled by default instead of dropped.**
    A column classified as a direct or alphanumeric identifier now defaults
    to `simulation = "scramble"` — it is kept in the output but de-identified
    (its letters and digits are reordered while delimiters and length are
    preserved, so no original value survives) rather than silently removed.
    Dropping a column is now an explicit action (`simulation = "drop"`).
    Recipes or scripts that relied on the old drop-by-default behaviour will
    now see the column present (scrambled) unless they set `simulation: drop`
    explicitly; regenerate affected bundles and update recipes as needed.

## Disclosure and privacy fixes

*   **The data preview highlights exact-match rows.** When a synthetic row is a
    verbatim copy of an original row (the disclosure the red EXACT MATCHES stat
    counts), those rows are now tinted red in the preview panel: the offending
    rows in the Synthetic tab and the reproduced rows in the Original tab. The
    highlight uses the same match rule as the stat, so the two always agree, and
    is visible on every step (including Configure) because the preview panel is.
*   **Scrambling now de-identifies short numeric identifiers.** Reordering a
    value's characters cannot change a single digit (`"5"`) or a run of
    identical digits (`"11"`), so a plain integer ID column left its smallest
    values in place — a re-identification leak. Such values are now replaced
    with random digits of the same width instead, guaranteeing every scrambled
    value differs from the original. Genuine alphanumeric IDs are still
    reordered (their character multiset is preserved) as before.
*   **Character-stored dates and times are now synthesised properly.**
    Values such as `"01/08/2020"` or `"14:30"` were previously detected as
    dates but fell through to generic categorical resampling — the original
    values were reshuffled across rows with no date-range synthesis and no
    `coarsen_dates` protection. They are now parsed with their own format,
    synthesised through the date/datetime/time-of-day machinery, and
    reformatted back to the source pattern. Character-stored dates also now
    receive the same `quasi` disclosure default as native `Date` columns.
*   k-anonymity whole-cell suppression volume is now visible: the `kanon`
    attribute, `manifest.json`, and `human/human.md` carry `suppressed_rows`
    and `suppressed_row_frac`, and `dataganger inspect` reports k-anonymity
    status. A single below-k residual cell can cascade into suppressing much
    more of a quasi-identifier column than its cell count implies; this is
    existing, intended behaviour, now surfaced rather than hidden.
*   The **EXACT MATCHES** statistic box turns red when any synthetic row
    matches a real row verbatim (a direct disclosure), and the previously
    inert `stat risk` styling now renders.

## Configure page and role taxonomy

*   The column-type taxonomy is simplified: logical is folded into
    categorical, free text is handled as categorical gated by a new
    data-size-aware comparison cutoff (`dg_max_comparable_levels()`), and the
    former "pseudo identifier" type is merged into a single "alphanumeric ID"
    identifier catch-all.
*   Type overrides on the Configure page now take effect reliably: overriding
    an identifying column to a non-identifying type no longer silently drops
    it, and retyping a column away from an identifying type resets its
    "direct identifier" answer so the change must be re-confirmed.
*   The action-override panel now explains the consequence of overriding an
    ID or free-text column, including a warning when the column will still
    exceed the Compare page's comparison cap.
*   Added bulk-configure: select multiple columns with checkboxes and apply a
    type, identifier answer, sensitivity answer, or action to all of them at
    once, for wide datasets.

## Upload experience

*   The upload drop zone now accepts drag-and-drop across the whole upload
    card, and a drag-and-drop column-filter popup appears immediately after
    upload for narrowing wide datasets before configuration.
*   The column-filter popup now works on column names only and the data is
    read **after** you click Continue: a column dragged to Drop is never
    loaded, profiled, role-detected, synthesised, or exported. Previously the
    Drop choice was recorded but not applied, so dropped columns still flowed
    into the rest of the workflow.

# dataganger 0.6.1

CRAN resubmission changes requested on 2026-07-10.

*   DESCRIPTION now quotes the package name `'shiny'` correctly in the
    Description field.
*   `read_input()` examples are now self-contained, executable, and no longer
    use commented-out code.
*   `export_synthetic()`, `make_agent_bundle()`, and
    `export_diagnostic_package()` examples now write only to temporary paths
    and no longer use `\dontrun{}`.
*   Audited exported write functions, examples, tests, and vignettes for
    home-filespace writes; package examples now stay within `tempfile()` /
    `tempdir()`.

# dataganger 0.6.0

One minimal export bundle, a Configure page with no silent defaults, and a package-wide audit pass: privacy fixes, an honest engine story, honest privacy wording, and a lighter dependency footprint.

## Privacy fixes

*   **k-anonymity now runs before column renaming.** Previously, with
    `name_strategy = "generic"`, generic renaming ran first and recorded the
    name map before `enforce_kanon()` dropped direct identifiers and applied
    suppression — so bundles could under-protect. Both engine paths now
    enforce k-anonymity first. Bundles generated with generic names on
    earlier versions may be under-protected; regenerate them.
*   The manifest and the privacy report now agree on a single exact-row-match
    number: `export_synthetic()` uses the same roles-derived exclusion rule
    as `privacy_check_post()`.
*   Seeded synthesis is fully deterministic: `decimal_places()` samples long
    columns with a deterministic stride instead of `sample()`, removing an
    RNG side effect.
*   The app's theme no longer references Google Fonts at all
    (`bslib::font_google()` replaced with plain family strings served from
    the packaged self-hosted files); the no-network source guard now also
    scans `inst/app/`.

## Bundle & agent skill

*   Export bundles use one minimal layout: `synthetic_data.csv` at the root,
    `human/` (`human.md` with the privacy report folded in, plus optional
    `comparison_report.html`), and `agent/` (`recipe.yaml` = combined
    spec + roles, `AGENT.md`, `manifest.json`). `data_dictionary.csv`,
    `load_data.R`, `analysis.qmd`, `ai-readme.md`, `privacy_report.txt`, and
    the separate `spec.yaml`/`roles.yaml` are gone. CLI `synthesize` gains
    `--recipe <recipe.yaml>`; `--spec`/`--roles` remain supported. `compact`
    and `include_dictionary` are deprecated no-ops.
*   `export_synthetic()` honors its `code_readiness` argument: when
    supplied, the bundle gains `agent/code_readiness_report.json`;
    `make_agent_bundle()` computes it automatically, so every agent bundle
    now ships the structural-compatibility report.
*   Both shipped skills (`AGENT.md` in every bundle and
    `using-dataganger-bundles`) rewritten to the minimal bundle contract.

## Engine

*   `"auto"` is a real engine alias in `synth_spec()`, `synthesize_data()`,
    the CLI (`--engine <auto|internal|synthpop>`), and spec YAML. An explicit
    `"auto"` behaves exactly like leaving the engine unset: the engine is
    derived from the objective and `dataganger.disable_synthpop` is
    respected.
*   The misleading `engine_required` spec field is retired; spec printing
    reports the explicit engine or `auto (derived from objective)`.

## App (Shiny)

*   Compare now separates Univariate and Bivariate views. The Bivariate view
    uses an X-by-synthetic interaction test to show whether predictor-outcome
    relationships changed, with outcome-specific effect sizes and p-value
    fidelity colors.
*   Exported comparison reports now include the relationship-interaction table,
    using data-column order to define predictor then outcome.
*   Synthesis controls are folded into collapsed **Advanced settings**, keeping
    the generation review focused on the effective configuration.
*   Generation guidance now invites users to review, generate, or go back to
    adjust settings, and the data panel automatically previews each newly
    generated synthetic dataset.
*   When `synthpop` is unavailable, the upload attestation recommends installing
    it for correlation-aware synthesis.
*   Configure has no silent defaults: Q1 (Points to a person?) and Q2
    (Sensitive?) start blank for every column — auto-detected values no
    longer pre-select — and generation is gated until every column has an
    explicit answer to both questions. Explicit UI answers are tracked
    separately (`user_identifies` / `user_sensitive`) so CLI synthesis is
    unaffected.
*   The upload fail-safe flags only direct-identifier candidates (ID
    patterns / free text), no longer sensitive-named columns such as
    `income`; flagged columns get a "potential identifier" pill and
    semantically-coloured actions.
*   Categorical comparisons are now inference-aware like numeric ones:
    colored by a chi-square/Fisher distributional p-value with TVD as the
    displayed effect size; the SMD definition is shown on the effect column.
*   Assorted Configure/Compare/Generate polish: bottom Confirm-and-Continue,
    calmer attestation wording (with a disable-internet note), preserve-panel
    highlight, per-question help tied to table columns, and a generation
    fidelity recap.

## Docs

*   Sensitive-column wording is honest and consistent everywhere:
    quasi-identifying columns are "grouped with k-anonymity so no rare
    combination survives"; sensitive non-identifying columns are "recreated
    from its distribution; exact values are not copied — attribute-level
    protection is not yet applied". The former "protected from linkage"
    claim is gone.
*   Engine documentation matches reality (demo → internal; development →
    synthpop when installed; analytics → synthpop + risk acknowledgement).
*   `detect_roles()` documents the two-axis columns; assorted roxygen and
    vignette corrections; the startup message is reduced to version +
    `run_app()`.

## Dependencies & internals

*   `purrr`, `tidyr`, and `vctrs` dropped from Imports (unused); `plotly`
    moved to Suggests with an install gate in the Compare module.
*   `check_code_readiness()` now documents and reports that
    `haven_labelled` → character is the expected round-trip for now.
*   Internal hygiene: unified ID-name regex, NULL/NA-safe role lookups (a
    roles object missing a column no longer errors), helper relocations, and
    dead-code removal.

# dataganger 0.5.0

Privacy gating, UI/CLI parity, an agent skill, and a provable no-network guarantee.

*   Comparison stats are now inference-aware for numeric variables: the Compare
    view shows mean SMD, SD ratio, and median standardized difference, each
    coloured by their t/F/Wilcoxon p-value bands; min/max remain value-only.
*   UI export bundles now include `spec.yaml` and `roles.yaml`, and CLI
    `synthesize --roles` can reuse the full role matrix so UI and CLI runs
    reproduce byte-identical output with the same seed.
*   The app now opens with a hard no-direct-identifiers attestation gate, then
    runs an early assistive fail-safe immediately after upload to flag possible
    direct identifiers before Objective / Configure. Once attested, Configure's
    first question collapses to `none` / `combination`. The two questions are
    framed as the remaining risks after direct identifiers: linkage (combination)
    and sensitivity.
*   Added an agents-only packaged `SKILL.md` plus `dataganger skill [--out <file>]`
    so an AI can drive the package to generate synthetic data without ever
    reading the real data; fixed `ai-readme.md` so dropped columns are not listed
    as `NA (NA)`.
*   No-network guarantee: web fonts are now self-hosted (no Google Fonts CDN), so
    the app makes no external requests; `report_issue()` prints a copy-paste
    GitHub issue instead of opening a browser (the Shiny button shows a copyable
    modal). A shipped runtime trap test and source guard prove the package makes
    no network calls, and a Linux `unshare -rn` CI job runs the suite with no
    network at all.
*   New `vignette("privacy-and-ai-workflow")` documents the privacy gating ladder,
    the two ways to use the package with AI, and the no-network guarantee.

# dataganger 0.4.0

Configure redesign around two intrinsic privacy questions.

*   The Configure step now classifies each column by answering two independent
    questions — does it point to a person (`identifies`: none / combination /
    direct) and is it sensitive (`sensitive`) — and **derives** the treatment
    rather than asking the user to pick it. The two questions are shown
    prominently above the per-column table.
*   k-anonymity membership now reads both axes, so a column that is both
    identifying-in-combination and sensitive is covered. Numeric
    quasi-identifiers are no longer coarsened into `NA` bins.
*   Each column row has an **Action override** column exposing the Action
    (synthesize / pass through / drop) and data-type overrides directly.
*   The Generate step now shows a per-column review table (points to a person?,
    sensitive?, action, and a plain-English outcome) so choices can be verified
    before generating.
*   Objective selection uses a single Protection meter, makes **development**
    the default objective, and rewrites the per-objective detail panel around
    consistent dimensions for use, values, relationships, identifiers, and
    sensitive / rare data.
*   Synthesis Settings labels are more human-readable, with matching
    `synth_spec()` documentation for the current settings surface.
*   The per-column data preview includes a filter so you can inspect one
    variable at a time while reviewing the Configure step.
*   `export_synthetic(compact = )` supports two bundle variants: the compact app
    download and the full CLI / agent bundle.
# dataganger 0.3.5

Generation, comparison, and export clarity pass:

*   Generate page now shows a read-only "Column decisions" snapshot (the Configure
    table as final, non-editable values) so choices can be reviewed before
    generating.
*   New `report_issue()` helper plus an in-app **Report a problem** button open
    a pre-filled GitHub issue with environment details, without sending anything
    automatically.
*   The engine recap resolves to the engine actually used (e.g. `synthpop (auto)`)
    after generation, instead of always showing `auto`.
*   The Regenerate button is disabled until the first generation, so it no longer
    duplicates Generate on the initial visit.
*   Exact-row-match count moved into the result stats; the redundant verbatim
    Result box was removed.
*   Compare page treats geography columns as categorical, so they get an
    original-vs-synthetic comparison instead of being skipped.
*   Export page gains a generation summary: original rows/columns, how many
    columns were synthesized, passed through, and dropped, and the final
    synthetic dimensions.
*   Internal: the cancellable-synthesis subprocess no longer uses a `:::`
    self-reference (clearing an `R CMD check` NOTE); spelling WORDLIST expanded.

# dataganger 0.3.4

Configure page clarity pass:

*   Integer-valued columns now display without spurious decimals (e.g. `127`, not
    `127.00`) by detecting whole-valued numerics, not just R integer storage.
*   The column-roles table renames "Simulation" to **Action**, folds the
    recommendation inline into the TYPE control (`... (recommended)`), drops the
    redundant `recommended_role` column, and adds a per-row info tooltip with a
    plain-English reason and the storage type.
*   Setting a column's Action to **Drop** or **Pass through** now greys out and
    disables its TYPE and DISCLOSURE selectors and no longer blocks generation;
    pass-through columns carry a "real values - verify before sharing" note.
*   Disclosure-detection reason strings rewritten in plain English.
*   Collapsible help now uses an obvious `+`/`-` affordance.

# dataganger 0.3.3

*   `pkgload` removed from `Suggests` (uses `.__DEVTOOLS__` namespace check directly).
*   CRAN-readiness: `cran-comments.md` refreshed with accurate 0/0/2 NOTE explanations.
*   Added `pkgdown` site generation via GitHub Actions (`_pkgdown.yml` + workflows).
*   Planning artifacts (`docs/superpowers/`, `todo.md`) migrated out of the package root.

# dataganger 0.3.2

## Disclosure roles
* `detect_roles()` is now conservative: it only auto-assigns a disclosure role
  when confident (a clear direct identifier, or a known-sensitive column name).
  All other columns are left **unselected** rather than defaulted to
  quasi-identifier. This fixes the root cause of the 100%-NA synthetic output:
  measures, counts, dates, and low-cardinality categoricals are no longer
  silently treated as quasi-identifiers.
* The Configure page now **requires an explicit disclosure role for every
  column** before generating. A live counter shows how many are still
  unselected. `None` is a valid explicit choice; empty is not.
* k-anonymity fires only on columns the user marks as quasi-identifiers. The
  `max_suppress_frac` feasibility backstop is retained as defense-in-depth.
* CLI: spec YAML accepts a `disclosure_roles:` mapping (column -> role) so
  disclosure decisions are reproducible from the command line.

# dataganger 0.3.1

* **Bug fix — CUSUM hang (Bug 5)**: Synthesis no longer hangs on datasets with
  character-stored date columns (e.g. "Jun 8, 2019") or other high-cardinality
  character columns. The root cause was two-fold: (1) character date strings were
  classified "unknown" and passed to synthpop as a 2000+-level factor, causing
  CART to enumerate billions of split candidates; (2) even moderate-cardinality
  character columns (>20 distinct values) used as CART *predictors* trigger the
  same 2^(k-1) blowup at k=34. Fix: `detect_roles()` now detects date strings
  (ISO, "Mon DD YYYY", MM/DD/YY) via regex and classifies them as "date"; and a
  new `synthpop_bridge_cols()` function excludes any character column with >20
  distinct values from synthpop's CART, synthesizes it independently via the
  marginal engine, and stitches it back into the output in the original column
  order. Generation on the CUSUM test file (41 k rows, 14 columns) now completes
  in under 10 seconds at both 50-row and 5000-row target sizes.

* **P1 — Configure busy indicator**: the Upload page now shows a "Profiling
  data…" / "Detecting column roles…" progress bar while the app analyses the
  uploaded file, so users know the app is working rather than frozen.

* **P2 — Row count first**: the Row count (n) input is now the first item in
  the Configure advanced-settings panel.

* **P3 — Role-reactive row suggestion**: `suggest_min_rows()` gains a `data`
  parameter; when called with `data` and `roles`, it recomputes the coverage
  estimate over only the columns that are still being synthesized. Dropping an
  ID or excluded column now immediately lowers the suggested row count on the
  Configure page.

* **P4 — Case IDs render as character**: columns detected as ID candidates are
  coerced to character in the data-panel preview, so a numeric case ID displays
  as "1078541" instead of "1,078,541.00".

* **P5 — Column summary stats**: the Configure page now shows a per-column
  summary section below the synthesis settings. Continuous columns get a
  min / Q1 / median / Q3 / max / mean / SD table; categorical columns get a
  top-5 frequency table with counts and percentages.

* **P6 — Generation progress bar + timer**: while synthesis is running, the
  Generation page displays a `MM:SS` elapsed-time counter and an animated
  progress bar. Both update every second.

# dataganger 0.3.0

* Cancellable background synthesis: the Shiny Generation step now runs the
  synthesize -> compare -> privacy pipeline in a background process (via
  `callr`), so the app stays responsive and a Cancel button can stop a long or
  stuck run. Falls back to synchronous in-process generation when `callr` is
  unavailable or the package is dev-loaded. The `dataganger.synthesis_async`
  option forces the deterministic synchronous path for tests and CI.

* Coverage-based row-count suggestion: `profile_data()` now carries
  cross-column coverage (distinct joint combinations of categorical columns
  plus the largest per-column level count), and the new `suggest_min_rows()`
  function turns that into a sufficient synthetic row count - capped at 5000,
  floored at the largest level count, and never above the original. The
  Configuration row-count slider pre-fills with the suggestion, shows an
  inline hint, and warns when set below the coverage floor. The Upload step
  shows a coverage-summary card.

* Diagnostics for long-running synthesis: `dg_log` / `dg_timeit` emit
  per-phase progress to the R console when `options(dataganger.verbose =
  TRUE)`, and `check_cancel()` polls `options(dataganger.cancel)` at column
  boundaries for cooperative cancellation. `.onAttach()` prints a startup
  hint with the package version and how to launch the app and CLI.

* Free-text detection now head-samples to 1000 rows, bounding a hot path
  that could slow the transition into Configure on wide or long-string data.

* `synthesize_marginal()` trusts the detected role instead of recomputing the
  free-text heuristic, removing a redundant pass over character columns.

* The Comparison step no longer shows a stale full table on first transition:
  the first comparable variable renders correctly without needing a click.

* The Configuration step shows inline help for each disclosure role (None,
  Direct identifier, Quasi-identifier, Sensitive) with a short example.

* `callr` and `pkgload` added to `Suggests`.

# dataganger 0.2.2

* The Column Roles step now shows a non-blocking notice when the uploaded data
  looks like an aggregated counts table rather than individual records, since
  disclosure control assumes individual-level microdata.

* New option `dataganger.disable_synthpop`: set
  `options(dataganger.disable_synthpop = TRUE)` to steer objective-derived
  synthesis onto the internal engine even when synthpop is installed. Intended
  for environments where a synthpop synthesis is undesirable or can hang
  unattended (for example continuous integration). An explicit
  `engine = "synthpop"` request is still honoured.

# dataganger 0.2.1

* Shiny app interface refinements: the Objective step shows each purpose's
  details on selection and uses consistent "more bars = stronger" meters,
  including an Anonymity meter for resistance to re-identification. Each
  synthesis engine (auto, internal, synthpop) now has a plain-language
  explainer. The Generation step shows a configuration recap (including
  advanced settings) with an "Adjust settings" shortcut back to Configuration.
  The Comparison step has a wrapping variable grid and a clearer original vs.
  synthetic explainer. The Export step downloads a single bundle (synthetic
  data as CSV, documentation, comparison report, and an analysis notebook),
  with an optional save-to-folder for sessions run locally.

* Every bundle now includes `analysis.qmd`, a Quarto report with runnable R
  code and reference Python code to read both the original and synthetic data
  and compare them (summary statistics, distribution plots, and DataGangeR's
  fidelity metrics).

* The Shiny synthesis spec exposes an engine selector (auto / internal /
  synthpop) with an inline note on whether synthpop is installed.

* Ship an agent skill (`inst/skills/using-dataganger-bundles/`) describing how
  AI agents should consume a bundle: never access the real data, ask the human
  for a go-ahead before touching the synthetic data, and where to save work.

* Fixed `interpolate()` so bundle helper files (e.g. `load_data.R`) render
  their templates instead of shipping literal placeholders.

* DataGangeR now routes relationship-preserving objectives to the optional
  synthpop engine when installed. Please cite: Nowok B, Raab GM, Dibben C
  (2016). "synthpop: Bespoke Creation of Synthetic Data in R." *Journal of
  Statistical Software*, 74(11), 1-26. doi:10.18637/jss.v074.i11

* Initial CRAN-ready scaffold.
