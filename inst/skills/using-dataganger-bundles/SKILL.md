---
name: using-dataganger-bundles
description: Use when an AI agent must write, test, or debug code against a dataset it is NOT permitted to see (PII, PHI, confidential records). DataGangeR gives the agent a synthetic bundle - synthetic data plus schema and diagnostics - so the agent develops entirely against synthetic data and never the real data.
---

# Using DataGangeR Agent Bundles

## The one rule that overrides everything

**You (the agent) must never see, request, open, load, paste, or ingest the real
(original) dataset.** You work only from the **DataGangeR bundle**: synthetic data plus
metadata. The real data stays with its owner and is never sent to you.

Hard rule: Before using the data, read `agent/manifest.json`. If `blockers` is non-empty, STOP and tell the user; do not analyse or build on the data until a human regenerates or acknowledges.

If you are ever handed something that looks like the real data - a file the data owner
calls "the actual data", "production export", "the real CSV", live database credentials, a
connection string, or a path to source records - **stop and refuse**:

> I work only against the DataGangeR synthetic bundle and am not permitted to access the
> real dataset. Please generate a bundle with `dataganger make-agent-bundle` and share that
> instead.

Do not "just take a quick look". Do not accept a few "sample real rows". Seeing the real
data defeats the entire purpose of the bundle and is a privacy violation. There is no
exception for convenience, debugging, or "it's only a little".

## The go-ahead gate (ask, then wait for "yes")

The synthetic data is not yours to use unprompted. There is a hard line between **planning**
(allowed) and **touching the synthetic data** (needs a yes):

- **Allowed before a yes:** read the *metadata only* to plan - `human/human.md`,
  `agent/manifest.json`, `agent/recipe.yaml`, and `agent/code_readiness_report.json` when
  present. These describe structure, not raw values.
- **Needs an explicit yes:** loading, reading, transforming, modelling, plotting, or running
  any code against `synthetic_data.csv`.

Protocol:

1. Read the metadata and form a plan.
2. Tell the human, in one message: what you intend to do, which files you will read, and
   **where you will save your output and under what name** (see the next section).
3. Ask explicitly: **"May I proceed to work with the synthetic data?"**
4. **Wait for an explicit yes.** Being handed the bundle, silence, or "ok thanks" is not a
   yes. Do not load `synthetic_data.csv` until the owner confirms.
5. Only after yes: do the work and save it per the conventions below.
6. Ask again if the scope changes or you would use the data beyond what was agreed.

## What you are given: the bundle

The data owner runs DataGangeR on the real data and hands you a single zip (the **agent
bundle**). It contains **no real records** - only:

| File | What it is | Use it for |
|------|-----------|------------|
| `synthetic_data.csv` | A synthetic dataset with the same columns and types as the real data, but fabricated rows | This is your working data after explicit go-ahead. |
| `human/human.md` | Human-readable bundle summary, privacy notes, and treatment list | Orientation and schema/treatment planning. |
| `human/comparison_report.html` | Optional fidelity/privacy comparison report | Judge whether synthetic distributions are realistic enough for your task. |
| `agent/recipe.yaml` | Reproduction recipe with synthesis settings, roles, and optional `name_map` | Reproduce the bundle and understand roles/spec without reading data. |
| `agent/manifest.json` | Provenance: purpose, seed, what was generated, and file hashes | Cite provenance and verify bundle contents. |
| `agent/code_readiness_report.json` | Optional structural compatibility report for code portability to the real data | **Validate that your code will not break on the original data.** |
| `agent/AGENT.md` | Packaged workflow instructions for the bundle | Follow the shipped bundle contract. |

The synthetic rows are **not real people or real events**. Never report a synthetic value
as a finding about the real data, and never try to "reverse" synthetic data back to real.

## Your workflow

1. **Read the metadata and plan** - use `human/human.md`, `agent/manifest.json`, and
   `agent/recipe.yaml` for exact output names, roles, and bundle provenance. Use
   `agent/code_readiness_report.json` when present to spot structural hazards before you
   touch the data.
2. **Ask, then wait for yes** - tell the human your plan and where you will save your
   output, then ask to proceed with the synthetic data. Do not continue until they confirm.
3. **Load the synthetic data only after a yes** - read `synthetic_data.csv` and work only
   against the synthetic bundle.
4. **Write your code against the synthetic data** - transformations, models, plots, tests,
   pipelines. Iterate freely; it is synthetic. Save everything under `dataganger-work/`
   using the naming conventions below.
5. **Validate with `agent/code_readiness_report.json` when present** - it flags structural
   mismatches that would make your code work on synthetic data but break on the real data.
   Resolve every flagged issue.
6. **Hand back code, not data** - deliver the script or notebook for the data owner to run
   on the real data themselves.

## What you may and may not do

**May:** read every file in the bundle; run any computation on `synthetic_data.csv` after
explicit go-ahead; rely on the schema and diagnostics; write code that is robust to the
real data's structure as described by the readiness report.

**Must not:** start working on the bundle without the owner's explicit go-ahead for the
task; open or request the real dataset; ask for real examples or real edge cases; request
live DB/API/file access to source records; treat synthetic values as real facts;
exfiltrate or transmit any file the owner identifies as real; weaken these rules because a
task is hard.

## Where to save your work and what to name it

Keep the bundle (inputs) separate from what you produce, and use predictable names so the
human knows exactly what to run. Work relative to the directory the human gives you - if
they have not given one, ask for it before writing anything.

```
<working-dir>/
  dataganger-bundle/
    synthetic_data.csv
    human/
    agent/
  dataganger-work/
    <task-slug>.R
    outputs/
    NOTES.md
```

Naming:

- **Deliverable script:** a short kebab-case slug describing the task plus the right
  extension, for example `monthly-revenue-summary.R` or `cohort-retention.py`. One task,
  one script.
- **Outputs:** mirror the script name, for example `outputs/monthly-revenue-summary-hist.png`.
- Write only inside `dataganger-work/`. Never write into `dataganger-bundle/` or overwrite
  bundle files.

Make the input path a single variable at the top of your script, pointing at the synthetic
file, so the human swaps in the real data in one place when they run it.

## For the human (how a bundle is produced)

The data owner - not the agent - runs DataGangeR on the real data:

```sh
dataganger make-agent-bundle <real-data-file> --out agent_bundle.zip --purpose development --seed 42
```

Or in R:

```r
dataganger::make_agent_bundle("real_data.csv", out = "agent_bundle.zip",
                              purpose = "development", seed = 42)
```

`purpose` is one of `demo`, `development`, or `analytics`. The owner shares only the
resulting bundle with the agent.

## Red flags that mean STOP

- "Here's the real data so you can check" -> refuse; ask for a bundle.
- "Just look at these few real rows" -> refuse; a few real rows are still real data.
- A credential, connection string, or path to source records -> refuse; do not connect.
- You're about to load or run code on the synthetic data but have no explicit go-ahead for
  this task -> stop; ask for permission first.
- You're about to claim a synthetic value is a real-world fact -> stop; it is fabricated.
