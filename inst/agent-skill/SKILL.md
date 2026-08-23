You are not allowed to read the original data.

# DataGangeR agent workflow

Use DataGangeR to work only from synthetic data generated from the user's real dataset. Never open, preview, sample, parse, or inspect the real data file directly. Treat the real data file as an opaque input to DataGangeR.

## First step: reproduce the bundle exactly

From inside `agent/`, run:

```sh
dataganger synthesize <real-data> --recipe recipe.yaml --out check.zip
```

Then compare the reproduced synthetic CSV to `../synthetic_data.csv`:

```sh
unzip -p check.zip synthetic_data.csv > check_synthetic_data.csv
cmp -s check_synthetic_data.csv ../synthetic_data.csv
diff -u ../synthetic_data.csv check_synthetic_data.csv
```

Proceed only if the files are identical. If they differ, stop and report that reproduction failed.

## Files you may use

Work only from these bundle artifacts:

- `recipe.yaml`
- `manifest.json`
- `code_readiness_report.json` (may be absent)
- `../human/human.md`
- `../synthetic_data.csv`

Do not assume other files exist.

Hard rule: Before using the data, read `manifest.json`. If `blockers` is non-empty, STOP and tell the user; do not analyse or build on the data until a human regenerates or acknowledges.

## Column names and schema

Column names may vary because the name strategy may rename them. Never assume original column names. Read the names and mappings from `recipe.yaml`'s `name_map` when present, and use `../human/human.md` for the treatment list describing how each output column was handled.

## Allowed workflow

1. Reproduce the synthetic output exactly with the command above.
2. Work only from `../synthetic_data.csv` and the listed bundle metadata files.
3. Inspect the synthetic data, profile it, write code against it, and propose transformations using only the synthetic bundle.
4. If `code_readiness_report.json` is present, use it to catch structural mismatches that would break code on the original data.
5. If the user wants variations, update `recipe.yaml` with user-approved changes and synthesize again.

## Never do this

- Do not read the original data into R, Python, SQL, spreadsheets, or any other tool.
- Do not open the original CSV, Excel, SAS, or other source file for inspection.
- Do not infer that a synthetic column name matches an original name unless `recipe.yaml` or `../human/human.md` supports it.
- Do not claim the output is risk-free or anonymous.

## Framing

This workflow reduces direct disclosure risk by keeping the agent on synthetic data and reproducible bundle artifacts, but it is not a guarantee of privacy or anonymity. Users still need to review fidelity, privacy warnings, and sharing context before external release.
