# Known-sensitive column-name terms (conservative)

`detect_roles()` auto-marks a column `disclosure_role = "sensitive"` when its
name matches one of these patterns. The list is intentionally narrow: a missed
sensitive column is recoverable at the Configure gate (the user must choose a
role for every column), while a false positive is annoying. Review and extend
deliberately.

diagnos, icd, disease, condition, symptom, race (word-start), ethnic, religio,
sexual, orientation, gender_identity, hiv, sti, std, mental_health, disabilit,
income, salary, wage (word-start), earnings, criminal, conviction, immigration

Word-boundary anchors are applied to short terms (race, wage, condition, and the
acronyms icd/hiv/sti/std) so they do not match benign substrings such as
"trace", "sewage", or "unconditional".

Not auto-classified (left unselected for the user): geography (city/zip/region),
dates, low-cardinality categoricals, measures and counts. These are common
quasi-identifiers but the quasi decision is the user's to make.
