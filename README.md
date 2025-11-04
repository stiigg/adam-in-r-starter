# ADaM in R (R-only Scaffold)

This repository now mirrors a submission-grade ADaM build-out. It contains derivations for the core subject-level and occurrence domains along with BDS templates, metadata-driven traceability, automated QC, reproducibility scaffolding, and synthetic SDTM inputs so you can run the entire workflow end-to-end in R.

## Domain Coverage

- **ADSL** – population flags (SAF/ITT/PP), exposure windows, and treatment start/end timestamps.
- **ADAE** – treatment-emergent flagging, duration calculations, and start/end relative days.
- **ADTTE** – time-to-first treatment-emergent adverse event with censoring logic.
- **ADLB** – laboratory BDS with baseline, change, percent change, and visit derivations.
- **ADOCC** – occurrence domain patterned after OCCDS with imputed dates and occurrence flags.
- **ADQS** – questionnaire BDS template showcasing baseline handling, analysis flags, and change-from-baseline metrics.

Every derivation consumes YAML specifications in `metadata/`, attaches traceability metadata, and honours codelists. New utilities in `R/utils.R` expose shared building blocks for partial-date imputation, visit windowing, population flagging, and traceability reporting.

## Metadata & Traceability

- Specifications are parsed through `metacore` (when available) so variable order, type, and labels can be carried through to export.
- `apply_metadata_mapping()` enforces the metadata contract while persisting a traceability tibble on each ADaM dataset.
- `traceability_report()` collapses those attributes into a consolidated audit trail target produced by the pipeline.

## Quality Control & Validation

- Domain-level rule sets powered by `{validate}` plus cross-dataset subject reconciliation.
- Automated outlier detection for numeric endpoints and optional double-programming comparison against `inst/extdata/*_reference.csv` files.
- Traceability presence checks ensure every delivered dataset records its source lineage.
- Comprehensive `{testthat}` coverage exercises utilities, derivations, QC, and TLF helpers; run with `testthat::test_dir("tests/testthat")`.

## Reproducibility

- A curated `renv.lock` (R 4.2 baseline) pins package versions. Restore with:
  ```r
  install.packages("renv")
  renv::restore()
  ```
- Use `{pkglite}` to bundle the project for archival: `pkglite::bundle_packages("inst/extdata", output = "adam_in_r_pkgs.txt")`.
- Session provenance can be captured via `sessionInfo()`; see `targets` pipeline outputs for automated manifests.

## Example Data

Synthetic SDTM-like CSVs reside in `data_raw/` (`dm`, `ex`, `ae`, `lb`, `occ`, and `qs`). Update them with anonymised or randomised extracts to exercise additional scenarios. Metadata and codelists can be extended through the YAML assets in `metadata/`.

## Running the Workflow

```r
renv::activate()
library(targets)
tar_make()
```

Key targets:

- `adsl`, `adae`, `adtte`, `adlb`, `adocc`, `adqs` – fully derived ADaM datasets.
- `qc_results` – list containing domain validations, cross-subject checks, outliers, double-programming findings, and traceability summaries.
- `traceability` – consolidated traceability tibble suitable for audit-ready documentation.
- `tlf_adae`, `tlf_adlb`, `tlf_adqs` – starter tables illustrating AE counts, lab shifts, and questionnaire summaries.
- `*_xpt` targets – SAS XPORT exports authored through `{xportr}` with metadata overlays from `metacore` when present.

## QC Outside of `targets`

To execute the QC layer manually:

```r
datasets <- list(ADSL = adsl_df, ADAE = adae_df, ADQS = adqs_df)
qc <- run_qc(datasets)
qc$individual
qc$cross$results
qc$double_programming
```

## Table/Listing/Figure Utilities

The `R/tlf.R` helpers build quick summaries for ADAE, ADLB, and ADQS domains and a generic listing constructor. Extend these to drive automated mock shells or production-quality outputs.

## Exporting

`_targets.R` orchestrates XPORT generation through helper wrappers in `R/exports.R`. Metadata-integrated export ensures variable order, type, and labels remain submission compliant.

## Next Steps

- Expand metadata specs or plug in sponsor-specific SDTM extracts.
- Replace the synthetic reference deliverables in `inst/extdata/` with double-programmed outputs from an independent programmer.
- Layer on TLF automation (e.g., via `{gtsummary}` or `{tern}`) using the derived datasets as inputs.
- Wire session reporting and environment manifests into your document control system for audit support.
