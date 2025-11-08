# ADaM in R (R-only Scaffold)

![R-CMD-check](https://github.com/stiigg/adam-in-r-starter/actions/workflows/R-CMD-check.yaml/badge.svg)

TL;DR: This repo provides end-to-end ADaM derivation, QC, and traceability tests with automated CI workflows running at every push/PR in a fresh Ubuntu + R environment.

---

This repository mirrors a submission-grade ADaM build-out. It contains derivations for the core subject-level and occurrence domains along with BDS templates, metadata-driven traceability, automated QC, reproducibility scaffolding, and synthetic SDTM inputs so you can run the entire workflow end-to-end in R.

## Workflow Overview

Continuous integration is managed by `.github/workflows/R-CMD-check.yaml`:
- Runs on every push and pull request.
- Sets up Ubuntu + R using r-lib/actions
- Installs all core dependencies (`targets`, `tarchetypes`, `dplyr`, `tidyr`, `testthat`, etc.) per `renv.lock`
- Sources all main scripts in `R/`
- Runs the full `{testthat}` suite for all domain utilities and derivations
- Executes the complete `targets` pipeline from scratch
- Stores build logs, test outputs, traceability results, and QC artifacts

## Domain Coverage

- **ADSL** – population flags (SAF/ITT/PP), exposure windows, and treatment start/end timestamps.
- **ADAE** – treatment-emergent flagging, duration calculations, and start/end relative days.
- **ADTTE** – time-to-first treatment-emergent adverse event with censoring logic.
- **ADLB** – laboratory BDS with baseline, change, percent change, and visit derivations.
- **ADOCC** – occurrence domain patterned after OCCDS with imputed dates and occurrence flags.
- **ADQS** – questionnaire BDS template showcasing baseline handling, analysis flags, and change-from-baseline metrics.

... (rest unchanged)
