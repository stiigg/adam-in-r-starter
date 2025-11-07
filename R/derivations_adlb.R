# Laboratory BDS derivations (ADLB) - Best Practice 2025 Update

# Step 1: Modular Baseline Calculation Using Robust Parameter-Invariant Logic
baseline_value <- function(values, days, baseline_type = "LBDY_LE_1") {
  valid <- !is.na(values) & !is.na(days) & days <= 1
  if (!any(valid)) return(NA_real_)
  latest <- which.max(days[valid])
  values[valid][latest]
}

# Step 2: Reference Range and Grading Flags (ANRIND, etc.)
derive_anrind <- function(value, low, high) {
  if (is.na(value) | is.na(low) | is.na(high)) return(NA_character_)
  if (value < low) return("LOW")
  if (value > high) return("HIGH")
  return("NORMAL")
}

# Step 3: Extremes, Baseline, and Worst Flags
derive_baseline_flag <- function(lbdys) {
  as.character(ifelse(lbdys == min(lbdys, na.rm = TRUE), "Y", NA))
}
derive_worst_flag <- function(vals) {
  as.character(ifelse(vals == min(vals, na.rm = TRUE) | vals == max(vals, na.rm = TRUE), "Y", NA))
}

# Step 4: Core ADLB Derivation Pipeline (Best Practice Alignment)
make_adlb <- function(lb, adsl, specs, param_meta = NULL) {
  stopifnot(specs$dataset == "ADLB")

  required_lb <- spec_required_columns(specs, "lb")
  if (length(required_lb)) lb <- ensure_cols(lb, required_lb)
  adsl <- ensure_cols(adsl, c("USUBJID", "TRTSDT", "TRT01A"))

  adlb <- lb |>
    dplyr::left_join(adsl |>
      dplyr::select("USUBJID", "TRTSDT", "TRT01A"), by = "USUBJID") |>
    dplyr::mutate(
      LBDT = impute_partial_date(.data$LBDTC),
      LBDY = derive_relative_day(.data$LBDT, .data$TRTSDT)
    ) |>
    dplyr::group_by(.data$USUBJID, .data$LBTESTCD) |>
    # Step 5: Derive baseline and flags FIRST
    dplyr::mutate(
      BASE = baseline_value(.data$LBSTRESN, .data$LBDY),
      ABLFL = derive_baseline_flag(.data$LBDY),
      # Step 6: Main value and derived calculations
      AVAL = .data$LBSTRESN,
      CHG = .data$AVAL - .data$BASE,
      PCHG = ifelse(is.na(.data$BASE) | .data$BASE == 0, NA_real_, (.data$AVAL - .data$BASE) / abs(.data$BASE) * 100),
      DTYPE = NA_character_,
      WORSTFL = derive_worst_flag(.data$LBSTRESN)
    ) |>
    dplyr::ungroup() |>
    # Step 7: PARAM/meta assignment next
    dplyr::mutate(
      PARAMCD = .data$LBTESTCD,
      PARAM = .data$LBTEST,
      VISIT = .data$VISIT,
      VISITNUM = .data$VISITNUM,
      EPOCH = .data$EPOCH,
      # Step 8: Key analysis/flag derivations
      ANL01FL = set_analysis_flag(!is.na(.data$AVAL)),
      ANRIND = derive_anrind(.data$LBSTRESN, .data$LBNRLO, .data$LBNRHI),
      LBSEQ = .data$LBSEQ,
      LBCAT = .data$LBCAT,
      LBDTC = .data$LBDTC,
      ITTFL = "Y", SAFFL = "Y", PPFL = NA_character_
    )

  # Step 9: Datetime derivation must follow all prior calcs
  adlb$LBDTM <- derive_datetime(adlb$LBDTC, if ("LBTM" %in% names(adlb)) adlb$LBTM else NULL)

  # Step 10: Parameter meta join after main derivation
  if (!is.null(param_meta))
    adlb <- dplyr::left_join(adlb, param_meta, by = "PARAMCD")

  # Step 11: Meta-data mapping and validation last
  adlb <- apply_metadata_mapping(adlb, specs)
  validate_metadata_variables(adlb, specs)

  # Optional: Grading/CTCAE extensions can be modulated here
  # Document cross-step requirements and references to ADaMIG v1.1 or later
  adlb
}

# Documentation: This workflow is modular, stepwise and best-practice aligned as of 2025 (CDISC ADaMIG v1.1+, interchanges, pharmaverse)
# Derivation steps: BASE -> Flag -> Change/Percent -> PARAM/meta -> Key flag assignments -> Datetime -> Mapping -> Final QA/validation
# Validation/testing assumed via testthat or downstream as per sponsor/QA best-practice recommendations.
# See also: https://pharmaverse.github.io/admiral/, https://cran.r-project.org/web/packages/admiral/vignettes/adlb.html
