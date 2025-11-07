# Laboratory BDS derivations (ADLB)

# Modular Baseline Calculation Supporting Multiple Definitions
baseline_value <- function(values, days, baseline_type = "LBDY_LE_1") {
  if (baseline_type == "LBDY_LE_1") {
    valid <- !is.na(values) & !is.na(days) & days <= 1
  } else {
    # Custom baseline logic can be added here
    valid <- !is.na(values) & !is.na(days)
  }
  if (!any(valid)) {
    return(NA_real_)
  }
  latest <- which.max(days[valid])
  values[valid][latest]
}

# Reference Range Flagging
reference_range_flag <- function(value, low, high) {
  if (is.na(value) | is.na(low) | is.na(high)) return(NA_character_)
  if (value < low) return("LOW")
  if (value > high) return("HIGH")
  return("NORMAL")
}

make_adlb <- function(lb, adsl, specs, param_meta = NULL) {
  stopifnot(specs$dataset == "ADLB")

  required_lb <- spec_required_columns(specs, "lb")
  if (length(required_lb)) {
    lb <- ensure_cols(lb, required_lb)
  }
  adsl <- ensure_cols(adsl, c("USUBJID", "TRTSDT", "TRT01A"))

  adlb <- lb |>
    dplyr::left_join(
      adsl |>
        dplyr::select("USUBJID", "TRTSDT", "TRT01A"),
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      LBDT = impute_partial_date(.data$LBDTC),
      LBDY = derive_relative_day(.data$LBDT, .data$TRTSDT)
    ) |>
    dplyr::group_by(.data$USUBJID, .data$LBTESTCD) |>
    dplyr::mutate(
      BASETYPE = "LBDY_LE_1", # Placeholder supports extension for multiple definitions
      BASE = baseline_value(.data$LBSTRESN, .data$LBDY, BASETYPE),
      ABLFL = ifelse(.data$LBDY == min(.data$LBDY, na.rm=TRUE), "Y", NA_character_),
      AVAL = .data$LBSTRESN,
      CHG = .data$AVAL - .data$BASE,
      PCHG = ifelse(is.na(.data$BASE) | .data$BASE == 0, NA_real_, (.data$AVAL - .data$BASE) / abs(.data$BASE) * 100),
      DTYPE = NA_character_ # Imputation/derivation info can be set by logic
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PARAMCD = .data$LBTESTCD,
      PARAM = .data$LBTEST,
      VISIT = .data$VISIT, # expects VISIT from LB
      VISITNUM = .data$VISITNUM, # expects VISITNUM from LB
      EPOCH = .data$EPOCH, # expects EPOCH from LB or mapping
      ANL01FL = set_analysis_flag(!is.na(.data$AVAL)),
      # Example reference range derivation
      ANRIND = reference_range_flag(.data$LBSTRESN, .data$LBNRLO, .data$LBNRHI),
      # Traceability
      LBSEQ = .data$LBSEQ,
      LBCAT = .data$LBCAT,
      LBDTC = .data$LBDTC,
      # Analysis denominator flags (placeholder, update per study)
      ITTFL = "Y",
      SAFFL = "Y",
      PPFL = NA_character_
    )

  adlb$LBDTM <- derive_datetime(adlb$LBDTC, if ("LBTM" %in% names(adlb)) adlb$LBTM else NULL)

  # Parameter metadata join
  if (!is.null(param_meta)) {
    adlb <- dplyr::left_join(adlb, param_meta, by = "PARAMCD")
  }

  adlb <- apply_metadata_mapping(adlb, specs)
  validate_metadata_variables(adlb, specs)

  # Labeling stub
  # adlb <- apply_variable_labels(adlb, specs) # if label mapping function exists
  adlb
}
