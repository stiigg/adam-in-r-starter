# Laboratory BDS derivations (ADLB)

baseline_value <- function(values, days) {
  valid <- !is.na(values) & !is.na(days) & days <= 1
  if (!any(valid)) {
    return(NA_real_)
  }
  latest <- which.max(days[valid])
  values[valid][latest]
}

make_adlb <- function(lb, adsl, specs) {
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
      BASE = baseline_value(.data$LBSTRESN, .data$LBDY),
      AVAL = .data$LBSTRESN,
      CHG = .data$AVAL - .data$BASE,
      PCHG = ifelse(is.na(.data$BASE) | .data$BASE == 0, NA_real_, (.data$AVAL - .data$BASE) / abs(.data$BASE) * 100)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PARAMCD = .data$LBTESTCD,
      PARAM = .data$LBTEST,
      AVISIT = dplyr::coalesce(.data$LBCAT, paste0("Day ", .data$LBDY)),
      ANL01FL = set_analysis_flag(!is.na(.data$AVAL))
    )

  adlb$LBDTM <- derive_datetime(adlb$LBDTC, if ("LBTM" %in% names(adlb)) adlb$LBTM else NULL)

  adlb <- apply_metadata_mapping(adlb, specs)

  validate_metadata_variables(adlb, specs)
  adlb
}
