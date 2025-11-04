# Questionnaire-based BDS derivations (ADQS template)

derive_bds_baseline <- function(df, value_var, day_var) {
  df |>
    dplyr::arrange(.data$USUBJID, .data[[day_var]], .data$AVISITN) |>
    dplyr::group_by(.data$USUBJID, .data$PARAMCD) |>
    dplyr::mutate(
      BASE = {
        valid <- !is.na(.data[[value_var]]) & !is.na(.data[[day_var]]) & .data[[day_var]] <= 1
        ifelse(valid, .data[[value_var]], NA_real_)
      }
    ) |>
    tidyr::fill(BASE, .direction = "down") |>
    dplyr::ungroup()
}

make_adqs <- function(qs, adsl, specs, visit_schedule = NULL) {
  stopifnot(specs$dataset == "ADQS")

  required_qs <- spec_required_columns(specs, "qs")
  needed <- unique(c(required_qs, "QSTEST", "QSTESTCD", "QSCAT", "QSDTC", "QSSTRESN"))
  if ("VISIT" %in% names(qs)) {
    needed <- c(needed, "VISIT")
  }
  qs <- ensure_cols(qs, needed)

  adsl <- ensure_cols(adsl, c("USUBJID", "TRTSDT", "TRTEDT", "TRT01A"))

  qs <- dplyr::mutate(qs, QSSEQ = dplyr::coalesce(.data$QSSEQ, dplyr::row_number()))

  qs$VISIT_SOURCE <- if ("VISIT" %in% names(qs)) qs$VISIT else NA_character_

  adqs <- qs |>
    dplyr::left_join(
      adsl |>
        dplyr::select("USUBJID", "TRTSDT", "TRTEDT", "TRT01A"),
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      QSDT = impute_partial_date(.data$QSDTC),
      QSDTM = derive_datetime(.data$QSDTC, if ("QSTM" %in% names(qs)) .data$QSTM else NULL),
      AVAL = as.numeric(.data$QSSTRESN),
      AVISIT = dplyr::coalesce(.data$QSCAT, .data$VISIT_SOURCE, paste0("Day ", derive_relative_day(.data$QSDT, .data$TRTSDT))),
      AVISITN = derive_visit_window(.data$QSDT, .data$TRTSDT, visit_schedule),
      ADY = derive_relative_day(.data$QSDT, .data$TRTSDT)
    )

  imputation_log <- track_imputation(qs$QSDTC, adqs$QSDT, "QSDT")

  adqs <- adqs |>
    dplyr::mutate(
      PARAMCD = .data$QSTESTCD,
      PARAM = .data$QSTEST,
      ABLFL = set_analysis_flag(.data$ADY <= 1),
      ANL01FL = set_analysis_flag(!is.na(.data$AVAL)),
      ANL02FL = set_analysis_flag(.data$ANL01FL == "Y" & .data$ADY >= 1)
    ) |>
    derive_bds_baseline("AVAL", "ADY") |>
    dplyr::group_by(.data$USUBJID, .data$PARAMCD) |>
    dplyr::arrange(.data$AVISITN, .by_group = TRUE) |>
    dplyr::mutate(
      CHG = .data$AVAL - .data$BASE,
      PCHG = ifelse(is.na(.data$BASE) | .data$BASE == 0, NA_real_, (.data$AVAL - .data$BASE) / abs(.data$BASE) * 100),
      AVISIT = dplyr::if_else(is.na(.data$AVISIT), paste0("Visit ", dplyr::row_number()), .data$AVISIT)
    ) |>
    dplyr::ungroup()

  adqs <- apply_metadata_mapping(adqs, specs)
  adqs <- attach_traceability(adqs, specs, metadata_note = "Derived via make_adqs")
  attr(adqs, "imputation_log") <- imputation_log

  validate_metadata_variables(adqs, specs)
  adqs
}
