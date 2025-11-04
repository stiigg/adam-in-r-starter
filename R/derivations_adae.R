# Adverse event derivations for ADAE domain

make_adae <- function(ae, adsl, specs) {
  stopifnot(specs$dataset == "ADAE")

  required_ae <- spec_required_columns(specs, "ae")
  if (length(required_ae)) {
    ae <- ensure_cols(ae, required_ae)
  }

  adsl_keys <- c("USUBJID", "TRTSDT", "TRTEDT", "TRT01A")
  adsl <- ensure_cols(adsl, intersect(adsl_keys, names(adsl)))

  adae <- ae |>
    dplyr::left_join(
      adsl |>
        dplyr::select(dplyr::all_of(adsl_keys)),
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      ASTDT = impute_partial_date(.data$AESTDTC),
      AENDT = impute_partial_date(.data$AEENDTC),
      ASTDY = derive_relative_day(.data$ASTDT, .data$TRTSDT),
      AENDY = derive_relative_day(.data$AENDT, .data$TRTSDT),
      ADURN = derive_duration(.data$ASTDT, .data$AENDT) + 1,
      TEAEFL = set_analysis_flag(.data$ASTDT >= .data$TRTSDT &
        (is.na(.data$TRTEDT) | .data$ASTDT <= .data$TRTEDT)),
      AOCCFL = set_analysis_flag(.data$ADURN >= 1)
    ) |>
    dplyr::mutate(AESEQ = dplyr::coalesce(.data$AESEQ, dplyr::row_number(), 1L))

  adae$ASTDTM <- derive_datetime(adae$AESTDTC, if ("AESTTM" %in% names(adae)) adae$AESTTM else NULL)
  adae$AENDTM <- derive_datetime(adae$AEENDTC, if ("AEENTM" %in% names(adae)) adae$AEENTM else NULL)

  adae <- apply_metadata_mapping(adae, specs)

  validate_metadata_variables(adae, specs)
  adae
}
