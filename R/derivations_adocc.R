# Occurrence dataset derivations (AOCC/ADCM-like OCCDS structure)

make_adocc <- function(occ, adsl, specs) {
  stopifnot(specs$dataset == "ADOCC")

  required_occ <- spec_required_columns(specs, "occ")
  if (length(required_occ)) {
    occ <- ensure_cols(occ, required_occ)
  }

  adsl <- ensure_cols(adsl, c("USUBJID", "TRTSDT", "TRT01A"))

  adocc <- occ |>
    dplyr::left_join(
      adsl |>
        dplyr::select("USUBJID", "TRTSDT", "TRT01A"),
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      OCDT = impute_partial_date(.data$OCDTC),
      OCDY = derive_relative_day(.data$OCDT, .data$TRTSDT),
      OCCFL = set_analysis_flag(.data$OCCUR %in% c("Y", "YES", "1")),
      OCCSEQ = dplyr::coalesce(.data$OCCSEQ, dplyr::row_number())
    )

  adocc$OCDTM <- derive_datetime(adocc$OCDTC, if ("OCTM" %in% names(adocc)) adocc$OCTM else NULL)

  adocc <- apply_metadata_mapping(adocc, specs)

  validate_metadata_variables(adocc, specs)
  adocc
}
