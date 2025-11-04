# Time-to-event derivations for ADTTE domain

make_adtte <- function(adsl, ae, specs) {
  stopifnot(specs$dataset == "ADTTE")

  required_ae <- spec_required_columns(specs, "ae")
  if (length(required_ae)) {
    ae <- ensure_cols(ae, required_ae)
  }

  adsl <- ensure_cols(adsl, c("USUBJID", "TRTSDT", "TRTEDT", "TRT01A"))

  event_dates <- ae |>
    dplyr::mutate(ASTDT = impute_partial_date(.data$AESTDTC)) |>
    dplyr::filter(!is.na(.data$ASTDT)) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(EVENTDT = min(.data$ASTDT, na.rm = TRUE), .groups = "drop")

  adtte <- adsl |>
    dplyr::left_join(event_dates, by = "USUBJID") |>
    dplyr::mutate(
      CNSR = ifelse(is.na(.data$EVENTDT), 1L, 0L),
      AVAL = ifelse(.data$CNSR == 0L,
        derive_duration(.data$TRTSDT, .data$EVENTDT),
        derive_duration(.data$TRTSDT, dplyr::coalesce(.data$TRTEDT, .data$TRTSDT))),
      PARAMCD = "TTAE",
      PARAM = "Time to first treatment-emergent AE",
      ANL01FL = set_analysis_flag(!is.na(.data$AVAL)),
      AVALU = "DAYS",
      SRC = dplyr::if_else(.data$CNSR == 0L, "EVENT", "CENSOR")
    )

  adtte$EVNTDTM <- derive_datetime(adtte$EVENTDT)

  adtte <- apply_metadata_mapping(adtte, specs)
  validate_metadata_variables(adtte, specs)
  adtte
}
