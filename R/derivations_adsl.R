# Core subject-level derivations for ADSL and shared utilities

first_exposure_date <- function(ex) {
  ex |>
    dplyr::arrange(.data$USUBJID, .data$EXSTDTC) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(TRTSDT = as.Date(dplyr::first(.data$EXSTDTC)), .groups = "drop")
}

last_exposure_date <- function(ex) {
  if (!"EXENDTC" %in% names(ex)) {
    return(dplyr::tibble(USUBJID = unique(ex$USUBJID), TRTEDT = as.Date(NA)))
  }
  ex |>
    dplyr::arrange(.data$USUBJID, .data$EXENDTC) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(TRTEDT = as.Date(dplyr::last(.data$EXENDTC)), .groups = "drop")
}

first_treatment_label <- function(ex) {
  ex |>
    dplyr::arrange(.data$USUBJID, .data$EXSTDTC) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(TRT01A = dplyr::first(.data$EXTRT), .groups = "drop")
}

make_adsl <- function(dm, ex, specs) {
  stopifnot(specs$dataset == "ADSL")

  dm <- ensure_cols(dm, c("STUDYID", "USUBJID", "AGE"))
  ex <- ensure_cols(ex, c("USUBJID", "EXTRT", "EXSTDTC"))

  trtsdt <- first_exposure_date(ex)
  trtedt <- last_exposure_date(ex)
  trt01a <- first_treatment_label(ex)

  adsl <- dm |>
    dplyr::select("STUDYID", "USUBJID", "AGE") |>
    dplyr::left_join(trtsdt, by = "USUBJID") |>
    dplyr::left_join(trtedt, by = "USUBJID") |>
    dplyr::left_join(trt01a, by = "USUBJID") |>
    dplyr::mutate(
      TRTSDT = as.Date(.data$TRTSDT),
      TRTEDT = as.Date(.data$TRTEDT),
      TRTSDTM = derive_datetime(.data$TRTSDT),
      TRTAN = dplyr::row_number(),
      SAFFL = derive_population_flag(!is.na(.data$TRTSDT)),
      ITTFL = derive_population_flag(!is.na(.data$TRTSDT)),
      PPFL = derive_population_flag(!is.na(.data$TRTEDT)),
      EOSDT = dplyr::coalesce(.data$TRTEDT, .data$TRTSDT)
    ) |>
    apply_metadata_mapping(specs)

  validate_metadata_variables(adsl, specs)
  adsl
}
