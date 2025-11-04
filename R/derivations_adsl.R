// Build ADSL from SDTM-like DM and EX

first_exposure_date <- function(ex) {
  ex |>
    dplyr::arrange(.data$USUBJID, .data$EXSTDTC) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(TRTSDT = as.Date(dplyr::first(.data$EXSTDTC)), .groups = "drop")
}

first_treatment_label <- function(ex) {
  ex |>
    dplyr::arrange(.data$USUBJID, .data$EXSTDTC) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(TRT01A = dplyr::first(.data$EXTRT), .groups = "drop")
}

make_adsl <- function(dm, ex, specs) {
  stopifnot(specs$dataset == "ADSL")
  dm <- ensure_cols(dm, c("STUDYID","USUBJID","AGE"))
  ex <- ensure_cols(ex, c("USUBJID","EXTRT","EXSTDTC"))

  trtsdt <- first_exposure_date(ex)
  trt01a <- first_treatment_label(ex)

  adsl <- dm |>
    dplyr::select("STUDYID", "USUBJID", "AGE") |>
    dplyr::left_join(trtsdt, by = "USUBJID") |>
    dplyr::left_join(trt01a, by = "USUBJID") |>
    dplyr::mutate(TRTSDT = as.Date(.data$TRTSDT))

  adsl
}
