# Table, Listing, and Figure utilities for ADaM datasets

summarize_adae_by_treatment <- function(adae) {
  ensure_cols(adae, c("USUBJID", "TRT01A", "AETOXGR"))

  adae |>
    dplyr::group_by(.data$TRT01A, .data$AETOXGR) |>
    dplyr::summarise(N = dplyr::n_distinct(.data$USUBJID), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "AETOXGR", values_from = "N", values_fill = 0) |>
    dplyr::arrange(.data$TRT01A)
}

summarize_adlb_shift <- function(adlb) {
  ensure_cols(adlb, c("USUBJID", "TRT01A", "AVISIT", "ANRIND"))

  adlb |>
    dplyr::filter(!is.na(.data$ANRIND)) |>
    dplyr::group_by(.data$TRT01A, .data$AVISIT, .data$ANRIND) |>
    dplyr::summarise(N = dplyr::n_distinct(.data$USUBJID), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "ANRIND", values_from = "N", values_fill = 0) |>
    dplyr::arrange(.data$TRT01A, .data$AVISIT)
}

summarize_adqs_response <- function(adqs) {
  ensure_cols(adqs, c("TRT01A", "PARAM", "AVISIT", "AVAL", "CHG"))

  adqs |>
    dplyr::filter(!is.na(.data$ANL02FL) & .data$ANL02FL == "Y") |>
    dplyr::group_by(.data$TRT01A, .data$PARAM, .data$AVISIT) |>
    dplyr::summarise(
      N = dplyr::n_distinct(.data$USUBJID),
      MEAN = mean(.data$AVAL, na.rm = TRUE),
      SD = stats::sd(.data$AVAL, na.rm = TRUE),
      MEAN_CHG = mean(.data$CHG, na.rm = TRUE),
      SD_CHG = stats::sd(.data$CHG, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$TRT01A, .data$PARAM, .data$AVISIT)
}

create_listing <- function(dataset, columns) {
  ensure_cols(dataset, columns)
  dataset |>
    dplyr::select(dplyr::all_of(columns)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(columns)))
}
