library(validate)

qc_rules_adsl <- function() {
  validator(
    is.character(STUDYID),
    is.character(USUBJID),
    is.numeric(AGE), AGE >= 18,
    !is.na(TRTSDT),
    !is.na(TRT01A),
    is.na(TRTEDT) | TRTSDT <= TRTEDT
  )
}

qc_rules_adae <- function() {
  validator(
    is.character(USUBJID),
    is.numeric(AESEQ),
    !is.na(ASTDT),
    is.na(AENDT) | ASTDT <= AENDT,
    is.character(TEAEFL),
    is.na(TEAEFL) | TEAEFL %in% c("Y", "N")
  )
}

qc_rules_adtte <- function() {
  validator(
    is.character(USUBJID),
    is.numeric(AVAL), AVAL >= 0,
    CNSR %in% c(0, 1),
    is.character(PARAMCD)
  )
}

qc_rules_adlb <- function() {
  validator(
    is.character(USUBJID),
    is.numeric(AVAL),
    is.numeric(BASE),
    is.numeric(CHG),
    is.character(PARAMCD)
  )
}

qc_rules_adocc <- function() {
  validator(
    is.character(USUBJID),
    is.numeric(OCCSEQ),
    !is.na(OCDT),
    is.character(OCCFL)
  )
}

qc_rules_adqs <- function() {
  validator(
    is.character(USUBJID),
    is.character(PARAMCD),
    is.numeric(AVAL),
    is.numeric(BASE) | is.na(BASE),
    is.character(ANL01FL),
    is.character(AVISIT)
  )
}

qc_dispatch <- list(
  ADSL = qc_rules_adsl,
  ADAE = qc_rules_adae,
  ADLB = qc_rules_adlb,
  ADTTE = qc_rules_adtte,
  ADOCC = qc_rules_adocc,
  ADQS = qc_rules_adqs
)

run_individual_qc <- function(name, dataset) {
  rule_fn <- qc_dispatch[[toupper(name)]]
  if (is.null(rule_fn)) {
    return(NULL)
  }
  cf <- validate::confront(dataset, rule_fn())
  summary(cf)
}

qc_outliers <- function(dataset, variable, lower = -Inf, upper = Inf) {
  if (!variable %in% names(dataset)) {
    return(tibble::tibble())
  }
  dataset |>
    dplyr::filter(!is.na(.data[[variable]]) & (.data[[variable]] < lower | .data[[variable]] > upper)) |>
    dplyr::mutate(flag_variable = variable)
}

qc_cross_dataset <- function(datasets) {
  datasets_with_id <- datasets[vapply(datasets, function(df) "USUBJID" %in% names(df), logical(1))]
  dataset_names <- names(datasets_with_id)
  if (length(dataset_names) < 2) {
    return(list(all_pass = TRUE, results = tibble::tibble()))
  }
  combinations <- utils::combn(dataset_names, 2, simplify = FALSE)
  results <- purrr::map_dfr(combinations, function(pair) {
    df_a <- datasets_with_id[[pair[1]]]
    df_b <- datasets_with_id[[pair[2]]]
    ids_a <- unique(df_a$USUBJID)
    ids_b <- unique(df_b$USUBJID)
    missing_in_b <- setdiff(ids_a, ids_b)
    missing_in_a <- setdiff(ids_b, ids_a)
    tibble::tibble(
      dataset_a = pair[1],
      dataset_b = pair[2],
      missing_in_b = paste(missing_in_b, collapse = ", "),
      missing_in_a = paste(missing_in_a, collapse = ", "),
      pass = length(missing_in_b) == 0 && length(missing_in_a) == 0
    )
  })
  list(all_pass = all(results$pass), results = results)
}

qc_double_programming <- function(dataset, name) {
  reference_path <- file.path("inst", "extdata", paste0(name, "_reference.csv"))
  if (!file.exists(reference_path)) {
    return(tibble::tibble())
  }
  reference <- readr::read_csv(reference_path, show_col_types = FALSE)
  comparison <- dplyr::anti_join(dataset, reference, by = intersect(names(dataset), names(reference)))
  comparison |>
    dplyr::mutate(reference_path = reference_path)
}

qc_traceability_check <- function(dataset, name) {
  trace <- attr(dataset, "traceability")
  tibble::tibble(
    dataset = name,
    has_traceability = !is.null(trace),
    missing_source = if (!is.null(trace)) any(is.na(trace$source)) else NA,
    variables_without_derivation = if (!is.null(trace)) paste(trace$variable[is.na(trace$source)], collapse = ", ") else NA_character_
  )
}

run_qc <- function(...) {
  datasets <- list(...)
  if (length(datasets) == 1 && is.list(datasets[[1]]) && !is.data.frame(datasets[[1]])) {
    datasets <- datasets[[1]]
  }
  if (is.null(names(datasets))) {
    stop("Datasets must be supplied as a named list or named arguments.")
  }
  individual <- Filter(Negate(is.null), purrr::imap(datasets, run_individual_qc))
  cross <- qc_cross_dataset(datasets)
  outliers <- purrr::imap_dfr(datasets, ~ qc_outliers(.x, "AVAL", lower = -999, upper = 999) |> dplyr::mutate(dataset = .y))
  double_programming <- purrr::imap(datasets, qc_double_programming)
  double_programming <- if (length(double_programming)) dplyr::bind_rows(double_programming, .id = "dataset") else tibble::tibble()
  traceability <- purrr::imap(datasets, qc_traceability_check)
  traceability <- dplyr::bind_rows(traceability)
  list(
    individual = individual,
    cross = cross,
    outliers = outliers,
    double_programming = double_programming,
    traceability = traceability
  )
}
