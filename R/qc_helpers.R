suppressPackageStartupMessages({ library(readr); library(dplyr); library(purrr); library(tibble) })
qc_traceability <- function(dataset_name, ds, sources = character()) {
  tibble(dataset = dataset_name, n_records = nrow(ds),
         variables = paste(names(ds), collapse = ","),
         sources = paste(sources, collapse = ","),
         timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}
qc_traceability_all <- function(datasets) {
  out <- "deliverables/traceability/traceability_summary.csv"
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  trs <- imap_dfr(datasets, ~ qc_traceability(.y, .x, names(datasets)))
  readr::write_csv(trs, out); out
}
