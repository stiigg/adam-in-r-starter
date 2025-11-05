suppressPackageStartupMessages({
  library(targets)
  tar_option_set(packages = c("dplyr","readr","admiral","metacore","xportr","yaml","purrr","tibble","haven"))
})
source("R/build_adsl.R"); source("R/build_adae.R"); source("R/write_xpt.R"); source("R/qc_helpers.R")

list(
  tar_target(dm_path, "sdtm/dm.csv", format = "file"),
  tar_target(ex_path, "sdtm/ex.csv", format = "file"),
  tar_target(ae_path, "sdtm/ae.csv", format = "file"),
  tar_target(spec_adsl_path, "metadata/spec_adsl.csv", format = "file"),
  tar_target(spec_adae_path, "metadata/spec_adae.csv", format = "file"),

  tar_target(dm, readr::read_csv(dm_path, show_col_types = FALSE)),
  tar_target(ex, readr::read_csv(ex_path, show_col_types = FALSE)),
  tar_target(ae, readr::read_csv(ae_path, show_col_types = FALSE)),

  tar_target(adsl, build_adsl(dm, ex)),
  tar_target(adae, build_adae(ae, adsl)),

  tar_target(adsl_xpt, write_xpt(adsl, spec_adsl_path, "ADSL", "deliverables/xpt/adsl.xpt"), format = "file"),
  tar_target(adae_xpt, write_xpt(adae, spec_adae_path, "ADAE", "deliverables/xpt/adae.xpt"), format = "file"),

  tar_target(traceability_csv, qc_traceability_all(list(ADSL = adsl, ADAE = adae)), format = "file")
)
