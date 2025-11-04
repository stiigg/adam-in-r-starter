library(targets)
library(tarchetypes)
source("R/utils.R")
source("R/derivations_adsl.R")
source("R/qc.R")
source("R/exports.R")

tar_option_set(format = "rds")

list(
  tar_target(dm, read_sdtm("data_raw/dm.csv"), format = "file"),
  tar_target(ex, read_sdtm("data_raw/ex.csv"), format = "file"),
  tar_target(dm_df, readr::read_csv(dm, show_col_types = FALSE)),
  tar_target(ex_df, readr::read_csv(ex, show_col_types = FALSE) |> dplyr::mutate(EXSTDTC = as.Date(EXSTDTC))),
  tar_target(specs, read_specs("metadata/specs_adsl.yaml")),
  tar_target(adsl, make_adsl(dm_df, ex_df, specs)),
  tar_target(qc_adsl, run_qc(adsl)),
  tar_target(adsl_xpt, write_xpt_adsl(adsl, "ADSL.xpt"), format = "file")
)
