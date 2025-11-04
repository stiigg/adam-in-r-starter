library(targets)
library(tarchetypes)
source("R/utils.R")
source("R/derivations_adsl.R")
source("R/derivations_adae.R")
source("R/derivations_adtte.R")
source("R/derivations_adlb.R")
source("R/derivations_adocc.R")
source("R/derivations_adqs.R")
source("R/qc.R")
source("R/exports.R")
source("R/tlf.R")

tar_option_set(format = "rds")

list(
  tar_target(dm_path, "data_raw/dm.csv", format = "file"),
  tar_target(ex_path, "data_raw/ex.csv", format = "file"),
  tar_target(ae_path, "data_raw/ae.csv", format = "file"),
  tar_target(lb_path, "data_raw/lb.csv", format = "file"),
  tar_target(occ_path, "data_raw/occ.csv", format = "file"),
  tar_target(qs_path, "data_raw/qs.csv", format = "file"),
  tar_target(dm_df, read_sdtm(dm_path)),
  tar_target(ex_df, read_sdtm(ex_path)),
  tar_target(ae_df, read_sdtm(ae_path)),
  tar_target(lb_df, read_sdtm(lb_path)),
  tar_target(occ_df, read_sdtm(occ_path)),
  tar_target(qs_df, read_sdtm(qs_path)),
  tar_target(specs_adsl, read_specs("metadata/specs_adsl.yaml")),
  tar_target(specs_adae, read_specs("metadata/specs_adae.yaml")),
  tar_target(specs_adtte, read_specs("metadata/specs_adtte.yaml")),
  tar_target(specs_adlb, read_specs("metadata/specs_adlb.yaml")),
  tar_target(specs_adocc, read_specs("metadata/specs_adocc.yaml")),
  tar_target(specs_adqs, read_specs("metadata/specs_adqs.yaml")),
  tar_target(adsl, make_adsl(dm_df, ex_df, specs_adsl)),
  tar_target(adae, make_adae(ae_df, adsl, specs_adae)),
  tar_target(adtte, make_adtte(adsl, ae_df, specs_adtte)),
  tar_target(adlb, make_adlb(lb_df, adsl, specs_adlb)),
  tar_target(adocc, make_adocc(occ_df, adsl, specs_adocc)),
  tar_target(adqs, make_adqs(qs_df, adsl, specs_adqs)),
  tar_target(qc_results, run_qc(list(ADSL = adsl, ADAE = adae, ADTTE = adtte, ADLB = adlb, ADOCC = adocc, ADQS = adqs))),
  tar_target(traceability, traceability_report(list(adsl, adae, adtte, adlb, adocc, adqs))),
  tar_target(tlf_adae, summarize_adae_by_treatment(adae)),
  tar_target(tlf_adlb, summarize_adlb_shift(adlb)),
  tar_target(tlf_adqs, summarize_adqs_response(adqs)),
  tar_target(adsl_xpt, write_xpt_adsl(adsl, specs_adsl, "ADSL.xpt"), format = "file"),
  tar_target(adae_xpt, write_xpt_adae(adae, specs_adae, "ADAE.xpt"), format = "file"),
  tar_target(adtte_xpt, write_xpt_adtte(adtte, specs_adtte, "ADTTE.xpt"), format = "file"),
  tar_target(adlb_xpt, write_xpt_adlb(adlb, specs_adlb, "ADLB.xpt"), format = "file"),
  tar_target(adocc_xpt, write_xpt_adocc(adocc, specs_adocc, "ADOCC.xpt"), format = "file"),
  tar_target(adqs_xpt, write_xpt_adqs(adqs, specs_adqs, "ADQS.xpt"), format = "file")
)
