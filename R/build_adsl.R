suppressPackageStartupMessages({ library(admiral); library(dplyr) })
build_adsl <- function(dm, ex) {
  dm %>%
    derive_vars_merged(dataset_add = ex, filter_add = !is.na(EXSTDTM),
                       new_vars = exprs(TRTSDTM = EXSTDTM),
                       by_vars = exprs(STUDYID, USUBJID), order = exprs(EXSTDTM), mode = "first") %>%
    derive_vars_merged(dataset_add = ex, filter_add = !is.na(EXENDTM),
                       new_vars = exprs(TRTEDTM = EXENDTM),
                       by_vars = exprs(STUDYID, USUBJID), order = exprs(EXENDTM), mode = "last") %>%
    derive_var_trtdurd(start_date = TRTSDTM, end_date = TRTEDTM, out_var = TRTSDUR)
}
