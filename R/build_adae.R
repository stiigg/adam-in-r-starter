suppressPackageStartupMessages({ library(admiral); library(dplyr) })
build_adae <- function(ae, adsl) {
  ae %>%
    derive_vars_dtm(source_vars = vars(AESTDTC), new_vars_suffix = "TM", highest_imputation = "M") %>%
    mutate(
      AESER  = if_else(is.na(AESER), "N", AESER),
      AESEV  = recode(AESEV, "MILD"="MILD","MODERATE"="MODERATE","SEVERE"="SEVERE", .default = NA_character_),
      AESEVN = recode(AESEV, "MILD"=1L,"MODERATE"=2L,"SEVERE"=3L, .default = NA_integer_)
    ) %>%
    left_join(select(adsl, STUDYID, USUBJID, TRTSDTM, TRTEDTM), by = c("STUDYID","USUBJID"))
}
