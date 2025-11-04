library(validate)

qc_rules_adsl <- function() {
  validator(
    is.character(STUDYID),
    is.character(USUBJID),
    is.numeric(AGE), AGE >= 18,
    !is.na(TRTSDT),
    is.character(TRT01A)
  )
}

run_qc <- function(adsl) {
  cf <- confront(adsl, qc_rules_adsl())
  summary(cf)
}
