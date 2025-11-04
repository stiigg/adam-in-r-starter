library(testthat)

test_that("ADSL has required columns", {
  dm <- tibble::tibble(STUDYID = "STUDY1", USUBJID = c("01","02"), AGE = c(55, 60))
  ex <- tibble::tibble(USUBJID = c("01","02"), EXTRT = c("DrugA","Placebo"), EXSTDTC = as.Date(c("2024-01-01","2024-01-05")))
  specs <- yaml::read_yaml(textConnection("
  dataset: ADSL
  variables: []
  "))
  source("R/utils.R"); source("R/derivations_adsl.R")
  adsl <- make_adsl(dm, ex, specs)
  expect_true(all(c("STUDYID","USUBJID","AGE","TRTSDT","TRT01A") %in% names(adsl)))
})
