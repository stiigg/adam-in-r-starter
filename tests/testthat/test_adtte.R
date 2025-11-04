library(testthat)

test_that("ADTTE derives censoring and AVAL", {
  adsl <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-05")),
    TRTEDT = as.Date(c("2024-02-01", "2024-01-20")),
    TRT01A = c("DrugA", "Placebo")
  )

  ae <- tibble::tibble(
    USUBJID = c("01", "01"),
    AESTDTC = c("2024-01-10", "2024-01-12")
  )

  specs <- read_specs("metadata/specs_adtte.yaml")
  adtte <- make_adtte(adsl, ae, specs)

  expect_true(all(c("AVAL", "CNSR", "PARAMCD", "EVENTDT") %in% names(adtte)))
  expect_equal(adtte$CNSR[adtte$USUBJID == "01"], 0)
  expect_equal(adtte$CNSR[adtte$USUBJID == "02"], 1)
})
