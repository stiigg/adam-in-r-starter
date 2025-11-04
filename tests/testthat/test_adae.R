library(testthat)

test_that("ADAE derives treatment-emergent flags and durations", {
  adsl <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-05")),
    TRTEDT = as.Date(c("2024-02-01", "2024-01-20")),
    TRT01A = c("DrugA", "Placebo")
  )

  ae <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "01", "02"),
    AESEQ = c(1, 2, 1),
    AETERM = c("Headache", "Nausea", "Fatigue"),
    AESTDTC = c("2024-01-02", "2024-01-05", "2024-03"),
    AEENDTC = c("2024-01-03", "2024-01-07", "2024-03-10"),
    AETOXGR = c("1", "2", "1")
  )

  specs <- read_specs("metadata/specs_adae.yaml")
  adae <- make_adae(ae, adsl, specs)

  expect_true(all(c("ASTDT", "AENDT", "TEAEFL", "ADURN") %in% names(adae)))
  expect_true(any(adae$TEAEFL == "Y"))
  expect_true(any(adae$ADURN >= 1))
})
