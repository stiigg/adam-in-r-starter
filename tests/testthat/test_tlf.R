library(testthat)

test_that("TLF utilities summarize datasets", {
  adae <- tibble::tibble(
    USUBJID = c("01", "02", "03"),
    TRT01A = c("DrugA", "DrugA", "Placebo"),
    AETOXGR = c("1", "2", "1")
  )

  adlb <- tibble::tibble(
    USUBJID = c("01", "01", "02"),
    TRT01A = c("DrugA", "DrugA", "Placebo"),
    AVISIT = c("Baseline", "Week 1", "Baseline"),
    ANRIND = c("NORMAL", "HIGH", "LOW")
  )

  adqs <- tibble::tibble(
    USUBJID = c("01", "01", "02"),
    TRT01A = c("DrugA", "DrugA", "Placebo"),
    PARAM = c("QOL", "QOL", "QOL"),
    AVISIT = c("Baseline", "Week 1", "Baseline"),
    AVAL = c(60, 70, 58),
    CHG = c(0, 10, 0),
    ANL02FL = c(NA_character_, "Y", NA_character_)
  )

  adae_summary <- summarize_adae_by_treatment(adae)
  adlb_summary <- summarize_adlb_shift(adlb)
  adqs_summary <- summarize_adqs_response(adqs)
  listing <- create_listing(adae, c("USUBJID", "TRT01A"))

  expect_true("DrugA" %in% adae_summary$TRT01A)
  expect_true("HIGH" %in% names(adlb_summary))
  expect_true("MEAN" %in% names(adqs_summary))
  expect_equal(ncol(listing), 2)
})
