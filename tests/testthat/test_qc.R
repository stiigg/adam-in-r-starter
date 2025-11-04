library(testthat)

test_that("run_qc returns individual and cross dataset results", {
  adsl <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02"),
    AGE = c(55, 60),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-05")),
    TRTEDT = as.Date(c("2024-02-01", "2024-01-20")),
    TRT01A = c("DrugA", "Placebo")
  )

  adae <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01"),
    AESEQ = 1,
    ASTDT = as.Date("2024-01-10"),
    AENDT = as.Date("2024-01-11"),
    TEAEFL = "Y"
  )

  results <- run_qc(list(ADSL = adsl, ADAE = adae))
  expect_true("individual" %in% names(results))
  expect_true("cross" %in% names(results))
  expect_true("outliers" %in% names(results))
  expect_true("double_programming" %in% names(results))
  expect_true("traceability" %in% names(results))
  expect_true(results$cross$all_pass)
  expect_s3_class(results$traceability, "tbl_df")
})
