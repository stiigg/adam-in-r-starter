library(testthat)

test_that("ADQS derivations populate baseline and change", {
  adsl <- tibble::tibble(
    USUBJID = c("01", "02"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-05")),
    TRTEDT = as.Date(c("2024-02-01", "2024-01-20")),
    TRT01A = c("DrugA", "Placebo")
  )

  qs <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = rep(c("01", "02"), each = 2),
    QSSEQ = c(1, 2, 1, 2),
    QSTEST = "Quality of Life Total Score",
    QSTESTCD = "QOL",
    QSCAT = c("SCREEN", "DAY 1", "SCREEN", "DAY 1"),
    QSDTC = c("2023-12-28", "2024-01-02", "2023-12-30", "2024-01-05"),
    QSTM = "08:00",
    QSSTRESN = c(60, 70, 58, 63),
    VISIT = c("Screening", "Week 1", "Screening", "Week 1")
  )

  specs <- read_specs("metadata/specs_adqs.yaml")
  adqs <- make_adqs(qs, adsl, specs)

  expect_true(all(c("PARAMCD", "AVAL", "BASE", "CHG", "ANL01FL", "ANL02FL") %in% names(adqs)))
  expect_true(any(adqs$ABLFL == "Y"))
  subject01 <- dplyr::filter(adqs, USUBJID == "01", PARAMCD == "QOL")
  expect_equal(subject01$BASE[subject01$AVISITN == min(subject01$AVISITN, na.rm = TRUE)], 60)
  expect_s3_class(attr(adqs, "traceability"), "tbl_df")
})
