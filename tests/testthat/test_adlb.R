library(testthat)

test_that("ADLB calculates change from baseline", {
  adsl <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01"),
    TRTSDT = as.Date("2024-01-01"),
    TRT01A = "DrugA"
  )

  lb <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = "01",
    LBSEQ = c(1, 2),
    LBTEST = "ALT",
    LBTESTCD = "ALT",
    LBDTC = c("2023-12-30", "2024-01-03"),
    LBTM = c("08:00", "09:00"),
    LBSTRESN = c(30, 45),
    LBCAT = c("SCREEN", "DAY 3"),
    ANRIND = c("NORMAL", "HIGH")
  )

  specs <- read_specs("metadata/specs_adlb.yaml")
  adlb <- make_adlb(lb, adsl, specs)
  subject <- dplyr::filter(adlb, LBSEQ == 2)

  expect_equal(subject$BASE, 30)
  expect_equal(subject$AVAL, 45)
  expect_equal(subject$CHG, 15)
  expect_true("ANL01FL" %in% names(adlb))
})
