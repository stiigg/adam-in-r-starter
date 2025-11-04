library(testthat)

test_that("ADOCC derives occurrence flags", {
  adsl <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "02"),
    TRTSDT = as.Date(c("2024-01-01", "2024-01-05")),
    TRT01A = c("DrugA", "Placebo")
  )

  occ <- tibble::tibble(
    STUDYID = "STUDY1",
    USUBJID = c("01", "01", "02"),
    OCCSEQ = c(1, 2, 1),
    OCCCAT = "Follow-up",
    OCCUR = c("Y", "N", "Y"),
    OCDTC = c("2024-02-15", "2024-03-01", "2024-02-18"),
    OCTM = c("10:00", "09:45", "11:00")
  )

  specs <- read_specs("metadata/specs_adocc.yaml")
  adocc <- make_adocc(occ, adsl, specs)

  expect_true(all(c("OCCFL", "OCDT", "OCDTM") %in% names(adocc)))
  expect_equal(adocc$OCCFL[adocc$OCCSEQ == 1 & adocc$USUBJID == "01"], "Y")
  expect_true(any(is.na(adocc$OCCFL[adocc$OCCSEQ == 2 & adocc$USUBJID == "01"])))
})
