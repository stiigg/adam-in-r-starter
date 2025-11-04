library(testthat)

test_that("ADSL has required columns", {
  dm <- tibble::tibble(STUDYID = "STUDY1", USUBJID = c("01", "02"), AGE = c(55, 60))
  ex <- tibble::tibble(
    USUBJID = c("01", "02"),
    EXTRT = c("DrugA", "Placebo"),
    EXSTDTC = c("2024-01-01", "2024-01-05"),
    EXENDTC = c("2024-02-01", "2024-01-20")
  )
  specs <- read_specs("metadata/specs_adsl.yaml")
  adsl <- make_adsl(dm, ex, specs)
  expect_true(all(c(
    "STUDYID", "USUBJID", "AGE", "TRTSDT", "TRTEDT", "TRT01A", "TRTSDTM",
    "TRTAN", "SAFFL", "ITTFL", "PPFL", "EOSDT"
  ) %in% names(adsl)))
  expect_equal(unique(adsl$SAFFL), "Y")
  expect_equal(unique(adsl$ITTFL), "Y")
  expect_equal(unique(adsl$PPFL), "Y")
  expect_s3_class(attr(adsl, "traceability"), "tbl_df")
})
