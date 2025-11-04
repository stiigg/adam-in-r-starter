library(testthat)

test_that("partial date imputation fills missing components", {
  dates <- impute_partial_date(c("2024", "2024-02", "2024-02-15"))
  expect_equal(as.character(dates[1]), "2024-01-01")
  expect_equal(as.character(dates[2]), "2024-02-01")
  expect_equal(as.character(dates[3]), "2024-02-15")
})

test_that("duration handles NA inputs", {
  start <- as.Date(c("2024-01-01", NA))
  end <- as.Date(c("2024-01-05", "2024-01-10"))
  duration <- derive_duration(start, end)
  expect_equal(duration[1], 4)
  expect_true(is.na(duration[2]))
})

test_that("analysis flag applies as expected", {
  flag <- set_analysis_flag(c(TRUE, FALSE, NA))
  expect_equal(flag, c("Y", NA, NA))
})

test_that("population flags and visit windows derive correctly", {
  pop <- derive_population_flag(c(TRUE, FALSE, NA))
  expect_equal(pop, c("Y", "N", NA))

  visit_schedule <- tibble::tibble(VISITDT = as.Date(c("2024-01-01", "2024-01-08")), VISITNUM = c(1, 2))
  visit <- derive_visit_window(as.Date(c("2024-01-02", "2024-01-10")), as.Date("2024-01-01"), visit_schedule)
  expect_equal(visit, c(1, 2))
})

test_that("traceability report consolidates metadata", {
  specs <- read_specs("metadata/specs_adsl.yaml")
  df <- tibble::tibble(STUDYID = "STUDY1", USUBJID = "01", AGE = 50, TRTSDT = as.Date("2024-01-01"))
  df <- apply_metadata_mapping(df, specs)
  report <- traceability_report(list(df))
  expect_s3_class(report, "tbl_df")
  expect_true("dataset" %in% names(report))
})
