suppressPackageStartupMessages({ library(admiral); library(dplyr); library(metacore) })

build_adae <- function(ae, adsl, spec) {
  # Load and apply metadata from spec
  metacore <- read_spec(spec)
  ae <- apply_metadata(ae, metacore)

  # Derive datetime and duration
  ae <- ae %>%
    derive_vars_dtm(source_vars = vars(AESTDTC), new_vars_suffix = "TM", highest_imputation = "M") %>%
    mutate(
      AESER = if_else(is.na(AESER), "N", AESER),
      AESEV = recode(AESEV, "MILD"="MILD", "MODERATE"="MODERATE", "SEVERE"="SEVERE", .default = NA_character_),
      AESEVN = recode(AESEV, "MILD"=1L, "MODERATE"=2L, "SEVERE"=3L, .default = NA_integer_),
      AEDUR = as.integer(AEENDTC - AESTDTC)
      # Add more derivations
    )

  # Merge with ADSL & derive study day, TE flag, SAFFL
  adae <- ae %>%
    left_join(select(adsl, STUDYID, USUBJID, TRTSDTM, TRTEDTM, SAFFL, EPOCH), by = c("STUDYID","USUBJID")) %>%
    mutate(
      ASTDY = as.integer(AESTDTM - TRTSDTM),
      AENDY = as.integer(AEENDTM - TRTSDTM),
      ASEFL = if_else(AESTDTM >= TRTSDTM & AESTDTM <= TRTEDTM, "Y", "N")
      # Add more flags
    )

  # Controlled terminology mapping, validation
  adae <- adae %>%
    mutate(
      # Map AETERM to MedDRA PT/SOC, validate codes
    )
  validate_and_log(adae) # Output QC and validation messages/logs

  # Assign attributes and finalize per spec
  adae <- finalize_dataset(adae, metacore)
  return(adae)
}
