prepare_for_export <- function(df, specs = NULL) {
  if (!requireNamespace("xportr", quietly = TRUE)) {
    stop("xportr package is required for XPT export")
  }

  meta <- NULL
  if (!is.null(specs)) {
    meta <- specs$metacore %||% try_build_metacore(specs)
  }

  if (!is.null(meta)) {
    df <- tryCatch(xportr::xportr_type(df, meta), error = function(e) df)
    df <- tryCatch(xportr::xportr_format(df, meta), error = function(e) df)
    df <- tryCatch(xportr::xportr_label(df, meta), error = function(e) df)
    df <- tryCatch(xportr::xportr_order(df, meta), error = function(e) df)
  }

  df
}

write_xpt_adsl <- function(adsl, specs, path = "ADSL.xpt") {
  adsl <- prepare_for_export(adsl, specs)
  xportr::xportr_write(adsl, path)
  path
}

write_xpt_adae <- function(adae, specs, path = "ADAE.xpt") {
  adae <- prepare_for_export(adae, specs)
  xportr::xportr_write(adae, path)
  path
}

write_xpt_adtte <- function(adtte, specs, path = "ADTTE.xpt") {
  adtte <- prepare_for_export(adtte, specs)
  xportr::xportr_write(adtte, path)
  path
}

write_xpt_adlb <- function(adlb, specs, path = "ADLB.xpt") {
  adlb <- prepare_for_export(adlb, specs)
  xportr::xportr_write(adlb, path)
  path
}

write_xpt_adocc <- function(adocc, specs, path = "ADOCC.xpt") {
  adocc <- prepare_for_export(adocc, specs)
  xportr::xportr_write(adocc, path)
  path
}

write_xpt_adqs <- function(adqs, specs, path = "ADQS.xpt") {
  adqs <- prepare_for_export(adqs, specs)
  xportr::xportr_write(adqs, path)
  path
}
