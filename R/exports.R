write_xpt_adsl <- function(adsl, path = "ADSL.xpt") {
  xportr::xportr_write(adsl, path)
  path
}
