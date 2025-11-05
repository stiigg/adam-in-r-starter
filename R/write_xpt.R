suppressPackageStartupMessages({ library(metacore); library(xportr); library(dplyr) })
write_xpt <- function(ds, path_spec, domain, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  mc <- metacore::spec_to_metacore(read.csv(path_spec))
  ds %>%
    xportr_type(metadata = mc, domain = domain) %>%
    xportr_order(metadata = mc, domain = domain) %>%
    xportr_label(metadata = mc, domain = domain) %>%
    xportr_date(metadata = mc, domain = domain) %>%
    xportr_write(file = out_path, metadata = mc, domain = domain, strict_checks = TRUE)
  out_path
}
