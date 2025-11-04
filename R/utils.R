read_sdtm <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

read_specs <- function(path) yaml::read_yaml(path)

ensure_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  df
}
