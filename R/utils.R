`%||%` <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 0)) y else x
}

read_sdtm <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

read_specs <- function(path) {
  specs <- yaml::read_yaml(path)
  specs$metacore <- try_build_metacore(specs)
  specs
}

ensure_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  df
}

spec_variables <- function(specs) {
  purrr::map_dfr(specs$variables, function(var) {
    source <- var$source %||% NA_character_
    if (!is.na(source) && stringr::str_detect(source, "\\.")) {
      pieces <- stringr::str_split_fixed(source, "\\.", 2)
      source_dataset <- pieces[, 1]
      source_variable <- pieces[, 2]
    } else {
      source_dataset <- NA_character_
      source_variable <- NA_character_
    }

    tibble::tibble(
      variable = var$name,
      type = var$type %||% NA_character_,
      source_dataset = source_dataset,
      source_variable = source_variable,
      derivation = var$derivation %||% NA_character_
    )
  })
}

spec_required_columns <- function(specs, dataset) {
  vars <- spec_variables(specs)
  vars$source_dataset <- tolower(vars$source_dataset)
  dataset <- tolower(dataset)
  vars |>
    dplyr::filter(.data$source_dataset == dataset) |>
    dplyr::pull(.data$source_variable) |>
    stats::na.omit() |>
    unique()
}

build_traceability <- function(specs) {
  vars <- spec_variables(specs)
  vars |>
    dplyr::mutate(
      dataset = specs$dataset,
      source = dplyr::if_else(
        !is.na(.data$source_dataset) & !is.na(.data$source_variable),
        paste0(.data$source_dataset, ".", .data$source_variable),
        .data$derivation
      )
    ) |>
    dplyr::select("dataset", variable = "variable", "source", "derivation", "type")
}

apply_metadata_mapping <- function(df, specs, metacore = NULL) {
  vars <- spec_variables(specs)
  ordered_vars <- intersect(vars$variable, names(df))
  df <- dplyr::select(df, dplyr::all_of(ordered_vars))

  meta <- metacore %||% specs$metacore
  if (!is.null(meta) && requireNamespace("metatools", quietly = TRUE)) {
    metatools_ns <- asNamespace("metatools")
    add_meta <- get0("add_var_metadata", envir = metatools_ns, inherits = FALSE)
    order_meta <- get0("order_vars", envir = metatools_ns, inherits = FALSE)
    if (is.function(add_meta)) {
      df <- tryCatch(add_meta(df, meta), error = function(e) df)
    }
    if (is.function(order_meta)) {
      df <- tryCatch(order_meta(df, meta), error = function(e) df)
    }
  }

  attr(df, "traceability") <- build_traceability(specs)
  df
}

impute_partial_date <- function(date_char, default_day = 1, default_month = 1) {
  vapply(date_char, function(value) {
    if (inherits(value, "Date")) {
      return(value)
    }
    if (is.na(value) || !nzchar(value)) {
      return(as.Date(NA))
    }
    parts <- unlist(strsplit(value, "-"))
    if (!length(parts)) {
      return(as.Date(NA))
    }
    year <- suppressWarnings(as.integer(parts[1]))
    month <- suppressWarnings(as.integer(parts[2] %||% default_month))
    day <- suppressWarnings(as.integer(parts[3] %||% default_day))
    if (is.na(year)) {
      return(as.Date(NA))
    }
    month <- ifelse(is.na(month), default_month, month)
    day <- ifelse(is.na(day), default_day, day)
    lubridate::make_date(year = year, month = month, day = day)
  }, FUN.VALUE = as.Date(NA))
}

derive_datetime <- function(date_char, time_char = NULL, default_time = "00:00:00") {
  dates <- impute_partial_date(date_char)
  if (is.null(time_char)) {
    time_char <- rep(default_time, length(dates))
  }
  time_char <- ifelse(is.na(time_char) | !nzchar(time_char), default_time, time_char)
  parsed <- paste(dates, time_char)
  suppressWarnings(lubridate::ymd_hms(parsed, quiet = TRUE))
}

derive_duration <- function(start, end, unit = "days", na_value = NA_real_) {
  if (all(is.na(start)) || all(is.na(end))) {
    return(rep(na_value, length(start)))
  }
  diffs <- difftime(end, start, units = unit)
  as.numeric(diffs)
}

derive_relative_day <- function(date, reference) {
  ifelse(is.na(date) | is.na(reference), NA_integer_, as.integer(difftime(date, reference, units = "days")) + 1L)
}

set_analysis_flag <- function(condition, flag_value = "Y") {
  ifelse(condition, flag_value, NA_character_)
}

derive_population_flag <- function(condition, true_flag = "Y", false_flag = "N") {
  dplyr::case_when(
    is.na(condition) ~ NA_character_,
    condition ~ true_flag,
    TRUE ~ false_flag
  )
}

derive_visit_window <- function(visit_date, reference_date, visit_schedule) {
  if (missing(visit_schedule) || is.null(visit_schedule)) {
    return(derive_relative_day(visit_date, reference_date))
  }
  purrr::map_dbl(visit_date, function(x) {
    if (is.na(x)) {
      return(NA_real_)
    }
    diffs <- abs(as.numeric(x - visit_schedule$VISITDT))
    visit_schedule$VISITNUM[which.min(diffs)]
  })
}

coalesce_with_na <- function(...) {
  purrr::reduce(list(...), function(x, y) dplyr::coalesce(x, y))
}

track_imputation <- function(original, imputed, variable) {
  tibble::tibble(
    variable = variable,
    original = original,
    imputed = imputed,
    changed = !is.na(imputed) & (is.na(original) | original != imputed)
  )
}

validate_metadata_variables <- function(df, specs) {
  vars <- spec_variables(specs)$variable
  missing <- setdiff(vars, names(df))
  if (length(missing)) {
    stop("Dataset is missing variables defined in specifications: ", paste(missing, collapse = ", "))
  }
  invisible(df)
}

collect_traceability <- function(datasets) {
  purrr::map_dfr(datasets, function(dataset) {
    trace <- attr(dataset, "traceability")
    if (is.null(trace)) {
      return(tibble::tibble())
    }
    trace
  })
}

try_build_metacore <- function(specs) {
  if (!requireNamespace("metacore", quietly = TRUE)) {
    return(NULL)
  }

  variables <- spec_variables(specs)
  ds_spec <- tibble::tibble(
    dataset = specs$dataset,
    label = specs$label %||% specs$dataset,
    keys = paste(specs$keys %||% character(), collapse = " ")
  )

  var_spec <- variables |>
    dplyr::mutate(
      dataset = specs$dataset,
      order = dplyr::row_number(),
      length = NA_real_,
      label = .data$variable,
      core = TRUE
    ) |>
    dplyr::select("dataset", variable = "variable", "type", "label", "length", "order", "core")

  tryCatch(
    metacore::metacore(
      ds_spec = ds_spec,
      var_spec = var_spec,
      value_spec = NULL,
      supp_spec = NULL,
      code_spec = NULL
    ),
    error = function(e) {
      warning("Unable to build metacore object: ", conditionMessage(e))
      NULL
    }
  )
}

attach_traceability <- function(df, specs, metadata_note = NULL) {
  trace <- build_traceability(specs)
  if (!is.null(metadata_note)) {
    trace$note <- metadata_note
  }
  attr(df, "traceability") <- trace
  df
}

traceability_report <- function(datasets) {
  traces <- collect_traceability(datasets)
  if (!nrow(traces)) {
    return(traces)
  }
  if (!"note" %in% names(traces)) {
    traces$note <- NA_character_
  }
  traces |>
    dplyr::arrange(.data$dataset, .data$variable)
}
