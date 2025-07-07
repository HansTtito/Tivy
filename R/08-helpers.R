#' Extract length values from column names
#'
#' @description
#' Helper function to extract numerical length values from column names.
#' Handles different naming patterns like "length_8.5", "weighted_9", "8", etc.
#' Uses multiple extraction strategies to ensure robust parsing of column names.
#'
#' @param column_names Character vector of column names.
#' @param use_fallback Logical. If TRUE, uses fallback strategy when 
#'   numeric values cannot be extracted.
#' @param fallback_type Character. Type of fallback to use when use_fallback = TRUE.
#'   Options: "sequential", "ones", "zeros".
#' @param verbose Logical. Print information about extraction strategy used.
#'
#' @return Numeric vector of length values extracted from column names.
#' 
#' @details
#' The function uses the following extraction strategies in order:
#' \enumerate{
#'   \item Specific prefixes: "length_", "weighted_", "pond_" followed by numbers
#'   \item Purely numeric column names
#'   \item General pattern: extracts first number found in each name
#'   \item Fallback: uses specified fallback strategy if previous methods fail
#' }
#' 
#' @examples
#' extract_numeric_values(c("length_8.5", "weighted_10", "pond_12"))
#' extract_numeric_values(c("8", "10.5", "12"))
#' extract_numeric_values(c("size_8", "data_10.5", "value_12"))
#' extract_numeric_values(c("length_8", "no_numbers"), use_fallback = FALSE)
#' extract_numeric_values(c("bad_name1", "bad_name2"), fallback_type = "ones")
#' 
#' @export
#' @importFrom stringr str_extract
extract_numeric_values <- function(column_names, 
                                  use_fallback = TRUE, 
                                  fallback_type = "sequential",
                                  verbose = FALSE) {
  
  if (!is.character(column_names)) {
    stop("'column_names' must be a character vector.")
  }
  
  if (all(grepl("^(length_|weighted_|pond_)([0-9]+(\\.[0-9]+)?)$", column_names))) {
    values <- as.numeric(gsub("^(length_|weighted_|pond_)", "", column_names))
    if (verbose) {
      message("Extracted values using specific prefix pattern")
    }
    return(values)
  }
  
  if (all(grepl("^[0-9]+(\\.[0-9]+)?$", column_names))) {
    values <- as.numeric(column_names)
    if (verbose) {
      message("Extracted values from purely numeric column names")
    }
    return(values)
  }
  
  values <- suppressWarnings(as.numeric(stringr::str_extract(column_names, "\\d+\\.?\\d*")))
  
  if (!anyNA(values)) {
    if (verbose) {
      message("Extracted values using general numeric pattern")
    }
    return(values)
  }
  
  if (use_fallback) {
    if (verbose) {
      message("⚠ Some names don't contain numbers. Using fallback strategy.")
    }
    
    fallback_values <- switch(fallback_type,
      "sequential" = seq_along(column_names),
      "position" = seq_along(column_names),
      "ones" = rep(1, length(column_names)),
      "zeros" = rep(0, length(column_names)),
      stop("Invalid fallback_type. Use 'sequential', 'ones', or 'zeros'.")
    )
    
    values[is.na(values)] <- fallback_values[is.na(values)]
    return(values)
    
  } else {
    if (anyNA(values)) {
      warning("Some names do not contain numerical values and were converted to NA.")
    }
    return(values)
  }
}

#' Check date range
#'
#' @description
#' Internal helper function to check if a date falls within a specified range.
#'
#' @param date_str Date to check.
#' @param start_date Start date of range.
#' @param end_date End date of range.
#'
#' @return Logical indicating if date is in range.
#' @keywords internal
is_date_in_range <- function(date_str, start_date, end_date) {
  date_part <- convert_to_date(date_str, output_type = "date")
  date <- convert_to_date(date_part, output_type = "date")
  start <- convert_to_date(start_date, output_type = "date")
  end <- convert_to_date(end_date, output_type = "date")
  return(date >= start && date <= end)
}

#' Check required packages
#'
#' @description
#' Internal function to verify required packages are installed.
#'
#' @param packages Character vector of package names to check.
#'
#' @return TRUE if all packages are available, stops execution otherwise.
#' @keywords internal
check_required_packages <- function(packages = c("httr", "rvest", "jsonlite")) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Please install it with install.packages('", pkg, "')")
    }
  }
  return(TRUE)
}

#' Process weighting block
#'
#' @description
#' Helper function that processes a block of data for catch weighting.
#' Used internally by apply_catch_weighting.
#'
#' @param data Data frame block to process.
#' @param length_cols Length column names.
#' @param catch_col Catch column name.
#' @param a Coefficient of length-weight relationship.
#' @param b Exponent of length-weight relationship.
#' @param silence_warnings Logical. Suppress warnings.
#'
#' @return Data frame with weighted columns added.
#' @keywords internal
process_weighting_block <- function(data, length_cols, catch_col, a, b, silence_warnings = FALSE) {
  
  length_values <- as.numeric(sub(".*_", "", length_cols))
  if (all(is.na(length_values))) {
    length_values <- as.numeric(length_cols)
  }
  
  if (any(is.na(length_values))) {
    warning("Could not extract length values, using sequence 1:n")
    length_values <- seq_along(length_cols)
  }
  
  warning_counter <- list(
    catch_na = 0,
    freq_zero = 0,
    weight_zero = 0,
    length_zero = 0
  )
  
  result <- data
  for (i in 1:nrow(data)) {
    catch_i <- data[i, catch_col]
    frequencies_i <- as.numeric(data[i, length_cols])
    
    if (is.na(catch_i) || catch_i <= 0) warning_counter$catch_na <- warning_counter$catch_na + 1
    if (sum(frequencies_i, na.rm = TRUE) == 0) warning_counter$freq_zero <- warning_counter$freq_zero + 1
    if (any(length_values <= 0, na.rm = TRUE)) warning_counter$length_zero <- warning_counter$length_zero + 1
    
    weighted <- weight_by_catch(
      frequency = frequencies_i,
      catch = catch_i,
      length = length_values,
      a = a,
      b = b,
      silence_warnings = TRUE
    )
    
    for (j in seq_along(length_cols)) {
      result[i, paste0("weighted_", length_values[j])] <- weighted[j]
    }
  }
  
  if (!silence_warnings) {
    warnings_text <- character(0)
    if (warning_counter$catch_na > 0) {
      warnings_text <- c(warnings_text, paste0(warning_counter$catch_na, " rows with catch NA or <= 0"))
    }
    if (warning_counter$freq_zero > 0) {
      warnings_text <- c(warnings_text, paste0(warning_counter$freq_zero, " rows with zero frequencies"))
    }
    if (warning_counter$length_zero > 0) {
      warnings_text <- c(warnings_text, "Length values <= 0 may produce invalid results")
    }
    
    if (length(warnings_text) > 0) {
      warning("Processing summary: ", paste(warnings_text, collapse = "; "))
    }
  }
  
  return(result)
}