#' Convert dates to standard format
#'
#' @description
#' Converts a vector of dates in various formats to a standard date format.
#' The function tries to parse each date using predefined formats and returns
#' the first valid date found for each entry. If a date cannot be interpreted,
#' it is assigned as `NA`.
#'
#' @param date_vector A character vector containing dates in various formats.
#' @param output_type Type of object to return: "date" for Date, "datetime" for POSIXct.
#'
#' @return A vector of `Date` or `POSIXct` objects, or `NA` if the date cannot be converted.
#'
#' @examples
#' dates <- c("2025-04-10", "10/04/2025", "April 10, 2025")
#' converted_dates <- convert_to_date(dates)
#' print(converted_dates)
#'
#' @export
#' @importFrom lubridate parse_date_time
convert_to_date <- function(date_vector, output_type = c("date", "datetime")) {
  output_type <- match.arg(output_type)

  if (length(date_vector) == 0 || all(is.na(date_vector))) {
    return(if (output_type == "datetime") as.POSIXct(NA) else as.Date(NA))
  }

  # If already Date or POSIXct
  if (inherits(date_vector, "Date")) {
    return(if (output_type == "datetime") as.POSIXct(date_vector) else date_vector)
  }
  if (inherits(date_vector, "POSIXct")) {
    return(if (output_type == "date") as.Date(date_vector) else date_vector)
  }

  # Vectorize parsing for character input
  formats <- c(
    "Ymd HMS", "Ymd HM", "Ymd",
    "Y/m/d HMS", "Y/m/d HM", "Y/m/d",
    "dmy HMS", "dmy HM", "dmy",
    "d/m/Y HMS", "d/m/Y HM", "d/m/Y",
    "mdy HMS", "mdy HM", "mdy",
    "m/d/Y HMS", "m/d/Y HM", "m/d/Y",
    "bd Y HMS", "bd Y HM", "bd Y",
    "db Y HMS", "db Y HM", "db Y"
  )

  result <- suppressWarnings(
    lubridate::parse_date_time(date_vector, orders = formats, quiet = TRUE)
  )

  if (all(is.na(result))) {
    warning("Some or all dates could not be parsed.")
  }

  if (output_type == "date") {
    return(as.Date(result))
  } else {
    return(result)
  }
}


#' Find column by pattern matching
#'
#' @description
#' Searches for a column in a data frame using multiple pattern options.
#' If multiple columns match, warns the user and returns the first match.
#'
#' @param patterns Character vector of regex patterns to search for.
#' @param column_names Character vector of column names to search in.
#' @param verbose Logical. If TRUE, prints detailed matching information.
#'
#' @return Integer index of the first matching column, or NULL if no match found.
#'
#' @examples
#' cols <- c("codigo_faena", "numero_cala", "especie", "especies_capturadas")
#' species_patterns <- c("especie", "species", "sp")
#' col_index <- find_column(species_patterns, cols)
#'
#' @export
find_column <- function(patterns, column_names, verbose = FALSE) {
  
  if (length(column_names) == 0) {
    return(NULL)
  }
  
  for (pattern in patterns) {
    matches <- grep(pattern, column_names, ignore.case = TRUE)
    
    if (length(matches) > 0) {
      if (length(matches) > 1) {
        matched_names <- column_names[matches]
        warning(sprintf("Multiple columns found for pattern '%s': %s (positions %s). Using first match: '%s' (position %d).",
                       pattern,
                       paste(matched_names, collapse = ", "),
                       paste(matches, collapse = ", "),
                       matched_names[1],
                       matches[1]))
      }
      
      if (verbose && length(matches) == 1) {
        message(sprintf("Pattern '%s' matched column '%s' (position %d)", 
                       pattern, column_names[matches[1]], matches[1]))
      }
      
      return(matches[1])
    }
  }
  
  return(NULL)
}

#' Find columns by pattern
#'
#' @description
#' Identifies columns in a data frame that match a specific pattern.
#' Useful for finding length columns, weight columns, etc.
#'
#' @param data Data frame to search.
#' @param pattern Regular expression pattern to match.
#' @param sort Logical. Sort results numerically.
#'
#' @return Character vector of matching column names.
#'
#' @examples
#' # Create dummy data
#' data <- data.frame(
#'   weighted_8.5 = c(1, 2, 3),
#'   weighted_10 = c(4, 5, 6),
#'   `8` = c(7, 8, 9),
#'   other = c(10, 11, 12)
#' )
#'
#' # Find weighted columns
#' weighted_cols <- find_columns_by_pattern(data, pattern = "weighted_")
#'
#' # Find numeric-only named columns (e.g., "8")
#' length_cols <- find_columns_by_pattern(data, pattern = "")
#'
#' @export
find_columns_by_pattern <- function(data, pattern = "weighted_", sort = TRUE) {
  
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.character(pattern)) stop("'pattern' must be a character string.")
  
  column_names <- colnames(data)
  reg_exp <- paste0("^", pattern, "[0-9]+(\\.[0-9]+)?$")
  matching_columns <- column_names[grep(reg_exp, column_names)]
  
  if (length(matching_columns) == 0) {
    warning("No columns matching pattern found: ", pattern)
    return(character(0))
  }
  
  if (sort) {
    numerical_values <- safe_numeric_conversion(gsub(pattern, "", matching_columns))
    matching_columns <- matching_columns[order(numerical_values)]
  }
  
  return(matching_columns)
}


#' Create download directory
#'
#' @description
#' Creates download directory if it doesn't exist.
#'
#' @param download_dir Path to download directory.
#' @param verbose Show detailed messages.
#'
#' @return TRUE if directory was created or already exists.
#'
#' @keywords internal
create_download_dir <- function(download_dir, verbose = TRUE) {
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
    if (verbose) message("Created download directory: ", download_dir)
  }
  return(TRUE)
}

#' Safe numeric conversion
#'
#' @description
#' Safely converts values to numeric with warning handling.
#'
#' @param x Vector to convert.
#' @param column_name Name of column being converted (for warnings).
#'
#' @return Numeric vector.
#'
#' @keywords internal
safe_numeric_conversion <- function(x, column_name = "column") {
  if (is.numeric(x)) {
    return(x)
  }
  
  result <- suppressWarnings(as.numeric(x))
  
  if (any(is.na(result) & !is.na(x))) {
    warning("Some values in ", column_name, " could not be converted to numeric and were set to NA")
  }
  
  return(result)
}
