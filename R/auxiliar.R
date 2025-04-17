#' Search for columns that contain a pattern followed by a number
#'
#' Identifies columns in a dataframe whose names follow the pattern
#' of a specific prefix followed by a number.
#'
#' @param data A data frame to search for columns.
#' @param pattern The pattern or prefix to search for (default "pond_").
#' @param sort Indicates if columns should be sorted numerically (default TRUE).
#' @return Character vector with the names of columns that match the pattern.
#' @export
#' @examples
#' 
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#' 
#' # Identify all pond_X columns in the dataframe
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_trips <- merge(x = data_fishing_trips, y = hauls_length, by = 'fishing_trip_code')
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls, data_length_fishing_trips = data_length_trips)
#' final_data <- add_variables(data_total)
#'
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#' length_weighted <- weight_length_df(df = final_data, length_cols = length_cols, catch_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984)
#'
#' pond_cols <- search_pattern_columns(data = length_weighted, pattern = "pond_")
#'
#' # Use the found columns for calculations
#' length_weighted[, pond_cols]
search_pattern_columns <- function(data, pattern = "pond_", sort = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.character(pattern)) stop("The 'pattern' parameter must be a text string.")

  # Get all column names
  column_names <- colnames(data)

  # Search for columns that contain the pattern
  reg_exp <- paste0("^", pattern, "[0-9]+(\\.[0-9]+)?$")
  matching_columns <- column_names[grep(reg_exp, column_names)]

  if (length(matching_columns) == 0) {
    warning("No columns matching the pattern were found: ", pattern)
    return(character(0))
  }

  # Sort numerically if necessary
  if (sort) {
    # Extract numerical values from column names
    numerical_values <- as.numeric(gsub(pattern, "", matching_columns))
    # Sort columns according to these values
    matching_columns <- matching_columns[order(numerical_values)]
  }

  return(matching_columns)
}



#' Extract numerical values from column names
#'
#' This function extracts all numerical values (including decimals) from a vector
#' of column names, removing any non-numeric characters.
#'
#' @param column_names Character vector with column names.
#' @return Numeric vector with the extracted length values.
#' @export
#' @examples
#' names <- c("pond_10.5", "length_11", "12.5mm", "T14", "13-error")
#' extract_length_values(names)
#' # Result: 10.5 11.0 12.5 14.0 13.0
extract_length_values <- function(column_names) {
  if (!is.character(column_names)) {
    stop("The 'column_names' parameter must be a character vector.")
  }

  # Extract the first occurrence of a number in each string (supports decimals)
  values <- as.numeric(stringr::str_extract(column_names, "\\d+\\.?\\d*"))

  if (any(is.na(values))) {
    warning("Some names do not contain numerical values and were converted to NA.")
  }

  return(values)
}


#' Get names and positions of columns that match a pattern
#'
#' Returns both the names and positions of columns in a dataframe
#' that follow the specified pattern.
#'
#' @param data A data frame to search for columns.
#' @param pattern The pattern or prefix to search for (default "pond_").
#' @param sort Indicates if results should be sorted numerically (default TRUE).
#' @return List with two elements: "positions" and "names" of matching columns.
#' @export
#' @examples
#' # Get both positions and names
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_trips <- merge(x = data_fishing_trips, y = hauls_length, by = 'fishing_trip_code')
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls, data_length_fishing_trips = data_length_trips)
#' final_data <- add_variables(data_total)
#'
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5","12", "12.5", "13", "13.5", "14", "14.5", "15")
#' length_weighted <- weight_length_df(df = final_data, length_cols = length_cols, catch_col = "catch_ANCHOVETA", a = 0.0001, b = 2.984)
#'
#' result <- pattern_columns_info(length_weighted, "pond_")
#'
#' positions <- result$positions
#' print(positions)
#'
#' names <- result$names
#' print(names)
pattern_columns_info <- function(data, pattern = "pond_", sort = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.character(pattern)) stop("The 'pattern' parameter must be a text string.")

  # Get all column names
  column_names <- colnames(data)

  # Search for columns that contain the pattern
  reg_exp <- paste0("^", pattern, "[0-9]+(\\.[0-9]+)?$")
  indices <- grep(reg_exp, column_names)

  if (length(indices) == 0) {
    warning("No columns matching the pattern were found: ", pattern)
    return(list(positions = integer(0), names = character(0)))
  }

  matching_columns <- column_names[indices]

  # Sort numerically if necessary
  if (sort) {
    # Extract numerical values from column names
    numerical_values <- as.numeric(gsub(pattern, "", matching_columns))
    # Sort according to these values
    order <- order(numerical_values)
    indices <- indices[order]
    matching_columns <- matching_columns[order]
  }

  return(list(
    positions = indices,
    names = matching_columns
  ))
}



#' Calculate juvenile percentages for a set of frequencies
#'
#' Helper function that calculates juvenile percentages both in number and weight
#' from a set of length frequencies.
#'
#' @param frequencies Numeric vector with frequencies by length.
#' @param length_values Numeric vector with the corresponding length values.
#' @param juvLim Length limit to consider juveniles (default 12 cm).
#' @param a Coefficient of the length-weight relationship.
#' @param b Exponent of the length-weight relationship.
#' @return Data frame with juvenile percentages and totals.
#' @export
#' @examples
#' frequencies <- c(10, 15, 25, 30, 20, 10)
#' length_values <- c(8, 9, 10, 11, 12, 13)
#' calculate_juveniles(frequencies, length_values)
calculate_juveniles <- function(frequencies, length_values, juvLim = 12, a = 0.0012, b = 3.1242) {
  # Parameter validation
  if (!is.numeric(frequencies)) stop("The 'frequencies' parameter must be numeric.")
  if (!is.numeric(length_values)) stop("The 'length_values' parameter must be numeric.")
  if (length(frequencies) != length(length_values))
    stop("The 'frequencies' and 'length_values' vectors must have the same length.")

  # Check if there is data
  total_number <- sum(frequencies, na.rm = TRUE)

  if (total_number == 0) {
    # If there is no data, return NA without warnings
    return(data.frame(
      perc_juv_number = NA_real_,
      perc_juv_weight = NA_real_,
      total_number = 0,
      total_weight = 0
    ))
  }

  # Calculate juveniles in number with the existing function
  # We use suppressWarnings to avoid redundant warnings
  perc_juv_number <- suppressWarnings(juvenile_percentage(frequencies, length_values, juvLim))

  # Calculate weights
  weights <- length_weight(length_values, a, b) * frequencies
  total_weight <- sum(weights, na.rm = TRUE)

  # Calculate juveniles in weight with the same function
  if (total_weight == 0) {
    perc_juv_weight <- NA_real_
  } else {
    perc_juv_weight <- suppressWarnings(juvenile_percentage(weights, length_values, juvLim))
  }

  return(data.frame(
    perc_juv_number = perc_juv_number,
    perc_juv_weight = perc_juv_weight,
    total_number = total_number,
    total_weight = total_weight
  ))
}