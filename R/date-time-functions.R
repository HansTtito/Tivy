#' Converts a vector of dates in different formats to a standard date format
#'
#' @description
#' The `convert_to_date()` function takes a vector of dates in various formats and converts them to a standard date format in R.
#' The function tries to parse each date using a series of predefined formats and returns the first valid date found for each entry.
#' If a date cannot be interpreted by any of the formats, it is assigned as `NA`.
#'
#' @param date_vector A character vector containing dates in various formats.
#' @param type Type of object to return: "date" for Date, "datetime" for POSIXct (default).
#'
#' @return A vector of `Date` or `POSIXct` objects according to the type parameter, or `NA` if the date cannot be converted.
#'
#' @examples
#'
#' dates <- c("2025-04-10", "10/04/2025", "April 10, 2025")
#'
#' converted_dates <- convert_to_date(dates)
#'
#' print(converted_dates)
#'
#' @export
#' @importFrom lubridate parse_date_time
convert_to_date <- function(date_vector, type = "datetime") {
  # Check if the vector is empty
  if (length(date_vector) == 0) {
    return(date_vector)
  }
  # Format codes for parse_date_time
  orders <- c(
    "Ymd HMS", "Ymd HM", "Ymd",
    "Y/m/d HMS", "Y/m/d HM", "Y/m/d",
    "dmy HMS", "dmy HM", "dmy",
    "d/m/Y HMS", "d/m/Y HM", "d/m/Y",
    "mdy HMS", "mdy HM", "mdy",
    "m/d/Y HMS", "m/d/Y HM", "m/d/Y",
    "bd Y HMS", "bd Y HM", "bd Y",
    "db Y HMS", "db Y HM", "db Y"
  )
  # Convert dates using parse_date_time
  converted_dates <- lubridate::parse_date_time(date_vector, orders = orders, quiet = TRUE)
  # Convert to the specified output type
  if (type == "date") {
    converted_dates <- as.Date(converted_dates)
  }
  return(converted_dates)
}