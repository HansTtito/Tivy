#' Processing length data from hauls, PRODUCE sitrapesca file
#'
#' @description
#' Processes length data from PRODUCE logbooks in CSV or XLSX format.
#' Cleans the relevant columns and transforms the long format to wide format (rows by haul, columns by length).
#'
#' @param data_length Data frame with length data.
#' @param format Input format: "xlsx" (default) or "csv".
#'
#' @return A data frame with length by haul (wide format).
#' @export
#'
#' @examples
#'
#' process_length(data_length = tallas_bitacora, format = "xlsx")
#'
#' @importFrom dplyr select mutate %>%
#' @importFrom stringr str_trim
#' @importFrom tidyr pivot_wider
#' @importFrom stats na.omit
process_length <- function(data_length, format = "xlsx") {
  if (!format %in% c("xlsx", "csv")) {
    stop("The 'format' parameter must be 'xlsx' or 'csv'.")
  }
  # Select columns according to format
  if (format == "xlsx") {
    if (ncol(data_length) < 10) stop("At least 10 columns are expected in XLSX files.")
    data_length <- data_length %>% dplyr::select(3, 4, 5, 8, 10)
  } else if (format == "csv") {
    if (ncol(data_length) < 6) stop("At least 6 columns are expected in CSV files.")
    data_length <- data_length[, -c(1, 6)]
  }
  # Assign names
  names(data_length) <- c("fishing_trip_code", "haul_number", "description", "length", "freq")
  # Cleaning and conversion
  data_length <- data_length %>%
    dplyr::mutate(
      description = stringr::str_trim(.data[["description"]]),
      length = suppressWarnings(as.numeric(.data[["length"]])),
      freq = suppressWarnings(as.numeric(.data[["freq"]]))
    )
  # Save desired length order
  length_order <- sort(unique(stats::na.omit(data_length$length)))
  # Transform to wide format
  data_length <- tidyr::pivot_wider(
    data_length,
    names_from = .data[["length"]],
    values_from = .data[["freq"]],
    values_fill = list(freq = 0)
  )
  # Reorder length columns
  fixed_columns <- c("fishing_trip_code", "haul_number", "description")
  ordered_length_columns <- as.character(length_order)
  data_length <- data_length[, c(fixed_columns, ordered_length_columns)]
  return(as.data.frame(data_length))
}