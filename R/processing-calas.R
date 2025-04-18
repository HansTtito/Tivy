#' Processing fishing haul data from PRODUCE sitrapesca files
#'
#' @description
#' Processes fishing haul data from PRODUCE logbooks,
#' in CSV or XLSX format. Returns a clean and standardized data frame with
#' coordinates converted to decimal degrees.
#'
#' @param data_hauls Data frame with raw haul data.
#' @param format Format of the input file: "xlsx" (default) or "csv".
#' @param correct_coordinates Logical. If TRUE (default), possible
#'   errors in coordinates are corrected during the conversion from degrees, minutes, and
#'   seconds to decimal degrees.
#'
#' @return A data frame with the following standardized columns:
#'   \itemize{
#'     \item fishing_trip_code: Unique identifier of the fishing trip
#'     \item haul_number: Haul number within the fishing trip
#'     \item start_date: Start date and time of the haul
#'     \item end_date: End date and time of the haul
#'     \item start_latitude, start_longitude: Initial coordinates in original format
#'     \item end_latitude, end_longitude: Final coordinates in original format
#'     \item gear_type: Type of fishing gear used
#'     \item description: Additional description of the haul
#'     \item catch: Total recorded catch
#'     \item status: Status of the haul
#'     \item haul_origin: Source of the haul data
#'     \item registration_date: Date of registration in the system
#'     \item lat_initial, lon_initial: Initial coordinates in decimal degrees
#'     \item lat_final, lon_final: Final coordinates in decimal degrees
#'   }
#' @export
#' @importFrom dplyr select mutate %>%
#' @importFrom stringi stri_trim
#'
#' @examples
#' # Load example data
#'
#' data(calas_bitacora)
#'
#' # Process data with default configuration (correcting coordinates)
#' processed_hauls <- process_hauls(
#'   data_hauls = calas_bitacora,
#'   format = "xlsx"
#' )
#'
#' print(head(processed_hauls))
#'
#' # Process data without correcting possible errors in coordinates
#' hauls_without_correction <- process_hauls(
#'   data_hauls = calas_bitacora,
#'   format = "xlsx",
#'   correct_coordinates = FALSE
#' )
#'
#' # View the first rows of the result
#' print(head(hauls_without_correction))
process_hauls <- function(data_hauls, format = "xlsx", correct_coordinates = TRUE) {
  if (!format %in% c("xlsx", "csv")) {
    stop("The 'format' parameter must be 'xlsx' or 'csv'.")
  }
  if (format == "xlsx") {
    if (ncol(data_hauls) < 18) stop("At least 18 columns are expected in XLSX files.")
    data_hauls <- data_hauls %>%
      dplyr::select(3, 4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
  } else if (format == "csv") {
    data_hauls <- data_hauls[, -1]  # Remove extra first column
  }

  names(data_hauls) <- c(
    "fishing_trip_code", "haul_number", "start_date", "end_date",
    "start_latitude", "start_longitude", "end_latitude", "end_longitude",
    "gear_type", "description", "catch", "status", "haul_origin", "registration_date"
  )

  data_hauls <- data_hauls %>%
    dplyr::mutate(
      description = stringi::stri_trim(.data[["description"]]),
      lat_initial = dms_to_decimal(.data[["start_latitude"]], correct_errors = correct_coordinates),
      lon_initial = dms_to_decimal(.data[["start_longitude"]], hemisphere = "W", correct_errors = correct_coordinates),
      lat_final = dms_to_decimal(.data[["end_latitude"]], correct_errors = correct_coordinates),
      lon_final = dms_to_decimal(.data[["end_longitude"]], hemisphere = "W", correct_errors = correct_coordinates)
    )
  return(data_hauls)
}