#' Processing fishing trip data from PRODUCE sitrapesca files
#'
#' @description
#' Processes fishing trip data from PRODUCE logbooks,
#' in CSV or XLSX format. Returns a clean and standardized data frame.
#'
#' @param data_fishing_trips Data frame with raw fishing trip data.
#' @param format Format of the input file: "xlsx" (default) or "csv".
#'
#' @return A data frame with the following standardized columns:
#'   \itemize{
#'     \item fishing_trip_code: Unique identifier of the fishing trip
#'     \item vessel: Vessel name
#'     \item owner: Vessel owner
#'     \item id_vessel: Vessel registration number
#'     \item start_date: Start date and time of the fishing trip (optional, only in XLSX format)
#'     \item end_date: End date and time of the fishing trip (optional, only in XLSX format)
#'   }
#'
#' @export
#' @importFrom dplyr select %>%
#'
#' @examples
#'
#' data(faenas_bitacora)
#'
#' fishing_trips = process_fishing_trips(data_fishing_trips = faenas_bitacora, format = "xlsx")
#'
#' print(head(fishing_trips))
process_fishing_trips <- function(data_fishing_trips, format = "xlsx") {
  if (!format %in% c("xlsx", "csv")) {
    stop("The 'format' parameter must be 'xlsx' or 'csv'.")
  }
  if (format == "xlsx") {
    if (ncol(data_fishing_trips) < 11) stop("At least 11 columns are expected in XLSX files.")
    data_fishing_trips <- data_fishing_trips %>% dplyr::select(11, 4, 3, 7, 9, 10)
    names(data_fishing_trips) <- c("fishing_trip_code", "vessel", "owner", "id_vessel", "start_date", "end_date")
  } else if (format == "csv") {
    if (ncol(data_fishing_trips) < 8) stop("At least 8 columns are expected in CSV files.")
    data_fishing_trips <- data_fishing_trips %>% dplyr::select(8, 3, 2, 4, 6, 7)
    names(data_fishing_trips) <- c("fishing_trip_code", "vessel", "owner", "id_vessel", "start_date", "end_date")
  }
  return(data_fishing_trips)
}
