#' Add variables for juveniles, sample length, distance to coast, and distance category
#'
#' @description
#' Adds new variables to a dataset, including the proportion of juveniles,
#' the total number of individuals in the sample, the distance to the coast, and the distance category.
#'
#' @param data Data frame that must contain latitude (`lat_initial`) and longitude (`lon_initial`) coordinates,
#'   as well as columns with individual length.
#' @param JuvLim Length threshold to consider juveniles.
#' @param distance_type Type of distance calculation to the coast.
#' @param window Window parameter to smooth the coastline.
#' @param unit Distance unit used in the calculation ("nm", "km", etc.).
#' @param coastline Data frame with coastline coordinates. Must have columns named `Long` and `Lat`.
#'   If `NULL`, uses internal dataset `peru_coastline`.
#' @param suppress_warnings Logical. If TRUE, warnings are suppressed.
#'
#' @return Data frame with new variables: `juv` (proportion of juveniles), `sample` (total individuals),
#'   `dc` (distance to coast), and `dc_cat` (categorical distance).
#'
#' @examples
#' \dontrun{
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' data_length_trips <- merge(
#'   x = data_fishing_trips, 
#'   y = hauls_length, 
#'   by = 'fishing_trip_code'
#' )
#' data_total <- merge_length_fishing_trips_hauls(
#'   data_hauls = data_hauls, 
#'   data_length_fishing_trips = data_length_trips
#' )
#'
#' results <- add_variables(data = data_total)
#' }
#'
#' @export
#' @importFrom dplyr mutate case_when
add_variables <- function(data,
                          JuvLim = 12,
                          distance_type = "haversine",
                          window = 0.5,
                          unit = "nm",
                          coastline = NULL,
                          suppress_warnings = TRUE) {

  if (is.null(coastline)) {
    coastline <- peru_coastline
  }

  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")

  stopifnot(is.data.frame(data))
  required_cols <- c("lon_initial", "lat_initial")

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  length <- grep(pattern = "^[1-9][0-9]*$|^[1-9][0-9]*\\.[0-9]+$",
                x = names(data),
                value = TRUE)

  if (length(length) == 0) {
    warning("No length columns with numeric names found. Juvenile proportion will not be calculated.")
    return(data)
  }

  data[length] <- lapply(data[length], as.numeric)

  zero_frequency_count <- 0

  data$juv <- apply(data[, length, drop = FALSE], 1, function(row) {
    if (sum(row, na.rm = TRUE) == 0) {
      zero_frequency_count <<- zero_frequency_count + 1
      return(NA_real_)
    }

    calculate_juvenile_percentage(
      frequency = row,
      length = as.numeric(length),
      juvenile_limit = JuvLim,
      silence_warnings = TRUE
    )
  })

  if (!suppress_warnings && zero_frequency_count > 0) {
    warning("Found ", zero_frequency_count,
            " rows with zero frequency. 'juv' was set to NA.")
  }

  data$sample <- rowSums(data[, length], na.rm = TRUE)

  distance_warning_shown <- FALSE

  data$dc <- tryCatch(
    coast_distance(
      lon = data$lon_initial,
      lat = data$lat_initial,
      coastline = coastline,
      distance_type = distance_type,
      window = window,
      unit = unit
    ),
    error = function(e) {
      if (!suppress_warnings && !distance_warning_shown) {
        warning("Error calculating distance to coast: ", conditionMessage(e))
        distance_warning_shown <<- TRUE
      }
      return(rep(NA_real_, nrow(data)))
    }
  )

  data <- data %>%
    dplyr::mutate(
      dc_cat = dplyr::case_when(
        !is.na(dc) & dc >= 5  & dc < 15  ~ "05-15 nm",
        !is.na(dc) & dc >= 15 & dc < 30  ~ "15-30 nm",
        !is.na(dc) & dc >= 30 & dc < 50  ~ "30-50 nm",
        !is.na(dc) & dc >= 50 & dc < 100 ~ "50-100 nm",
        TRUE                             ~ NA_character_
      )
    )

  return(data)
}