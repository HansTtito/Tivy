#' Adds variables for juveniles, sample length, distance to coast, and distance category
#'
#' This function adds new variables to a dataset, including the proportion of juveniles,
#' the total number of individuals in the sample, the distance to the coast, and the distance category.
#'
#' @param data A data frame that must contain latitude (`lat_initial`) and longitude (`lon_initial`) coordinates,
#' as well as columns with individual length.
#' @param JuvLim Length threshold to consider juveniles (default = 12). If the length is below this value,
#' the individual is considered juvenile.
#' @param distance_type Type of distance calculation to the coast (e.g., "haversine"), default is "haversine".
#' @param window Window parameter to smooth the coastline, default is 0.5.
#' @param unit Distance unit used in the calculation ("nm", "km", etc.), default is "nm".
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param suppress_warnings Logical. If TRUE (default), warnings are suppressed; otherwise, they are shown.
#'
#' @return A data frame with the following new variables:
#' \itemize{
#'   \item `juv`: Proportion of juveniles in each row.
#'   \item `sample`: Total number of individuals in the sample.
#'   \item `dc`: Distance to the coast based on provided latitude and longitude.
#'   \item `dc_cat`: Categorical variable for distance to the coast (e.g., "05-15 nm", "15-30 nm").
#' }
#' @examples
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' # Process data
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Merge hauls, length and fishing trips
#' data_length_trips <- merge(
#'  x = data_fishing_trips, 
#'  y = hauls_length, 
#'  by = 'fishing_trip_code'
#' )
#' data_total <- merge_length_fishing_trips_hauls(
#'  data_hauls = data_hauls, 
#'  data_length_fishing_trips = data_length_trips
#' )
#'
#' # Apply function
#' results <- add_variables(data = data_total)
#'
#' print(results)
#' @export
#' @importFrom dplyr mutate case_when %>%
add_variables <- function(data,
                          JuvLim = 12,
                          distance_type = "haversine",
                          window = 0.5,
                          unit = "nm",
                          coastline = NULL,
                          suppress_warnings = TRUE) {


    # Load default coastline if NULL
  if (is.null(coastline)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default coastline data (Tivy::peru_coastline) is not available. Please provide a coastline.")
    }
    coastline <- Tivy::peru_coastline
  }

  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")

  stopifnot(is.data.frame(data))
  required_cols <- c("lon_initial", "lat_initial")

  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Identify length columns
  length <- grep(pattern = "^[1-9][0-9]*$|^[1-9][0-9]*\\.[0-9]+$",
                x = names(data),
                value = TRUE)

  # Warn if no length columns found
  if (length(length) == 0) {
    warning("No length columns with numeric names found. Juvenile proportion will not be calculated.")
    return(data)
  }

  # Ensure length columns are numeric
  data[length] <- lapply(data[length], as.numeric)

  zero_frequency_count <- 0

  # Apply proportion of juveniles
  data$juv <- apply(data[, length, drop = FALSE], 1, function(row) {
    if (sum(row, na.rm = TRUE) == 0) {
      zero_frequency_count <<- zero_frequency_count + 1
      return(NA_real_)
    }

    juvenile_percentage(
      frequency = row,
      length = as.numeric(length),
      juvLim = JuvLim,
      silence_warnings = TRUE
    )
  })

  # Show warning summary if needed
  if (!suppress_warnings && zero_frequency_count > 0) {
    warning("Found ", zero_frequency_count,
            " rows with zero frequency. 'juv' was set to NA.")
  }

  # Sample length
  data$sample <- rowSums(data[, length], na.rm = TRUE)

  distance_warning_shown <- FALSE

  # Distance to coast
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

  # Distance categories
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