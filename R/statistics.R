#' Length - Weight Relationship
#'
#' This function estimates the weight of an individual from its length, using
#' the general formula of the length-weight relationship: \eqn{W = a \cdot L^b}, where
#' \eqn{W} is the weight, \eqn{L} the length (length), \eqn{a} and \eqn{b} the specific parameters.
#'
#' @param length A numeric vector containing the lengths of the individuals.
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @return A numeric vector with the estimated weights.
#' @export
#' @examples
#'
#' lengths <- seq(5, 20, by = 0.5)
#' a <- 0.0001
#' b <- 2.984
#'
#' weights <- length_weight(length = lengths, a = a, b = b)
#'
#' print(weights)
length_weight <- function(length, a, b) {
  # Parameter validation
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")

  if (any(length <= 0, na.rm = TRUE)) warning("Length values <= 0 were detected, which could produce invalid results.")
  if (a <= 0) warning("The value of 'a' is <= 0, which could produce biologically implausible results.")

  return(a * length^b)
}



#' Weighting length according to total catch
#'
#' Calculates a weighting of the sampled length based on the total recorded catch.
#' This allows scaling the observed frequency to the total catch.
#'
#' @param frequency A numeric vector with the frequency of length observed in the sampling.
#' @param catch Numeric value with the total catch (in kg or tons, as appropriate).
#' @param length A numeric vector with the length corresponding to the frequencies.
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @param silence_warnings Logical. If TRUE, warning messages are suppressed (default = FALSE).
#' @return A numeric vector with the weighted frequencies.
#' @export
#' @examples
#'
#' frequency <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' length <- seq(5, 20, by = 1)
#'
#' catch <- 1000
#' a <- 0.0001
#' b <- 2.984
#'
#' weighted <- weighting(frequency, catch, length, a, b)
#'
#' print(head(weighted))
weighting <- function(frequency, catch, length, a, b, silence_warnings = FALSE) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(catch)) stop("The 'catch' parameter must be numeric.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")
  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Accumulate warning messages
  warning_messages <- character()

  # Catch validation
  if (is.na(catch) || catch <= 0) {
    warning_messages <- c(warning_messages,
                          "The value of 'catch' is NA or <= 0, catch = 1 will be used.")
    catch <- 1
  }

  # length validation
  if (any(length <= 0, na.rm = TRUE)) {
    warning_messages <- c(warning_messages,
                          "There are length <= 0, invalid results could be produced.")
  }

  # Frequency validation
  frequency[is.na(frequency)] <- 0
  if (sum(frequency, na.rm = TRUE) == 0) {
    warning_messages <- c(warning_messages,
                          "The sum of frequencies is zero. A vector of zeros will be returned.")
    if (!silence_warnings && length(warning_messages) > 0)
      warning(paste(warning_messages, collapse = " | "))
    return(rep(0, length(length)))
  }

  # Weight calculation
  weight <- length_weight(length = length, a = a, b = b) * frequency
  weight_sum <- sum(weight, na.rm = TRUE)
  if (weight_sum == 0) {
    warning_messages <- c(warning_messages,
                          "The sum of weights is zero. A vector of zeros will be returned.")
    if (!silence_warnings && length(warning_messages) > 0)
      warning(paste(warning_messages, collapse = " | "))
    return(rep(0, length(length)))
  }

  # Show accumulated warnings (if they exist)
  if (!silence_warnings && length(warning_messages) > 0) {
    warning(paste(warning_messages, collapse = " | "))
  }

  # Final calculation
  weighted_length <- (catch / weight_sum) * frequency
  return(weighted_length)
}


#' Prepare polygons from coordinate data
#'
#' @description
#' Helper function to process data and prepare polygons using preexisting parallel lines.
#' The east and west edges of the polygon will follow the shape of the lines parallel
#' to the coast at the specified nautical miles, while the north and south edges will be
#' straight lines at the given latitudes.
#'
#' @param data A data frame with coordinates.
#' @param coastline A data frame with the coastline.
#' @param coast_parallels List of data frames with lines parallel to the coast at different distances.
#'        Each element of the list must have a name indicating the distance (e.g. "l5", "l10").
#' @param column_names A vector with column names in the coast_parallels data frames.
#'        Must contain: c("lat", "lon", "dc") where "dc" is the distance to the coast.
#'
#' @return A list of polygons for visualization.
#' @keywords internal
prepare_polygons <- function(data, coastline, coast_parallels = NULL, column_names = c("lat", "lon", "dc")) {
  # Parameter validation
  if (missing(data)) {
    stop("The 'data' parameter is required.")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  if (!is.data.frame(coastline) || !all(c("Long", "Lat") %in% names(coastline))) {
    stop("'coastline' must be a data.frame with columns 'Long' and 'Lat'.")
  }

  # Check if there are coordinates to work with
  if (nrow(data) == 0) {
    stop("The data frame 'data' contains no rows.")
  }

  # Validate coast_parallels if provided
  if (!is.null(coast_parallels)) {
    if (!is.list(coast_parallels)) {
      stop("'coast_parallels' must be a list of data.frames.")
    }

    # Validate that each element of the list is a data.frame with the necessary columns
    for (name in names(coast_parallels)) {
      df <- coast_parallels[[name]]
      if (!is.data.frame(df)) {
        warning("The element '", name, "' in coast_parallels is not a data.frame.")
        next
      }
      if (!all(column_names %in% names(df))) {
        warning("The element '", name, "' in coast_parallels does not have all the required columns: ",
                paste(column_names, collapse = ", "))
      }
    }

    # Check that we have at least one valid element
    valid_elements <- sapply(names(coast_parallels), function(name) {
      df <- coast_parallels[[name]]
      is.data.frame(df) && all(column_names %in% names(df))
    })

    if (!any(valid_elements)) {
      warning("No element in coast_parallels has the correct format. The original approximation method will be used.")
      coast_parallels <- NULL
    }
  }

  # Prepare the data for visualization
  prepared_data <- data

  # Convert text coordinates to numeric if necessary
  if (any(c("StartLatitude", "EndLatitude") %in% names(data))) {
    if (!"lat_ini" %in% names(data)) {
      prepared_data$lat_ini <- Tivy::dms_to_decimal(data$StartLatitude)
    }
    if (!"lat_fin" %in% names(data)) {
      prepared_data$lat_fin <- Tivy::dms_to_decimal(data$EndLatitude)
    }
  }

  if (any(c("StartLongitude", "EndLongitude") %in% names(data))) {
    if (!"lon_ini" %in% names(data)) {
      prepared_data$lon_ini <- Tivy::dms_to_decimal(data$StartLongitude)
    }
    if (!"lon_fin" %in% names(data)) {
      prepared_data$lon_fin <- Tivy::dms_to_decimal(data$EndLongitude)
    }
  }

  # Create list to store polygons
  polygons <- list()

  # Process each row to create polygons
  for (i in 1:nrow(prepared_data)) {
    # Check if we have nautical miles instead of longitudes
    if (!is.na(data$StartNauticalMiles[i]) && !is.na(data$EndNauticalMiles[i]) &&
        (is.na(prepared_data$lon_ini[i]) || is.na(prepared_data$lon_fin[i]))) {

      # Get decimal latitudes and make sure they are in the correct order
      lat_north <- max(prepared_data$lat_ini[i], prepared_data$lat_fin[i])
      lat_south <- min(prepared_data$lat_ini[i], prepared_data$lat_fin[i])

      # Get miles
      miles_ini <- data$StartNauticalMiles[i]  # Closer to the coast
      miles_fin <- data$EndNauticalMiles[i]     # Further from the coast

      # Consider using preexisting coast_parallels
      if (!is.null(coast_parallels)) {
        lat_name <- column_names[1] # Latitude column
        lon_name <- column_names[2] # Longitude column

        # Find parallel lines for the initial and final miles
        line_ini_info <- find_parallel_line(miles_ini, coast_parallels)
        line_fin_info <- find_parallel_line(miles_fin, coast_parallels)

        line_ini <- line_ini_info$df  # Line closest to the coast
        line_fin <- line_fin_info$df  # Line furthest from the coast

        # If we find lines for both miles
        if (!is.null(line_ini) && !is.null(line_fin) &&
            nrow(line_ini) > 0 && nrow(line_fin) > 0) {

          # Interpolate exact points at the corners
          point_ne <- interpolate_point(line_ini, lat_north, lat_name, lon_name)
          point_se <- interpolate_point(line_ini, lat_south, lat_name, lon_name)
          point_nw <- interpolate_point(line_fin, lat_north, lat_name, lon_name)
          point_sw <- interpolate_point(line_fin, lat_south, lat_name, lon_name)

          # Extract points from the west line (furthest) between latitudes
          west_points <- extract_points_between_latitudes(
            line_fin, lat_south, lat_north, lat_name, lon_name
          )

          # Extract points from the east line (closest) between latitudes
          east_points <- extract_points_between_latitudes(
            line_ini, lat_south, lat_north, lat_name, lon_name
          )

          # Create coordinates for the complete polygon
          # 1. Start at the northeast corner and go north along the east edge
          # 2. Then go from north to south along the west edge
          # 3. Return to the starting point to close the polygon

          # Sort west points from north to south
          west_points <- west_points[order(west_points[, 2], decreasing = TRUE), ]

          # Sort east points from south to north
          east_points <- east_points[order(east_points[, 2]), ]

          # Combine all points to form the polygon
          coords <- rbind(
            east_points,          # East edge (south to north)
            west_points,         # West edge (north to south)
            east_points[1, ]      # Close the polygon by returning to the first point
          )

          # Create polygon object
          polygon <- list(
            coords = coords,
            id = i,
            start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
            end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
            file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
            Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
            Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
            End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
            End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
            StartNauticalMiles = miles_ini,
            EndNauticalMiles = miles_fin,
            announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
            actual_mile_ini = line_ini_info$actual_mile,
            actual_mile_fin = line_fin_info$actual_mile,
            method = "detailed"
          )

          polygons[[i]] <- polygon
          next  # Continue to the next iteration
        }
      }

      # If we get here, it's because we couldn't create a detailed polygon with parallel lines
      # Fall back to the original method with approximation
      coast_lon_lat_north <- calculate_coast_longitude(coastline, lat_north)
      coast_lon_lat_south <- calculate_coast_longitude(coastline, lat_south)

      # Conversion factor adjusted by each latitude
      factor_lat_north <- cos(lat_north * pi/180)
      factor_lat_south <- cos(lat_south * pi/180)

      # Convert miles to degrees for each latitude
      offset_ini_lat_north <- miles_ini / 60 / factor_lat_north
      offset_fin_lat_north <- miles_fin / 60 / factor_lat_north
      offset_ini_lat_south <- miles_ini / 60 / factor_lat_south
      offset_fin_lat_south <- miles_fin / 60 / factor_lat_south

      # Create polygon coordinates with 4 corners (original method)
      coords <- rbind(
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north),  # Northeast corner
        c(coast_lon_lat_north - offset_fin_lat_north, lat_north),  # Northwest corner
        c(coast_lon_lat_south - offset_fin_lat_south, lat_south),  # Southwest corner
        c(coast_lon_lat_south - offset_ini_lat_south, lat_south),  # Southeast corner
        c(coast_lon_lat_north - offset_ini_lat_north, lat_north)   # Close the polygon
      )

      # Create polygon object (original method)
      polygon <- list(
        coords = coords,
        id = i,
        start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
        end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
        file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
        Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
        Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
        End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
        End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
        StartNauticalMiles = miles_ini,
        EndNauticalMiles = miles_fin,
        announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
        method = "approximate"
      )

      polygons[[i]] <- polygon

    } else if (!is.na(prepared_data$lat_ini[i]) && !is.na(prepared_data$lat_fin[i]) &&
               !is.na(prepared_data$lon_ini[i]) && !is.na(prepared_data$lon_fin[i])) {

      # Case with explicit coordinates: create rectangular polygon
      coords <- rbind(
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i]),  # Northwest corner
        c(prepared_data$lon_fin[i], prepared_data$lat_ini[i]),  # Northeast corner
        c(prepared_data$lon_fin[i], prepared_data$lat_fin[i]),  # Southeast corner
        c(prepared_data$lon_ini[i], prepared_data$lat_fin[i]),  # Southwest corner
        c(prepared_data$lon_ini[i], prepared_data$lat_ini[i])   # Close the polygon
      )

      # Create polygon object
      polygon <- list(
        coords = coords,
        id = i,
        start_date = if ("StartDateTime" %in% names(data)) data$StartDateTime[i] else NA,
        end_date = if ("EndDateTime" %in% names(data)) data$EndDateTime[i] else NA,
        file_name = if ("file_name" %in% names(data)) data$file_name[i] else NA,
        Start_Long = if ("StartLongitude" %in% names(data)) data$StartLongitude[i] else NA,
        Start_Lat = if ("StartLatitude" %in% names(data)) data$StartLatitude[i] else NA,
        End_Long = if ("EndLongitude" %in% names(data)) data$EndLongitude[i] else NA,
        End_Lat = if ("EndLatitude" %in% names(data)) data$EndLatitude[i] else NA,
        StartNauticalMiles = if ("StartNauticalMiles" %in% names(data)) data$StartNauticalMiles[i] else NA,
        EndNauticalMiles = if ("EndNauticalMiles" %in% names(data)) data$EndNauticalMiles[i] else NA,
        announcement = if ("announcement" %in% names(data)) data$announcement[i] else paste("Polygon", i),
        method = "explicit"
      )

      polygons[[i]] <- polygon
    } else {
      warning("Row ", i, " does not have sufficient data to create a valid polygon.")
    }
  }

  # Filter NA polygons
  polygons <- polygons[!sapply(polygons, is.null)]

  if (length(polygons) == 0) {
    stop("No valid polygons could be created with the provided data.")
  }

  return(polygons)
}



#' Process a block of rows for size weighting
#'
#' Helper function that processes a set of rows to calculate weighted sizes.
#' Can be used directly to process data subsets.
#'
#' @param df_block Data frame to process
#' @param sizes_cols Names of the size columns
#' @param catch_col Name of the catch column
#' @param a Coefficient of the length-weight relationship
#' @param b Exponent of the length-weight relationship
#' @param silence_warnings Whether to suppress warnings
#' @return Data frame with weighted columns added with the prefix "pond_"
#'
#' @keywords internal
process_block <- function(df, sizes_cols, catch_col, a, b, silence_warnings = FALSE) {
  # Get list of sizes
  size_values <- as.numeric(sub(".*_", "", sizes_cols))
  if (all(is.na(size_values))) {
    size_values <- as.numeric(sizes_cols)
  }

  # If there are still NAs, use sequence
  if (any(is.na(size_values))) {
    warning("Could not extract numerical size values, using sequence 1:n")
    size_values <- seq_along(sizes_cols)
  }

  # Counter for warnings
  warning_counter <- list(
    catch_na = 0,
    freq_zero = 0,
    weight_zero = 0,
    sizes_zero = 0
  )

  # Process each row
  result <- df
  for (i in 1:nrow(df)) {
    catch_i <- df[i, catch_col]
    frequencies_i <- as.numeric(df[i, sizes_cols])

    # Count possible warnings
    if (is.na(catch_i) || catch_i <= 0) warning_counter$catch_na <- warning_counter$catch_na + 1
    if (sum(frequencies_i, na.rm = TRUE) == 0) warning_counter$freq_zero <- warning_counter$freq_zero + 1
    if (any(size_values <= 0, na.rm = TRUE)) warning_counter$sizes_zero <- warning_counter$sizes_zero + 1

    # Calculate weighting (silencing individual warnings)
    weighted <- Tivy::weighting(
      frequency = frequencies_i,
      catch = catch_i,
      sizes = size_values,
      a = a,
      b = b,
      silence_warnings = TRUE
    )

    # Add results
    for (j in seq_along(sizes_cols)) {
      result[i, paste0("pond_", size_values[j])] <- weighted[j]
    }
  }

  # Show warning summary at the end
  if (silence_warnings == FALSE) {
    warnings_text <- character(0)
    if (warning_counter$catch_na > 0)
      warnings_text <- c(warnings_text,
                         paste0(warning_counter$catch_na, " rows with catch NA or <= 0"))
    if (warning_counter$freq_zero > 0)
      warnings_text <- c(warnings_text,
                         paste0(warning_counter$freq_zero, " rows with sum of frequencies = 0"))
    if (warning_counter$sizes_zero > 0)
      warnings_text <- c(warnings_text,
                         paste0("There are sizes <= 0 that could produce invalid results"))

    if (length(warnings_text) > 0) {
      warning("Summary of problems found: ", paste(warnings_text, collapse = "; "))
    }
  }

  return(result)
}


#' Percentage of juveniles
#'
#' Estimates the percentage of individuals considered juveniles in a sample
#' according to an established length limit.
#'
#' @param frequency A numeric vector with the frequency of sampled length.
#' @param length Vector of length corresponding to the frequencies.
#' @param juvLim length limit (default 12 cm) to classify as juvenile.
#' @param silence_warnings Logical. If TRUE, warning messages are suppressed (default = FALSE).
#' @return Percentage of juveniles in the sample.
#' @export
#' @examples
#'
#' frequency <- c(0, 1, 4, 8, 16, 12, 23, 34, 55, 35, 24, 15, 10, 6, 3, 2)
#'
#' length <- seq(5, 20, by = 1)
#'
#' perc <- juvenile_percentage(frequency, length, juvLim = 12)
#'
#' print(perc)
juvenile_percentage <- function(frequency, length, juvLim = 12, silence_warnings = FALSE) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")
  if (!is.numeric(juvLim)) stop("The 'juvLim' parameter must be numeric.")
  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }
  if (juvLim <= 0 && !silence_warnings)
    warning("The value of 'juvLim' is <= 0, which may not be biologically plausible.")

  total_frequency <- sum(frequency, na.rm = TRUE)
  if (total_frequency == 0) {
    if (!silence_warnings) warning("The sum of frequencies is zero. NA will be returned.")
    return(NA_real_)
  }

  juv <- 100 * (sum(frequency[length < juvLim], na.rm = TRUE) / total_frequency)
  return(juv)
}



#' Minimum observed length with positive frequency
#'
#' @param frequency A numeric vector with the frequencies of length.
#' @param length Corresponding length vector.
#' @return Minimum length value with frequency greater than zero.
#' @export
#' @examples
#' min_range(frequency = c(0,0,1,2,3), length = c(5,6,7,8,9))
min_range <- function(frequency, length) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")

  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Check if there are positive frequencies
  if (all(frequency <= 0, na.rm = TRUE) || all(is.na(frequency))) {
    warning("No positive frequencies. NA will be returned.")
    return(NA_real_)
  }

  frequency[frequency <= 0] <- NA
  return(min(length[!is.na(frequency)], na.rm = TRUE))
}



#' Maximum observed length with positive frequency
#'
#' @param frequency A numeric vector with the frequencies of length.
#' @param length Corresponding length vector.
#' @return Maximum length value with frequency greater than zero.
#' @export
#' @examples
#' max_range(frequency = c(0,0,1,2,3), length = c(5,6,7,8,9))
max_range <- function(frequency, length) {
  # Parameter validation
  if (!is.numeric(frequency)) stop("The 'frequency' parameter must be numeric.")
  if (!is.numeric(length)) stop("The 'length' parameter must be numeric.")

  if (length(frequency) != length(length)) {
    stop("The 'frequency' and 'length' vectors must have the same length.")
  }

  # Check if there are positive frequencies
  if (all(frequency <= 0, na.rm = TRUE) || all(is.na(frequency))) {
    warning("No positive frequencies. NA will be returned.")
    return(NA_real_)
  }

  frequency[frequency <= 0] <- NA
  return(max(length[!is.na(frequency)], na.rm = TRUE))
}



#' Conversion from number of individuals to weight
#'
#' Converts numerical length frequencies in a data.frame to weight estimates,
#' using the length-weight relationship.
#'
#' @param data Data frame where columns with names equal to length contain frequencies.
#' @param length Vector of length (which must match the column names of `data`).
#' @param a Numeric value of the coefficient of the length-weight relationship.
#' @param b Numeric value of the exponent of the length-weight relationship.
#' @return Data frame with the same dimensions but expressed in weight.
#' @export
#' @importFrom stats setNames
#' @examples
#'
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' # Process data
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(x = data_fishing_trips, y = hauls_length, by = 'fishing_trip_code')
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls,
#'                                        data_length_fishing_trips = data_length_fishing_trips)
#' final_data <- add_variables(data_total)
#'
#' # Define length columns
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Weight length
#' results <- weight_length_df(df = final_data,
#'                                length_cols = length_cols,
#'                                catch_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#'number_to_weight(data = results, length = paste0('pond_', length_cols), a = 0.0012, b = 3.1242)
number_to_weight <- function(data, length, a, b) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!is.numeric(a)) stop("The 'a' parameter must be numeric.")
  if (!is.numeric(b)) stop("The 'b' parameter must be numeric.")

  if (is.character(length)) {
    length_num <- extract_length_values(length)
  }

  # Validation that length are present in the data.frame
  length_present <- length %in% colnames(data)
  if (!all(length_present)) {
    missing_length <- length[!length_present]
    stop("The following length are not present as columns in the data.frame: ",
         paste(missing_length, collapse = ", "))
  }

  # Check that length columns contain numeric values
  for (length_col in length) {
    if (!is.numeric(data[[length_col]])) {
      data[[length_col]] <- as.numeric(data[[length_col]])
      warning("Column '", length_col, "' has been converted to numeric.")
    }
  }

  # Weight calculation
  tryCatch({
    weights <- as.data.frame(t(apply(data[, length, drop = FALSE], 1, function(x) {
      length_weight(length = length_num, a = a, b = b) * x
    })))

    # Rename columns with prefix "weight_"
    colnames(weights) <- paste0("weight_", length)

    # Add weight columns to the original data
    result <- cbind(data, weights)

    return(result)
  }, error = function(e) {
    stop("Error calculating weights: ", e$message)
  })
}





#' Calculation of juvenile percentage by groups
#'
#' @description
#' Calculates the percentage of juveniles by specified groups, both in number and
#' in weight. Uses a modern approach with dplyr to process the data and calculate
#' the proportions of juveniles based on length frequencies.
#'
#' @param data Data frame with length frequency data.
#' @param group_cols Vector of column names to group the data.
#' @param cols_length Vector of column names or indices that contain the
#'   length frequencies. Can be names with patterns like "pond_X", "length_X" or "X".
#' @param juvLim length limit to consider juveniles (default 12 cm).
#' @param a Coefficient of the length-weight relationship (default 0.0012).
#' @param b Exponent of the length-weight relationship (default 3.1242).
#' @param remove_empty Logical. If TRUE (default), removes groups
#'   without data (total_number = 0).
#'
#' @return Data frame with the following fields:
#'   \itemize{
#'     \item Grouping columns specified in group_cols
#'     \item perc_juv_number: Percentage of juveniles in number
#'     \item perc_juv_weight: Percentage of juveniles in weight
#'     \item total_number: Total number of individuals in the group
#'     \item total_weight: Total weight in the group
#'   }
#'
#' @export
#' @importFrom dplyr group_by_at summarize across everything filter pick
#' @importFrom tidyr unnest
#'
#' @examples
#' # Load sample data
#' data(calas_bitacora, faenas_bitacora, tallas_bitacora)
#'
#' # Process data
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#' data_fishing_trips <- process_fishing_trips(data_fishing_trips = faenas_bitacora)
#' hauls_length <- process_length(data_length = tallas_bitacora)
#'
#' # Integrate data
#' data_length_fishing_trips <- merge(x = data_fishing_trips, y = hauls_length, by = 'fishing_trip_code')
#' data_total <- merge_length_fishing_trips_hauls(data_hauls = data_hauls,
#'                                        data_length_fishing_trips = data_length_fishing_trips)
#' final_data <- add_variables(data_total)
#'
#' # Define length columns
#' length_cols <- c("8", "8.5", "9", "9.5", "10", "10.5", "11", "11.5",
#'                  "12", "12.5", "13", "13.5", "14", "14.5", "15")
#'
#' # Weight length
#' results <- weight_length_df(df = final_data,
#'                                length_cols = length_cols,
#'                                catch_col = "catch_ANCHOVETA",
#'                                a = 0.0001,
#'                                b = 2.984)
#'
#' # Add date column for grouping
#' results$unique_date <- convert_to_date(results$start_date_haul, type = "date")
#'
#' # Calculate juveniles by date
#' results_by_date <- juveniles_by_group(
#'   data = results,
#'   group_cols = "unique_date",
#'   cols_length = paste0("pond_", length_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' # Calculate juveniles by date and distance to coast
#' results_date_dc <- juveniles_by_group(
#'   data = results,
#'   group_cols = c("unique_date", "dc_cat"),
#'   cols_length = paste0("pond_", length_cols),
#'   juvLim = 12,
#'   a = 0.0012,
#'   b = 3.1242
#' )
#'
#' # View results
#' head(results_by_date)
juveniles_by_group <- function(data, group_cols, cols_length, juvLim = 12, a = 0.0012, b = 3.1242,
                                remove_empty = TRUE) {
  # Parameter validation
  if (!is.data.frame(data)) stop("The 'data' parameter must be a data.frame.")
  if (!all(group_cols %in% colnames(data)))
    stop("Not all grouping columns are in the data.frame.")

  # Determine if cols_length contains names or indices
  if (is.numeric(cols_length)) {
    # If they are numeric indices, get the corresponding names
    if (any(cols_length > ncol(data) | cols_length < 1))
      stop("Some of the indices in cols_length are outside the range of the data.frame.")

    # Convert indices to names to work uniformly
    cols_names <- names(data)[cols_length]
  } else {
    # If they are already names, check that they exist in the data.frame
    if (!all(cols_length %in% colnames(data)))
      stop("Not all length columns are in the data.frame.")

    cols_names <- cols_length
  }

  # Ensure that length columns are numeric
  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(cols_names), ~as.numeric(.x)))

  # Extract numerical length values from column names
  # Only if the names seem to contain length information (e.g., "pond_8.5", "8", "length_9")
  if (all(grepl("^(pond_)?([0-9]+(\\.[0-9]+)?)$|^length_[0-9]+(\\.[0-9]+)?$", cols_names))) {
    # Extract numerical values removing common prefixes
    length_values <- as.numeric(gsub("^(pond_|length_)?", "", cols_names))
  } else if (is.numeric(cols_length)) {
    # If cols_length were originally numeric and don't appear to be length patterns,
    # use the original values
    length_values <- cols_length
  } else {
    # Otherwise, try to convert the names directly to numeric
    length_values <- suppressWarnings(as.numeric(cols_names))

    # If the conversion doesn't work (generating NAs), use sequential numbers
    if (anyNA(length_values)) {
      warning("Could not determine numerical length values from column names. Using sequence 1:n.")
      length_values <- seq_along(cols_names)
    }
  }

  # Function to process each group using calculate_juveniles
  process_group <- function(df) {
    # Check if the dataframe is empty or all frequencies are zero
    if (nrow(df) == 0) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0
      ))
    }

    # Extract and sum frequencies by length
    frequencies <- colSums(df[, cols_names, drop = FALSE], na.rm = TRUE)

    # Check if all frequencies are zero
    if (all(frequencies == 0)) {
      return(data.frame(
        perc_juv_number = NA_real_,
        perc_juv_weight = NA_real_,
        total_number = 0,
        total_weight = 0
      ))
    }

    # Call the external function to calculate juveniles
    calculate_juveniles(frequencies, length_values, juvLim, a, b)
  }

  # If there are no grouping columns, calculate for the entire set
  if (length(group_cols) == 0) {
    return(process_group(data))
  }

  # Group and calculate - updated version without using cur_data()
  results <- data %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::reframe(
      result = list(
        {
          # Use pick() to select the length columns
          df_group <- dplyr::pick(all_of(cols_names))
          # Add rows to complete the data.frame
          df_group <- as.data.frame(df_group)
          process_group(df_group)
        }
      )
    ) %>%
    tidyr::unnest(result)

  # Optionally remove groups without data
  if (remove_empty && any(results$total_number == 0, na.rm = TRUE)) {
    results <- results %>%
      dplyr::filter(total_number > 0)
  }

  return(results)
}