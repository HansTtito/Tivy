#' Convert latitude or longitude to decimal degrees
#'
#' @description
#' Converts coordinates expressed in degrees, minutes and seconds (DMS) or degrees and minutes (DM) format to decimal degrees.
#' By default, coordinates are assumed to be in the southern hemisphere (negative latitudes).
#' The function can automatically correct common errors such as minutes or seconds greater than 60.
#'
#' @param coordinates Character vector. Each element should be in formats such as:
#'   - With symbols: `"D° M' S\""`, `"D° M'"`, `"17°26'S"`
#'   - Without symbols: `"D M S"`, `"D M"`, `"17 26 S"`
#'   - The hemisphere can be included in the coordinate
#' @param hemisphere Character `"N"`, `"S"`, `"E"`, `"W"` or `"O"` indicating the corresponding hemisphere
#' when not specified in the coordinate. `"S"` and `"W"`/`"O"` generate negative values. Default: `"S"`.
#' @param correct_errors Logical. If TRUE (default value), the function automatically corrects
#' out-of-range values, such as minutes or seconds greater than 60, by appropriately converting them
#' to the higher unit.
#'
#' @return A numeric vector with coordinates converted to decimal degrees.
#' @export
#'
#' @examples
#' # Convert southern coordinates (complete format)
#' dms_to_decimal(c("39° 48' 36\""), hemisphere = "S")
#'
#' # Convert western coordinates (complete format without symbols)
#' dms_to_decimal(c("73 15 0"), hemisphere = "W")
#'
#' # Convert coordinates with only degrees and minutes
#' dms_to_decimal(c("39° 48'"), hemisphere = "S")
#' dms_to_decimal(c("73 15"), hemisphere = "W")
#'
#' # Coordinates with hemisphere included
#' dms_to_decimal(c("17°26'S"))
#' dms_to_decimal(c("73°15'W"))
#' dms_to_decimal(c("39 48 N"))
#'
#' # Automatically correct out-of-range values
#' dms_to_decimal(c("39° 75' 36\""), correct_errors = TRUE)  # Minutes > 60
#' dms_to_decimal(c("39° 48' 98\""), correct_errors = TRUE)  # Seconds > 60
#'
#' # Disable automatic correction
#' dms_to_decimal(c("39° 75' 36\""), correct_errors = FALSE)  # Will generate a warning
#'
#' # In a dataframe
#'
#' data(calas_bitacora)
#'
#' dms_to_decimal(calas_bitacora$Longitud.Fin)
#'
#' @importFrom stringr str_split str_count str_detect str_extract
dms_to_decimal <- function(coordinates, hemisphere = "S", correct_errors = TRUE) {
  # Input validation
  if (missing(coordinates)) {
    stop("The 'coordinates' parameter is required.")
  }

  if (length(coordinates) == 0) {
    warning("The coordinate vector is empty.")
    return(numeric(0))
  }

  # Convert to character if factor
  if (is.factor(coordinates)) {
    coordinates <- as.character(coordinates)
    warning("The coordinate vector has been converted from factor to character.")
  }

  if (!is.character(coordinates)) {
    if (is.numeric(coordinates)) {
      warning("The provided coordinates are already numeric. They are returned unchanged.")
      return(coordinates)
    } else {
      stop("Coordinates must be a text string or a character vector.")
    }
  }

  if (!hemisphere %in% c("N", "S", "E", "W", "O")) {
    stop("The hemisphere must be one of: 'N', 'S', 'E', 'W' or 'O'.")
  }

  # Process each coordinate
  results <- vapply(coordinates, function(coord) {
    # Handle NA
    if (is.na(coord) || coord == "") {
      return(NA_real_)
    }

    tryCatch({
      # Detect if the coordinate includes the hemisphere
      local_hemisphere <- hemisphere
      original_coord <- coord

      # Search for hemisphere indicators (N, S, E, W, O)
      hemisphere_pattern <- "[NSEW]|O"
      found_hemisphere <- regmatches(coord, regexpr(hemisphere_pattern, coord))

      if (length(found_hemisphere) > 0 && found_hemisphere != "") {
        local_hemisphere <- found_hemisphere
        # Remove the hemisphere from the coordinate
        coord <- gsub(hemisphere_pattern, "", coord)
      }

      # Determine the sign according to the hemisphere
      sign <- ifelse(local_hemisphere %in% c("S", "W", "O"), -1, 1)

      # Clean and normalize the coordinate
      # Replace degrees, minutes and seconds with spaces
      clean_coord <- gsub("[°'\"]", " ", coord)
      # Remove multiple spaces
      clean_coord <- gsub("\\s+", " ", clean_coord)
      # Remove spaces at beginning and end
      clean_coord <- trimws(clean_coord)

      # Split into components
      components <- unlist(strsplit(clean_coord, " "))
      # Filter non-numeric components
      num_components <- components[grepl("^[0-9]+(\\.[0-9]+)?$", components)]

      # Check if there are numeric components
      if (length(num_components) == 0) {
        warning(paste("No numeric components found in the coordinate:", original_coord))
        return(NA_real_)
      }

      # Convert to numeric with validation
      parts <- suppressWarnings(as.numeric(num_components))

      # Check if any value is NA after conversion
      if (any(is.na(parts))) {
        warning(paste("Error converting some component of the coordinate to numeric:", original_coord))
        return(NA_real_)
      }

      # NEW: Automatically correct out-of-range values
      if (correct_errors && length(parts) >= 3) {
        # Correct seconds >= 60
        if (!is.na(parts[3]) && parts[3] >= 60) {
          extra_minutes <- floor(parts[3] / 60)
          parts[3] <- parts[3] %% 60
          parts[2] <- parts[2] + extra_minutes
        }

        # Correct minutes >= 60
        if (!is.na(parts[2]) && parts[2] >= 60) {
          extra_degrees <- floor(parts[2] / 60)
          parts[2] <- parts[2] %% 60
          parts[1] <- parts[1] + extra_degrees
        }
      } else {
        # Issue warnings but don't correct
        if (length(parts) >= 1 && (is.na(parts[1]) || parts[1] < 0 || parts[1] > 180)) {
          warning(paste("Degrees out of range (0-180) in the coordinate:", original_coord))
        }

        if (length(parts) >= 2 && (is.na(parts[2]) || parts[2] < 0 || parts[2] >= 60)) {
          warning(paste("Minutes out of range (0-59) in the coordinate:", original_coord))
        }

        if (length(parts) >= 3 && (is.na(parts[3]) || parts[3] < 0 || parts[3] >= 60)) {
          warning(paste("Seconds out of range (0-59) in the coordinate:", original_coord))
        }
      }

      # Calculate according to the number of components
      if (length(parts) == 3) {
        # Complete format: degrees, minutes, seconds
        decimal <- sign * (parts[1] + parts[2] / 60 + parts[3] / 3600)
      } else if (length(parts) == 2) {
        # Partial format: only degrees and minutes
        decimal <- sign * (parts[1] + parts[2] / 60)
      } else if (length(parts) == 1) {
        # Only degrees
        decimal <- sign * parts[1]
      } else {
        warning(paste("Unrecognized format for coordinate:", original_coord))
        return(NA_real_)
      }

      # Validate the final result
      if (abs(decimal) > 180) {
        warning(paste("The calculated decimal coordinate is out of range (-180 to 180):", decimal))
      }

      return(decimal)
    }, error = function(e) {
      warning(paste("Error processing coordinate", coord, ":", e$message))
      return(NA_real_)
    })
  }, FUN.VALUE = numeric(1))

  # Ensure the result has no names
  names(results) <- NULL

  return(results)
}


#' Vectorized distance to coast
#'
#' @description
#' Estimates the distance between a set of points (lon, lat) and a coastline defined by coordinates.
#' It can be executed sequentially or in parallel, and also return the indices of the nearest coastal points.
#'
#' @param lon Numeric vector with the longitudes of the points of interest.
#' @param lat Numeric vector with the latitudes of the points of interest.
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param return_indices Logical. If `TRUE`, also returns the indices of the nearest coastline points. Default `FALSE`.
#' @param distance_type Type of geographic distance to use: `"haversine"`, `"euclidean"`, `"grid"`.
#' @param unit Unit of measurement for distance: `"nm"` (nautical miles), `"km"`, etc.
#' @param window Search window in degrees around the point to limit calculations and improve efficiency. Default `1`.
#' @param parallel Logical. If `TRUE`, performs the calculation in parallel using multiple cores. Default `FALSE`.
#' @param cores Number of cores to use for parallel processing. Default `4`.
#'
#' @return If `return_indices = FALSE`, returns a numeric vector with distances to the coast for each point.
#'         If `return_indices = TRUE`, returns a list with:
#'         \itemize{
#'           \item \code{distance}: numeric vector with distances to the coast
#'           \item \code{index}: vector of indices of the nearest point on the coastline
#'         }
#'
#' @export
#' @examples
#'
#' data(calas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#'
#' coast_distance(
#'   lon = data_hauls$lon_final,
#'   lat = data_hauls$lat_final,
#'   coastline = NULL,
#'   distance_type = "haversine",
#'   unit = "nm",
#'   parallel = TRUE,
#'   cores = 2
#' )
#'
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
coast_distance <- function(lon,
                            lat,
                            coastline = NULL,
                            return_indices = FALSE,
                            distance_type = "haversine",
                            unit = "nm",
                            window = 1,
                            parallel = FALSE,
                            cores = 4) {
  # Parameter validation
  if (missing(lon) || missing(lat)) {
    stop("The parameters 'lon', 'lat', and 'coastline' are required.")
  }

  # Default coastline if NULL
  if (is.null(coastline)) {
    if (requireNamespace("Tivy", quietly = TRUE)) {
      coastline <- Tivy::peru_coastline
    } else {
      stop("Package 'Tivy' is required to use the default coastline. Please install it or provide a coastline manually.")
    }
  }

  # Validate data types
  if (!is.numeric(lon)) stop("The 'lon' parameter must be numeric.")
  if (!is.numeric(lat)) stop("The 'lat' parameter must be numeric.")
  if (!is.data.frame(coastline)) stop("The 'coastline' parameter must be a data.frame.")
  if (!is.logical(return_indices)) stop("The 'return_indices' parameter must be logical (TRUE/FALSE).")
  if (!is.character(distance_type)) stop("The 'distance_type' parameter must be text.")
  if (!is.character(unit)) stop("The 'unit' parameter must be text.")
  if (!is.numeric(window)) stop("The 'window' parameter must be numeric.")
  if (!is.logical(parallel)) stop("The 'parallel' parameter must be logical (TRUE/FALSE).")
  if (!is.numeric(cores) || cores < 1) stop("The 'cores' parameter must be a positive integer.")

  # Validate vector lengths
  if (length(lon) != length(lat)) {
    stop("The 'lon' and 'lat' vectors must have the same length.")
  }

  # Validate coordinate ranges
  if (any(abs(lon) > 180, na.rm = TRUE)) {
    warning("Longitude values outside the valid range (-180 to 180) were detected.")
  }
  if (any(abs(lat) > 90, na.rm = TRUE)) {
    warning("Latitude values outside the valid range (-90 to 90) were detected.")
  }

  # Validate distance_type
  valid_types <- c("haversine", "euclidean", "grid")
  if (!distance_type %in% valid_types) {
    stop("The 'distance_type' parameter must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Validate unit
  known_units <- c("nm", "km", "m", "mi")
  if (!unit %in% known_units) {
    stop("The unit '", unit, "' is not one of the common units: ", paste(known_units, collapse = ", "))
  }

  # Check coastline structure
  if (!all(c("Long", "Lat") %in% colnames(coastline))) {
    stop("coastline must contain columns 'Long' and 'Lat'")
  }

  # Check that coastline has data
  if (nrow(coastline) == 0) {
    stop("coastline is empty")
  }

  # Check that coastline has numeric coordinates
  if (!is.numeric(coastline$Long) || !is.numeric(coastline$Lat)) {
    stop("The 'Long' and 'Lat' columns of coastline must be numeric")
  }

  # Handle NA values
  valid <- !is.na(lon) & !is.na(lat)
  if (sum(valid) == 0) {
    warning("All input points contain NA values")
    return(rep(NA, length(valid)))
  }

  valid_lon <- lon[valid]
  valid_lat <- lat[valid]

  # Divide points into batches
  n_points <- length(valid_lon)
  cores <- min(cores, n_points)  # Adjust cores if there are fewer points than cores
  batch_size <- ceiling(n_points / cores)
  batch_indices <- split(seq_len(n_points), ceiling(seq_len(n_points) / batch_size))

  # Load future.apply library if parallel is to be used
  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Package 'future.apply' not found. Using sequential processing.")
      parallel <- FALSE
    } else {
      future::plan(future::multisession, workers = cores)
      on.exit(future::plan(future::sequential), add = TRUE)  # Ensure sequential plan is restored
    }
  }

  # Execute in parallel or sequentially
  apply_fun <- if (parallel) future.apply::future_lapply else lapply

  # Calculation function wrapped in tryCatch to handle errors
  batch_results <- tryCatch({
    apply_fun(batch_indices, function(batch_indices) {
      calculate_distances_vectorized(
        lon_point = valid_lon[batch_indices],
        lat_point = valid_lat[batch_indices],
        coast_lon = coastline$Long,
        coast_lat = coastline$Lat,
        distance_type = distance_type,
        window = window,
        unit = unit
      )
    })
  }, error = function(e) {
    stop("Error in distance calculation: ", e$message)
  })

  # Combine results
  tryCatch({
    distances <- unlist(lapply(batch_results, `[[`, "distances"))
    indices <- unlist(lapply(batch_results, `[[`, "indices"))

    # Check results
    if (length(distances) != sum(valid)) {
      warning("The number of calculated distances does not match the number of valid points.")
    }

    # Results with NAs in original positions
    final_result <- rep(NA_real_, length(valid))
    final_result[valid] <- distances

    if (return_indices) {
      final_indices <- rep(NA_integer_, length(valid))
      final_indices[valid] <- indices
      return(list(distance = final_result, index = final_indices))
    } else {
      return(final_result)
    }
  }, error = function(e) {
    warning("Error processing results: ", e$message)
    return(rep(NA_real_, length(valid)))
  })
}



#' Points on land
#'
#' @description
#' This function classifies a set of geographic coordinates (longitude and latitude) as "land" or "sea" according to their relative position to a coastline. A point is considered to be on land if its longitude is greater than that of its nearest point on the coastline. Additionally, it allows parallel computation to improve performance with large volumes of data.
#'
#' @param x_point Numeric vector of longitudes (in decimal degrees).
#' @param y_point Numeric vector of latitudes (in decimal degrees).
#' @param coastline `data.frame` with coastline coordinates. Must have columns named `Long` and `Lat`. If `NULL` (default), uses internal dataset `Tivy::peru_coastline`.
#' @param parallel Logical. If `TRUE`, performs the calculation in parallel using multiple cores. Default is `FALSE`.
#' @param cores Number of cores to use for parallel processing. Default is `4`.
#' @param distance_type Type of geodesic distance to use in the calculation: `"haversine"` (default) or others if the internal function allows it.
#' @param window Geographic window in degrees to reduce the number of coastline points to consider for each point. Default is `0.5`.
#' @param unit Unit of measurement for distance: `"km"` (default) or another if the internal function allows it.
#'
#' @return A text vector of the same length as `x_point`, indicating whether each point is on `"land"` or `"sea"`. `NA` values are maintained as `NA`.
#'
#' @details
#' This function internally uses `calculate_distances_vectorized()` to identify the nearest point on the coastline for each coordinate. If `parallel = TRUE`, it uses the `future` and `future.apply` packages to distribute the work among multiple cores.
#'
#' @examples
#'
#' data(calas_bitacora)
#'
#' data_hauls <- process_hauls(data_hauls = calas_bitacora)
#'
#' result <- land_points(
#'   x_point = data_hauls$lon_final,
#'   y_point = data_hauls$lat_final,
#'   coastline = NULL
#' )
#'
#' table(result)
#'
#' @export
land_points <- function(x_point,
                          y_point,
                          coastline = NULL,
                          parallel = FALSE,
                          cores = 4,
                          distance_type = "haversine",
                          window = 0.5,
                          unit = "nm") {
  # --- Validations ---
  if (!is.numeric(x_point) || !is.numeric(y_point)) {
    stop("`x_point` and `y_point` must be numeric vectors.")
  }

  if (length(x_point) != length(y_point)) {
    stop("`x_point` and `y_point` must have the same length.")
  }

    # Load default coastline if NULL
  if (is.null(coastline)) {
    if (!requireNamespace("Tivy", quietly = TRUE)) {
      stop("Default coastline data (Tivy::peru_coastline) is not available. Please provide a coastline.")
    }
    coastline <- Tivy::peru_coastline
  }


  if (!is.data.frame(coastline)) {
    stop("`coastline` must be a data.frame.")
  }

  if (!all(c("Long", "Lat") %in% names(coastline))) {
    stop("`coastline` must contain columns named 'Long' and 'Lat'.")
  }

  if (!is.logical(parallel)) {
    stop("`parallel` must be TRUE or FALSE.")
  }

  if (!is.numeric(cores) || cores < 1) {
    stop("`cores` must be a positive integer.")
  }

  if (!distance_type %in% c("haversine", "manhattan", "grid")) {
    stop("`distance_type` must be one of: 'haversine', 'manhattan', 'grid'.")
  }

  if (!is.numeric(window) || window < 0) {
    stop("`window` must be a non-negative number.")
  }

  if (!unit %in% c("km", "nm")) {
    stop("`unit` must be 'km' or 'nm'.")
  }

  # Convert factors or strings to numeric if necessary
  x_point <- as.numeric(x_point)
  y_point <- as.numeric(y_point)
  coastline$Long <- as.numeric(coastline$Long)
  coastline$Lat <- as.numeric(coastline$Lat)

  # Remove NA coordinates if they exist (keep NAs in output)
  n <- length(x_point)

  process_point <- function(i) {
    if (is.na(x_point[i]) || is.na(y_point[i])) return(NA_character_)

    result <- calculate_distances_vectorized(
      lon_point = x_point[i],
      lat_point = y_point[i],
      coast_lon = coastline$Long,
      coast_lat = coastline$Lat,
      distance_type = distance_type,
      window = window,
      unit = unit
    )

    coast_idx <- result$indices[1]

    if (coast_idx >= 2 && coast_idx < nrow(coastline)) {
      p1 <- c(coastline$Long[coast_idx - 1], coastline$Lat[coast_idx - 1])
      p2 <- c(coastline$Long[coast_idx + 1], coastline$Lat[coast_idx + 1])
      p  <- c(x_point[i], y_point[i])

      direction <- (p2[1] - p1[1]) * (p[2] - p1[2]) - (p2[2] - p1[2]) * (p[1] - p1[1])

      if (direction > 0) return("sea") else return("land")
    }

    return("unknown")
  }

  # Execute in parallel or not
  if (parallel) {
    future::plan(future::multisession, workers = cores)
    result <- future.apply::future_lapply(seq_len(n), process_point)
    future::plan(future::sequential)
  } else {
    result <- lapply(seq_len(n), process_point)
  }

  return(unlist(result))
}
